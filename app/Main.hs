module Main where

import Network.Socket
import Network.BSD
import System.IO
import System.Environment
import System.Directory
import Control.Concurrent.ParallelIO.Local
import Control.Exception
import Control.Monad.Fix (fix)
import Data.List
import Data.IP
import Data.List.Split

main :: IO ()
main = do
    [port] <- getArgs
    sock <- socket AF_INET Stream 0                            -- create socket
    setSocketOption sock ReuseAddr 1                           -- make socket immediately reusable.
    bind sock (SockAddrInet (toEnum $ read port) iNADDR_ANY)   -- listen on TCP port given by user.
    let nbThreads = 5
    listen sock (nbThreads*2)                                  -- queue of 10 connections max
    withPool nbThreads $ 
        \pool -> parallel_ pool (replicate nbThreads (mainLoop sock port))
    putStrLn "Server killed. See you !"
    
mainLoop :: Socket -> String -> IO ()
mainLoop sock port = do
    let killed = False
    putStrLn "Waiting for incoming connection..."
    conn <- try (accept sock) :: IO (Either SomeException (Socket, SockAddr))  -- try to accept a connection and handle it
    case conn of
        Left  _    -> putStrLn "Socket is now closed. Exiting."
        Right conn -> do
            putStrLn "Got a client !"
            killed <- runConn conn sock port                   -- run our server's logic, then
            if killed then return () else mainLoop sock port   -- repeat

runConn :: (Socket, SockAddr) -> Socket -> String -> IO Bool
runConn (sock, addr) originalSocket port = do
    let killed = False
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl LineBuffering
    killed <- handle (\(SomeException _) -> return False) $ fix $ \loop -> do
        line <- hGetLine hdl
        let commandAndArgs = splitOn " " line
        let command = head commandAndArgs
        let args = intercalate " " $ tail commandAndArgs 
        case command of
            "KILL_SERVICE" -> do
                killService originalSocket
                return True
            "HELO"         -> helo hdl addr args port >> loop
            _              -> otherCommand hdl line   >> loop
    hClose hdl
    return killed

sendResponse :: Handle -> String -> IO ()
sendResponse hdl resp = do
    hSetBuffering hdl $ BlockBuffering $ Just (length resp)
    hPutStrLn hdl resp

getHostNameIfDockerOrNot :: IO String
getHostNameIfDockerOrNot = do
    weAreInDocker <- doesFileExist "/.dockerenv"
    host <- if weAreInDocker 
        then getHostByName "dockerhost" 
        else (getHostName >>= getHostByName)
    return $ show $ fromHostAddress $ head $ hostAddresses host

killService :: Socket -> IO ()
killService originalSocket = do
    putStrLn "Killing Service..."
    close originalSocket

helo :: Handle -> SockAddr -> String -> String -> IO ()
helo hdl addr text port = do
    putStrLn $ "Responding to HELO command with params : " ++ text
    hostname <- getHostNameIfDockerOrNot
    sendResponse hdl $ "HELO " ++ text ++ "\nIP:" ++ hostname ++ "\nPort:" ++ port ++ "\nStudentID:16336620"

otherCommand :: Handle -> String -> IO ()
otherCommand hdl param = do
    putStrLn $ "Received unknown query : " ++ param
    sendResponse hdl $ "Command not implemented yet : " ++ param ++ "\nStay tuned !"
