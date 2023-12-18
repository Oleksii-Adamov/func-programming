module Main where
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket.ByteString as NSB
import Graphics.UI.Threepenny.Core
import Network.Socket
import Control.Concurrent (forkIO)
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Control.Monad (forever, unless, forM_)
import Control.Exception (finally, bracket, try, catch, handle, IOException)

startServer :: IO ()
startServer = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 1
    putStrLn "Listening on port 3000"

    clients <- atomically $ newTVar []  -- TVar for managing clients
    forever $ do
        (conn, _) <- accept sock
        forkIO $ runConn conn clients

runConn :: Socket -> ClientList -> IO ()
runConn conn clients = do
    chan <- atomically newTChan
    let client = (conn, chan)
    atomically $ modifyTVar' clients (client :)
    clientHandler conn clients

clientHandler :: Socket -> ClientList -> IO ()
clientHandler conn clients = handleClient conn `catch` clientDisconnected
  where
    handleClient sock = do
        msg <- NSB.recv sock 1024
        if C.null msg
            then removeClient sock clients  -- Client disconnected, remove it
            else do
                let receivedMsg = C.unpack msg
                putStrLn $ "Received message: " ++ receivedMsg
                broadcast receivedMsg clients
                handleClient sock

    clientDisconnected :: IOException -> IO ()
    clientDisconnected e = do
        putStrLn $ "Client disconnected abruptly: " ++ show e
        removeClient conn clients

removeClient :: Socket -> ClientList -> IO ()
removeClient conn clients = do
    putStrLn "Removing client"
    atomically $ modifyTVar' clients $ filter ((/= conn) . fst)
    close conn  -- Close the socket

broadcast :: String -> ClientList -> IO ()
broadcast msg clients = do
    clist <- atomically $ readTVar clients
    forM_ clist $ \(sock, _) -> try (NSB.send sock (C.pack msg)) >>= handleResult sock
  where
    handleResult :: Socket -> Either IOException Int -> IO ()
    handleResult sock (Left e) = do
        putStrLn $ "Error sending to client: " ++ show sock ++ ", error: " ++ show e
        removeClient sock clients
    handleResult _ (Right _) = return ()  -- Successfully sent, do nothing

main :: IO ()
main = startServer