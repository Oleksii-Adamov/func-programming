module Main where
import qualified Graphics.UI.Threepenny as UI
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket.ByteString as NSB
import Graphics.UI.Threepenny.Core
import Network.Socket
import Control.Concurrent
import Control.Concurrent.STM
import System.IO

setupConnection :: String -> String -> (Socket -> IO ()) -> IO ()
setupConnection host port action = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    action sock
    close sock

sendMsg :: Socket -> String -> IO ()
sendMsg sock msg = do
    putStrLn $ "Sending message: " ++ msg
    _ <- NSB.send sock (C.pack msg)  -- Discard the return value
    return ()

setup :: Socket -> Window -> UI ()
setup sock window = do
    return window # set title "Haskell Chat"

    -- UI elements
    input <- UI.input
    sendButton <- UI.button #+ [string "Send"]
    output <- UI.div

    -- Layout
    getBody window #+ [element input, element sendButton, element output]

    -- Event handling
    on UI.click sendButton $ \_ -> do
        message <- get value input
        liftIO $ sendMsg sock message
        newMsg <- UI.div #+ [string ("You: " ++ message)]
        element output #+ [element newMsg]
        element input # set value ""

main :: IO ()
main = do
    let host = "localhost"
    let port = "3000"
    setupConnection host port $ \sock ->
        startGUI defaultConfig (setup sock)