{-# LANGUAGE OverloadedStrings #-}
import System.Nitro
import Control.Monad (void, forever)
import Control.Concurrent (forkIO, threadDelay)
import Data.ByteString.Char8 as BS

serverThread s i = forever $ do
    (_,fr) <- recvFrame s []
    threadDelay 1000000
    msgBack <- bstrToFrame (BS.pack $ "Hi it's thread #" ++ (show i))
    reply s fr msgBack []

rpc client = do
    send client "Whoa there" []
    back <- recv client []
    print back

main = do
    nitroRuntimeStart

    bound <- socket
    bind "tcp://*:7723" bound

    mapM_ (\i -> void $ forkIO $ serverThread bound i) [1..5]

    client <- socket
    connect "tcp://127.0.0.1:7723" client

    mapM_ (\_ -> rpc client) [1..5]

    close bound
    close client


