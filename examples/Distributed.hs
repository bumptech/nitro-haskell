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

rpc = withSocket (connect "tcp://127.0.0.1:7723" defaultOpts)
      (\client -> do
          send client "Whoa there" []
          back <- recv client []
          print back
      )

main = do
    nitroRuntimeStart

    bound <- bind "tcp://*:7723" defaultOpts

    mapM_ (\i -> void $ forkIO $ serverThread bound i) [1..5]

    client <- connect "tcp://127.0.0.1:7723" defaultOpts

    mapM_ (\_ -> rpc) [1..5]

    close bound
    close client


