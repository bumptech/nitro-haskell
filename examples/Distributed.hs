{-# LANGUAGE OverloadedStrings #-}
import System.Nitro
import Control.Monad (void, forever)
import Control.Concurrent (forkIO, threadDelay)
import Data.ByteString.Char8 as BS

serverThread s i = forever $ do
    fr <- recv s []
    threadDelay 1000000
    frBack <- bstrToFrame (BS.pack $ "Hi it's thread #" ++ (show i))
    reply s fr frBack []

rpc = withSocket (connect "tcp://127.0.0.1:7723" defaultOpts)
      (\client -> do
          fr <- bstrToFrame "Whoa there"
          send client fr []
          print =<< frameToBstr =<< recv client []
      )

main = do
    nitroRuntimeStart

    bound <- bind "tcp://*:7723" defaultOpts

    mapM_ (\i -> void $ forkIO $ serverThread bound i) [1..2]

    client <- connect "tcp://127.0.0.1:7723" defaultOpts

    mapM_ (\_ -> rpc) [1..5]

    close bound
    close client


