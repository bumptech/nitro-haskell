{-# LANGUAGE OverloadedStrings #-}
import System.Nitro
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Concurrent.MVar

-- Threaded
-- remember -> ghc-options: -threaded
main = do
    nitroRuntimeStart

    done1 <- newEmptyMVar
    done2 <- newEmptyMVar

    forkIO $ peasant1 done1
    forkIO $ peasant2 done2

    void $ takeMVar done1
    void $ takeMVar done2

peasant1 done = do
    bound <- bind "tcp://127.0.0.1:7723" defaultOpts

    print =<< frameToBstr =<< recv bound []

    fr <- bstrToFrame "I dunno, must be a king"
    send bound fr []

    print =<< frameToBstr =<< recv bound []

    fr <- bstrToFrame "He hasn't got shit all over him."
    send bound fr []

    putMVar done "done"

peasant2 done = do
    connected <- connect "tcp://127.0.0.1:7723" defaultOpts

    fr <- bstrToFrame "Who's that then?"
    send connected fr []

    print =<< frameToBstr =<< recv connected []

    fr <- bstrToFrame "Why?"
    send connected fr []

    print =<< frameToBstr =<< recv connected []

    putMVar done "done"
