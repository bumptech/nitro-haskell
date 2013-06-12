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

    quest1 <- recv bound []
    print quest1

    send bound "I dunno, must be a king" []

    quest2 <- recv bound []
    print quest2

    send bound "He hasn't got shit all over him." []

    putMVar done "done"

peasant2 done = do
    connected <- connect "tcp://127.0.0.1:7723" defaultOpts

    send connected "Who's that then?" []

    answer1 <- recv connected []
    print answer1

    send connected "Why?" []

    answer2 <- recv connected []
    print answer2

    putMVar done "done"
