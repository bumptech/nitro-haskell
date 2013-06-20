{-# LANGUAGE OverloadedStrings #-}
import System.Nitro
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)

-- The Contender
main = do
    nitroRuntimeStart

    b <- bind "tcp://127.0.0.1:7723" defaultOpts
    c <- connect "tcp://127.0.0.1:7723" defaultOpts

    sub c "con"
    threadDelay 1000000

    fr <- bstrToFrame "You don't understand!"
    pub b fr "contender" []
    print =<< frameToBstr =<< recv c []

    fr <- bstrToFrame "I coulda had class."
    pub b fr "contender" []
    print =<< frameToBstr =<< recv c []

    fr <- bstrToFrame "I coulda been a contender."
    pub b fr "contender" []
    print =<< frameToBstr =<< recv c []

    fr <- bstrToFrame "I could've been someone."
    pub b fr  "contender" []
    print =<< frameToBstr =<< recv c []
