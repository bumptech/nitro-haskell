{-# LANGUAGE OverloadedStrings #-}
import Nitro
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)

-- The Contender
main = do
    nitroRuntimeStart

    b <- socket
    bind "tcp://127.0.0.1:7723" b

    c <- socket
    connect "tcp://127.0.0.1:7723" c

    sub c "con"
    threadDelay 1000000

    pub b "You don't understand!" "contender"
    bstr <- recv c []
    print bstr

    pub b "I coulda had class." "contender"
    bstr <- recv c []
    print bstr

    pub b "I coulda been a contender." "contender"
    bstr <- recv c []
    print bstr

    pub b "I could've been someone."  "contender"
    bstr <- recv c []
    print bstr
