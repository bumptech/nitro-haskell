{-# LANGUAGE OverloadedStrings #-}
import Nitro
import Control.Monad
import Control.Concurrent

-- The Black Knight
main = do
    nitroRuntimeStart

    c <- socket
    connect "tcp://127.0.0.1:7724" c

    inp <- socket
    bind "tcp://127.0.0.1:7724" inp

    outp <- socket
    connect "tcp://127.0.0.1:7723" outp

    b <- socket
    bind "tcp://127.0.0.1:7723" b

    --client
    send c "Look, you stupid Bastard. You've got no arms left."  []

    --proxy
    (_, fr) <- recvFrame inp []
    relayFw outp fr []

    --server
    (_, fr) <- recvFrame b []
    reply b fr []

    --proxy
    (_, fr) <- recvFrame outp []
    relayBk inp fr []

    --client
    msg <- recv c []
    print msg




