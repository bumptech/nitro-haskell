{-# LANGUAGE OverloadedStrings #-}
import Nitro
import Control.Monad
import Control.Concurrent

-- Sir Robin
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
    send c "Brave Sir Robin ran away..."  []

    --proxy relay
    (_, fr1) <- recvFrame inp []
    relayFw outp fr1 []

    --server
    msg <- recv b []
    print msg
    send b "No!" []

    --proxy relay
    (_, fr2) <- recvFrame outp []
    relayFw inp fr2 []

    --client
    back <- recv c []
    print back




