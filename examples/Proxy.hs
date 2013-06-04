{-# LANGUAGE OverloadedStrings #-}
import Nitro
import Control.Monad
import Control.Concurrent
import Data.ByteString as BS

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
    (msg, fr) <- recvFrame inp []
    proxied <- bstrToFrame $ BS.append "Yeah! " msg
    relayFw outp fr proxied []

    --server
    (msg, fr) <- recvFrame b []
    print msg
    r <- bstrToFrame "Yes I have."
    reply b fr r []

    --proxy
    (msg, fr) <- recvFrame outp []
    proxied <- bstrToFrame $ BS.append msg " (hint: he's lying)"
    relayBk inp fr proxied []

    --client
    msg <- recv c []
    print msg




