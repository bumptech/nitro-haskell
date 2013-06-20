{-# LANGUAGE OverloadedStrings #-}
import System.Nitro
import Control.Monad
import Control.Concurrent
import Data.ByteString as BS

-- The Black Knight
main = do
    nitroRuntimeStart

    c <- connect "tcp://127.0.0.1:7724" defaultOpts
    inp <- bind "tcp://127.0.0.1:7724" defaultOpts
    outp <- connect "tcp://127.0.0.1:7723" defaultOpts
    b <- bind "tcp://127.0.0.1:7723" defaultOpts

    --client
    fr <- bstrToFrame "Look, you stupid Bastard. You've got no arms left."
    send c fr []

    --proxy
    fr <- recv inp []
    proxied <- bstrToFrame . BS.append "Yeah! " =<< frameToBstr fr
    relayFw outp fr proxied []

    --server
    fr <- recv b []
    print =<< frameToBstr fr
    r <- bstrToFrame "Yes I have."
    reply b fr r []

    --proxy
    fr <- recv outp []
    proxied <- bstrToFrame .  BS.append "(hint: he's lying) " =<< frameToBstr fr
    relayBk inp fr proxied []

    --client
    fr <- recv c []
    print =<< frameToBstr fr




