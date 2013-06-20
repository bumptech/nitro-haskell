{-# LANGUAGE OverloadedStrings #-}
import System.Nitro

-- The Brave Knight
main = do
    nitroRuntimeStart

    bound <- bind "tcp://127.0.0.1:7723" defaultOpts
    connected <- connect "tcp://127.0.0.1:7723" defaultOpts

    fr <- bstrToFrame "What is your name?"
    send connected fr []

    print =<< frameToBstr =<< recv bound []
    fr <- bstrToFrame "Sir Lancelot of Camelot"
    send bound fr []

    print =<< frameToBstr =<< recv connected []
    fr <- bstrToFrame "What is your quest?"
    send connected fr []

    print =<< frameToBstr =<< recv bound []
    fr <- bstrToFrame "To seek the Holy Grail!"
    send bound fr []

    print =<< frameToBstr =<< recv connected []
    fr <- bstrToFrame "What is your favorite colour?"
    send connected fr []

    print =<< frameToBstr =<< recv bound []
    fr <- bstrToFrame "Red!...I mean blue![ahhhhhhh!]"
    send bound fr []

    print =<< frameToBstr =<< recv connected []
