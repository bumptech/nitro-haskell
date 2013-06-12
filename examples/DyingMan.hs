{-# LANGUAGE OverloadedStrings #-}
import System.Nitro
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay)
import System.Posix.Types (Fd(..))
import GHC.Event as Event

-- The Async Dying Man
callback sock evtManager _ _ = do
    bstr <- recv sock [NoWait]
    print bstr
    Event.shutdown evtManager

main = do
    nitroRuntimeStart

    bound <- bind "tcp://127.0.0.1:7723" $ defaultOpts { wantFd = True }

    connected <- connect "tcp://127.0.0.1:7723" defaultOpts
    send connected "Open the pod bay doors, HAL." []

    fdnum <- fileno bound
    let fd = Fd (fromIntegral fdnum)

    evtManager <- Event.new
    _ <- Event.registerFd evtManager (callback bound evtManager) fd Event.evtRead

    Event.loop evtManager
