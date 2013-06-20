{-# LANGUAGE OverloadedStrings #-}
import System.Nitro
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay)
import System.Posix.Types (Fd(..))
import GHC.Event as Event

-- The Async Dying Man
callback sock evtManager _ _ = do
    print =<< frameToBstr =<< recv sock [NoWait]
    Event.shutdown evtManager

main = do
    nitroRuntimeStart

    bound <- bind "tcp://127.0.0.1:7723" $ defaultOpts { wantFd = True }

    connected <- connect "tcp://127.0.0.1:7723" defaultOpts
    fr <- bstrToFrame "Open the pod bay doors, HAL."
    send connected fr []

    fdnum <- fileno bound
    let fd = Fd (fromIntegral fdnum)

    evtManager <- Event.new
    _ <- Event.registerFd evtManager (callback bound evtManager) fd Event.evtRead

    Event.loop evtManager
