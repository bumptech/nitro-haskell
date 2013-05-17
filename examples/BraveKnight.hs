{-# LANGUAGE OverloadedStrings #-}
import Nitro

-- The Brave Knight
main = do
    nitroRuntimeStart

    bound <- socket
    bind "tcp://127.0.0.1:7723" bound

    connected <- socket
    connect "tcp://127.0.0.1:7723" connected

    send connected "What is your name?" []

    quest1 <- recv bound []
    print quest1
    send bound "Sir Lancelot of Camelot" []

    answer1 <- recv connected []
    print answer1
    send connected "What is your quest?" []

    quest2 <- recv bound []
    print quest2
    send bound "To seek the Holy Grail!" []

    answer2 <- recv connected []
    print answer2
    send connected "What is your favorite colour?" []

    quest3 <- recv bound []
    print quest3
    send bound "Red!...I mean blue![ahhhhhhh!]" []

    answer3 <- recv connected []
    print answer3
