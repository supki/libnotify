#!/usr/bin/env runhaskell

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import System.Libnotify

main :: IO ()
main = void $ withNotifications Nothing $
         do new "Default" "some text" "dialog-information" $
              do setTimeout Default
                 render
            threadDelay 2000000
            new "Infinite" "some text" "dialog-question" $
              do setTimeout Infinite
                 render
            threadDelay 2000000
            putStrLn "Done."
