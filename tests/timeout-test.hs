#!/usr/bin/env runhaskell

import System.Libnotify
import System.Libnotify.Types
import Control.Concurrent (threadDelay)

main :: IO ()
main = withNotifications Nothing $
         do _ <- new "Default" "some text" "dialog-information" $
              do setTimeout Default
                 render
            threadDelay 2000000
            _ <- new "Infinite" "some text" "dialog-question" $
              do setTimeout Infinite
                 render
            threadDelay 2000000
            putStrLn "Done."
