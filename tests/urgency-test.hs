#!/usr/bin/env runhaskell

import System.Libnotify
import System.Libnotify.Types
import Control.Concurrent (threadDelay)

main :: IO ()
main = withNotifications Nothing $
         do _ <- new "Some title" "some text" "dialog-information" $
              do setUrgency Low
                 render
            threadDelay 2000000
            _ <- new "Some title" "some text" "dialog-question" $
              do setUrgency Normal
                 render
            threadDelay 2000000
            _ <- new "Another title" "another text" "dialog-question" $
              do setUrgency Critical
                 render
            putStrLn "Done."
