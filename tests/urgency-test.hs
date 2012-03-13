#!/usr/bin/env runhaskell

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import System.Libnotify

main :: IO ()
main = void $ withNotifications Nothing $
         do new "Some title" "some text" "dialog-information" $
              do setUrgency Low
                 render
            threadDelay 2000000
            new "Some title" "some text" "dialog-question" $
              do setUrgency Normal
                 render
            threadDelay 2000000
            new "Another title" "another text" "dialog-question" $
              do setUrgency Critical
                 render
            putStrLn "Done."
