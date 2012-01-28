#!/usr/bin/env runhaskell

import System.Libnotify
import System.Libnotify.Types
import Control.Concurrent (threadDelay)

main :: IO ()
main = withNotifications Nothing $
         do s <- new "Some title" "some text" "dialog-information" render
            threadDelay 1000000
            continue s $
              do update Nothing (Just "another text") (Just "dialog-question")
                 render
