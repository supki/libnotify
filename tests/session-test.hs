#!/usr/bin/env runhaskell

import System.Libnotify
import System.Libnotify.Types
import Control.Concurrent (threadDelay)

main :: IO ()
main = withNotifications Nothing $
         do n <- new "Some title" (Just "some text") (Just "dialog-information")
              render
            threadDelay 10000000
            continue n $
              do update "Another title" (Just "another text") (Just "dialog-question")
                 render
