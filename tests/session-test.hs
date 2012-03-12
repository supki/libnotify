#!/usr/bin/env runhaskell

import System.Libnotify
import System.Libnotify.Types
import Control.Concurrent (threadDelay)

main :: IO ()
main = withNotifications Nothing $
         do let title = "Some title"
                body = "some text"
                icon = "dialog-information"
            s <- new title body icon render
            threadDelay 1000000
            continue title body icon s $
              do update Nothing (Just "another text") (Just "dialog-question")
                 render
