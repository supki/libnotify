#!/usr/bin/env runhaskell

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import System.Libnotify

main :: IO ()
main = void $ withNotifications Nothing $
         do let title = "Some title"
                body = "some text"
                icon = "dialog-information"
            Right s <- new title body icon render
            threadDelay 1000000
            continue s $
              do update Nothing (Just "another text") (Just "dialog-question")
                 render
