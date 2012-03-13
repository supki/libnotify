#!/usr/bin/env runhaskell

import Control.Monad (void)
import System.Libnotify
import System.Libnotify.Server

main :: IO ()
main = void $ withNotifications (Just "first-name") $
         do before <- getAppName
            putStrLn $ "Before: " ++ before
            setAppName "second-name"
            after <- getAppName
            putStrLn $ "After: " ++ after
            setAppName "third-name"
            afterAfter <- getAppName
            putStrLn $ "After after: " ++ afterAfter
