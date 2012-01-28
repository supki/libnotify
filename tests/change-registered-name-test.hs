#!/usr/bin/env runhaskell

import System.Libnotify
import System.Libnotify.Server

main :: IO ()
main = withNotifications (Just "first-name") $
       do before <- getAppName
          putStrLn $ "Before: " ++ before
          setAppName "second-name"
          after <- getAppName
          putStrLn $ "After: " ++ after
          setAppName "third-name"
          afterAfter <- getAppName
          putStrLn $ "After after: " ++ afterAfter
