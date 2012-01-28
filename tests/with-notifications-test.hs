#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import System.Libnotify

main :: IO ()
main = tryWithout >> tryWith >> return ()
  where tryWith :: IO ()
        tryWithout = handle (\(e :: SomeException) -> putStrLn "tryWithout: Initialization failure exception has caught.") $
                       new "Title" (Just "Without") (Just "dialog-information") render >> return ()
        tryWith    = withNotifications Nothing $
                       new "Title" (Just "With") (Just "dialog-information") render


