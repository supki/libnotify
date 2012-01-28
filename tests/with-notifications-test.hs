#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import System.Libnotify

main :: IO ()
main = tryWithout >> tryWith >> return ()
  where tryWithout = handle notifyErrorHandler $
                       new "Title" "Without" "dialog-information" render >> return ()
        tryWith = withNotifications Nothing $
                    new "Title" "With" "dialog-information" render


