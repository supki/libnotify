#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import System.Libnotify

main :: IO ()
main = tryWithout >> tryWith >> return ()
  where tryWithout = handle notifyErrorHandler $
                       new "Title" (Just "Without") (Just "dialog-information") render >> return ()
        tryWith = withNotifications Nothing $
                    new "Title" (Just "With") (Just "dialog-information") render


