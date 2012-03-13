#!/usr/bin/env runhaskell

import Control.Monad (void)
import System.Libnotify (oneShot)

main :: IO ()
main = void $ oneShot "Title" "Body" "dialog-information" Nothing
