#!/usr/bin/env runhaskell

import System.Libnotify (oneShot)

main :: IO ()
main = oneShot "Title" "Body" "dialog-information" Nothing
