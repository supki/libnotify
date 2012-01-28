#!/usr/bin/env runhaskell

import System.Libnotify (oneShot, GeneralHint)

main :: IO ()
main = oneShot "Title" "Body" "dialog-information" ([] :: [GeneralHint])
