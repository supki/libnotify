#!/usr/bin/env runhaskell

import System.Libnotify (oneShot)

main :: IO ()
main = oneShot "Title" (Just "Body") (Just "dialog-information") ([] :: [(String,String)])
