#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import System.Libnotify

main :: IO ()
main = tryWithout >> tryWith >> return ()
  where tryWithout = do r <- new "Title" "Without" "dialog-information" render
                        case r of
                          Left e -> print e
                          Right _ -> putStrLn "Everything went fine without withNotifications!"
        tryWith = withNotifications Nothing $ do
                    r <- new "Title" "With" "dialog-information" render
                    case r of
                      Left e -> print e
                      Right _ -> putStrLn "Everything went fine with withNotifications!"


