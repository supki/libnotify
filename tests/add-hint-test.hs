#!/usr/bin/env runhaskell

import Control.Concurrent (threadDelay)
import Data.Int (Int32)
import System.Libnotify

main :: IO ()
main = withNotifications Nothing $ (homogeneous >> geterogeneous)

homogeneous = do new "Same title" (Just "line 1") Nothing $ do addHint ("append","allowed")
                                                               render
                 threadDelay timeout
                 new "Same title" (Just "line 2") Nothing $ do addHint ("append","allowed")
                                                               render
                 threadDelay timeout
                 new "Same title" (Just "line 3") Nothing $ do addHint ("append","allowed")
                                                               render

geterogeneous = do new "Bar" Nothing (Just "dialog-information") $
                     do mapM_ addHint
                          [ generalize ("value", 33 :: Int32)
                          , generalize ("synchronous", "volume")
                          ]
                        render
                   threadDelay timeout
                   new "Bar" Nothing (Just "dialog-information") $
                     do mapM_ addHint
                          [ generalize ("value", 66 :: Int32)
                          , generalize ("synchronous", "volume")
                          ]
                        render
                   threadDelay timeout
                   new "Bar" Nothing (Just "dialog-information") $
                     do mapM_ addHint
                          [ generalize ("value", 100 :: Int32)
                          , generalize ("synchronous", "volume")
                          ]
                        render

timeout = 500000
