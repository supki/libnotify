#!/usr/bin/env runhaskell

import Control.Concurrent (threadDelay)
import Data.Int (Int32)
import System.Libnotify

main :: IO ()
main = withNotifications Nothing $ (homogeneous >> geterogeneous)

homogeneous = do new "Same title" "line 1" "" $ do addHint ("append","allowed")
                                                   render
                 new "Same title" "line 2" "" $ do addHint ("append","allowed")
                                                   render
                 new "Same title" "line 3" "" $ do addHint ("append","allowed")
                                                   render

geterogeneous = do new "Bar" "" "dialog-information" $
                     do mapM_ addHint
                          [ generalize ("value", 33 :: Int32)
                          , generalize ("synchronous", "volume")
                          ]
                        render
                   threadDelay timeout
                   new "Bar" "" "dialog-information" $
                     do mapM_ addHint
                          [ generalize ("value", 66 :: Int32)
                          , generalize ("synchronous", "volume")
                          ]
                        render
                   threadDelay timeout
                   new "Bar" "" "dialog-information" $
                     do mapM_ addHint
                          [ generalize ("value", 100 :: Int32)
                          , generalize ("synchronous", "volume")
                          ]
                        render

timeout = 500000
