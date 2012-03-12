#!/usr/bin/env runhaskell

import Control.Concurrent (threadDelay)
import Data.Int (Int32)
import System.Libnotify

main :: IO ()
main = withNotifications Nothing $ (homogeneous >> geterogeneous)

homogeneous = do new "Same title" "line 1" "" $ do addHint (HintString "append" "allowed")
                                                   render
                 new "Same title" "line 2" "" $ do addHint (HintString "append" "allowed")
                                                   render
                 new "Same title" "line 3" "" $ do addHint (HintString "append" "allowed")
                                                   render

geterogeneous = do new "Bar" "" "dialog-information" $
                     do mapM_ addHint
                          [ HintInt "value" 33
                          , HintString "synchronous" "volume"
                          ]
                        render
                   threadDelay timeout
                   new "Bar" "" "dialog-information" $
                     do mapM_ addHint
                          [ HintInt "value" 66
                          , HintString "synchronous" "volume"
                          ]
                        render
                   threadDelay timeout
                   new "Bar" "" "dialog-information" $
                     do mapM_ addHint
                          [ HintInt "value" 100
                          , HintString "synchronous" "volume"
                          ]
                        render

timeout = 500000
