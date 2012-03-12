#!/usr/bin/env runhaskell

import System.Libnotify

main :: IO ()
main = withNotifications Nothing $
         do new "Same title" "line 1" "" $
              do addHint (HintString "append" "allowed")
                 removeHints
                 render
            new "Same title" "line 2" "" $
              do addHint (HintString "append" "allowed")
                 removeHints
                 render
