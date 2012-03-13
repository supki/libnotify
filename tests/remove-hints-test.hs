#!/usr/bin/env runhaskell

import Control.Monad (void)
import System.Libnotify

main :: IO ()
main = void $ withNotifications Nothing $
         do new "Same title" "line 1" "" $
              do addHint (HintString "append" "allowed")
                 removeHints
                 render
            new "Same title" "line 2" "" $
              do addHint (HintString "append" "allowed")
                 removeHints
                 render
