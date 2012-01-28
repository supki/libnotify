#!/usr/bin/env runhaskell

import System.Libnotify

main :: IO ()
main = withNotifications Nothing $
         do new "Same title" (Just "line 1") Nothing $ do addHint ("append","allowed")
                                                          removeHints
                                                          render
            new "Same title" (Just "line 2") Nothing $ do addHint ("append","allowed")
                                                          removeHints
                                                          render
