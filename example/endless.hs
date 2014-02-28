#!/usr/bin/env runhaskell

-- | This little example program shows how to use libnotify action
-- callback together with glib MainLoop thing so they actually work

import Control.Applicative ((<$))
import Control.Concurrent (threadDelay)
import Libnotify
import System.Glib.MainLoop (MainLoop, mainLoopNew, mainLoopRun, mainLoopQuit)

main :: IO ()
main = () <$ do
  l <- mainLoopNew Nothing False
  display $
       summary "Hello!"
    <> body "Please, say \"blop\"!"
    <> icon "face-embarrassed"
    <> timeout Infinite
    <> action "blob" "Say \"blop\"" blopCallback
    <> action "flop" "Say \"flop\"" (flopCallback l)
  mainLoopRun l

blopCallback :: Notification -> t -> IO Notification
blopCallback n _ = do
  close n
  putStrLn "Thanks!"
  threadDelay second
  display (base n <> summary "" <> body "Pretty please, say \"blop\"!")

flopCallback :: MainLoop -> Notification -> t -> IO ()
flopCallback l n _ = do
  close n
  putStrLn "Pfft.."
  mainLoopQuit l

second :: Int
second = 1000000
