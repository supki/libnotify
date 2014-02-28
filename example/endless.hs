#!/usr/bin/env runhaskell

-- | This little example program shows how to use libnotify action
-- callback together with glib MainLoop thing so they actually work

import Control.Applicative ((<$))
import Control.Concurrent (threadDelay)
import Data.Functor.Identity
import Libnotify
import System.Glib.MainLoop (MainLoop, mainLoopNew, mainLoopRun, mainLoopQuit)

main :: IO ()
main = () <$ do
  l <- mainLoopNew Nothing False
  display $
       summary "Hello!"
    <> body query
    <> icon "face-embarrassed"
    <> timeout Infinite
    <> action "blob" "Say \"blop\"" blopCallback
    <> action "flop" "Say \"flop\"" (flopCallback l)
  mainLoopRun l

blopCallback :: Notification Identity -> t -> IO (Notification Identity)
blopCallback n _ = do
  close n
  putStrLn response
  threadDelay second
  display (base n)

flopCallback :: MainLoop -> Notification Identity -> t -> IO ()
flopCallback l n _ = do
  close n
  putStrLn "Pfft.."
  mainLoopQuit l

query, response :: String
query = "Please, say \"blop\"!"
response = "Thanks!"

second :: Int
second = 1000000
