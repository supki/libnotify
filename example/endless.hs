#!/usr/bin/env runhaskell

-- | This little example program shows how to use libnotify action
-- callback together with glib MainLoop thing so they actually work

import Control.Concurrent (threadDelay)
import Libnotify
import Libnotify.C.NotifyNotification
import System.Glib.MainLoop (MainLoop, mainLoopNew, mainLoopRun, mainLoopQuit)

main :: IO ()
main = withNotifications "endless" $ do
  l <- mainLoopNew Nothing False
  n <- notify_notification_new "Query" query "question"
  notify_notification_set_timeout n Infinite
  notify_notification_add_action n "blop" "blop" blopCallback
  notify_notification_add_action n "flop" "flop" (flopCallback l)
  notify_notification_show n
  mainLoopRun l

blopCallback :: NotifyNotification -> t -> IO ()
blopCallback n _ = do
  notify_notification_close n
  putStrLn "Thanks!"
  threadDelay second
  notify_notification_show n
  return ()

flopCallback :: MainLoop -> NotifyNotification -> t -> IO ()
flopCallback l n _ = do
  notify_notification_close n
  putStrLn "Pfft.."
  mainLoopQuit l

query :: String
query = "Please, choose \"blop\""

second :: Int
second = 1000000
