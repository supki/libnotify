module Main where

import Control.Monad
import System.Environment
import System.Glib.GError
import System.IO
import System.Libnotify

initial = do
  result <- getProgName >>= initNotify
  putStrLn $ "init: " ++ show result

new message = do
  notify <- newNotify message Nothing Nothing
  putStrLn $ "new: " ++ show notify
  return notify

open n = do
  catchGError (showNotify n) $ \(GError dom code msg) -> do
    putStr "show: " >> print (dom, code, msg)
    return False

close n = do
  catchGError (closeNotify n) $ \(GError dom code msg) -> do
    putStr "show: " >> print (dom, code, msg)
    return False

loop count lastNotify = do
  putStr $ show count ++ "> "
  hFlush stdout
  input <- getLine
  when (null input) initial
  notify <- new input
  case lastNotify of
    Just n  -> close n
    Nothing -> return True
  open notify
  loop (succ count) (Just notify)

main = initial >> loop 0 Nothing
