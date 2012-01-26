{-# LANGUAGE FlexibleInstances #-}
module System.Libnotify.Usable
  ( withNotifications
  , Hint, setHint
  , oneShot) where

import Data.Word (Word8)
import qualified Data.ByteString as BS

import System.Libnotify

withNotifications :: Maybe String -> IO a -> IO ()
withNotifications a x = do initted <- initNotify appName
                           if initted
                             then x >> uninitNotify
                             else error "withNotifications: init has failed."
  where appName = case a of
                    Just name -> name
                    Nothing   -> " "

type Title = String
type Body = String
type Icon = String

oneShot :: Hint a => Title -> Maybe Body -> Maybe Icon -> [a] -> IO ()
oneShot t b i hs = withNotifications Nothing $ do n <- newNotify t b i
                                                  mapM_ (setHint n) hs
                                                  showNotify n

type Key = String

class Hint a where
  setHint :: Notification -> a -> IO ()

instance Hint (Key,Int) where
  setHint n (k,v) = setHintInt32 n k v

instance Hint (Key,Double) where
  setHint n (k,v) = setHintDouble n k v

instance Hint (Key,String) where
  setHint n (k,v) = setHintString n k v

instance Hint (Key,Word8) where
  setHint n (k,v) = setHintByte n k v

instance Hint (Key,BS.ByteString) where
  setHint n (k,v) = setHintByteArray n k v
