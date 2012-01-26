{-# LANGUAGE FlexibleInstances #-}
module System.Libnotify.Usable
  ( withNotifications
  , setHint, generalize
  , oneShot) where

import Data.Word (Word8)
import Data.Int (Int32)
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

data GeneralHint = HintInt String Int32 | HintDouble String Double | HintString String String | HintByte String Word8 | HintArray String BS.ByteString

class Hint a where
  setHint :: Notification -> a -> IO ()
  generalize :: a -> GeneralHint

instance Hint GeneralHint where
  setHint n (HintInt k v) = setHint n (k,v)
  setHint n (HintDouble k v) = setHint n (k,v)
  setHint n (HintString k v) = setHint n (k,v)
  setHint n (HintByte k v) = setHint n (k,v)
  setHint n (HintArray k v) = setHint n (k,v)
  generalize = id

instance Hint (Key,Int32) where
  setHint n (k,v) = setHintInt32 n k v
  generalize (k, v) = HintInt k v

instance Hint (Key,Double) where
  setHint n (k,v) = setHintDouble n k v
  generalize (k, v) = HintDouble k v

instance Hint (Key,String) where
  setHint n (k,v) = setHintString n k v
  generalize (k, v) = HintString k v

instance Hint (Key,Word8) where
  setHint n (k,v) = setHintByte n k v
  generalize (k, v) = HintByte k v

instance Hint (Key,BS.ByteString) where
  setHint n (k,v) = setHintByteArray n k v
  generalize (k, v) = HintArray k v
