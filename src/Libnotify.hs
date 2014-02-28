{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
-- | High level interface to libnotify API
--
-- <<asset/Greeting.png>>
module Libnotify
  ( -- * Notification API
    Notification
  , display
  , close
  , NotifyError(..)
    -- * Modifiers
  , Mod
  , base
  , summary
  , body
  , icon
  , timeout
  , Timeout(..)
  , category
  , urgency
  , Urgency(..)
  , image
  , Hint(..)
  , nohints
  , action
  , noactions
    -- * Concenience re-exports
  , module Data.Semigroup
  ) where

import Control.Applicative ((<$))
import Control.Exception (Exception)
import Control.Monad (mplus)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Semigroup (Semigroup(..), Monoid(..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf)
import System.Glib.Properties (objectSetPropertyString)

import Libnotify.C.Notify
import Libnotify.C.NotifyNotification

{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Use if" #-}


-- | Notification token
newtype Notification = Notification
  { token_   :: NotifyNotification
  } deriving (Show, Eq)

-- | Libnotify errors
data NotifyError =
    NotifyInitEmptyAppNameError -- ^ empty string is passed to notify_init()
  | NotifyInitFailedError       -- ^ notify_init() has failed for another reason
    deriving (Show, Eq, Typeable)

instance Exception NotifyError

-- | Display notification
--
-- >>> display (summary "Greeting" <> body "Hello world!" <> icon "face-smile-big")
display :: Mod Notification -> IO Notification
display (Mod m a) = do
  notify_init "haskell-libnotify"
  n <- maybe (notify_notification_new "" "" "") (return . token_) m
  let y = Notification n
  a y
  notify_notification_show n
  return y

-- | Close notification
close :: Notification -> IO ()
close n = () <$ do
  notify_init "haskell-libnotify"
  notify_notification_close (token_ n)

-- | A notification modifier
data Mod a = Mod (Maybe Notification) (Notification -> IO ())

instance Semigroup (Mod a) where
  Mod m a <> Mod n b = Mod (n `mplus` m) (\x -> a x >> b x)

instance Monoid (Mod a) where
  mempty = Mod Nothing (\_ -> return ())
  mappend = (<>)

-- | Modify existing notification token, instead of creating a new one
base :: Notification -> Mod Notification
base n = Mod (Just n) (\_ -> return ())

-- | Set notification summary
--
-- Summary should not be an empty string
summary :: String -> Mod Notification
summary t = act (\n -> objectSetPropertyString "summary" n t)

-- | Set notification body
body :: String -> Mod Notification
body t = act (\n -> objectSetPropertyString "body" n t)

-- | Set notification icon
--
-- The argument is either icon name or file name
icon :: String -> Mod Notification
icon t = act (\n -> objectSetPropertyString "icon-name" n t)

-- | Set notification timeout
timeout :: Timeout -> Mod Notification
timeout t = act (\n -> notify_notification_set_timeout n t)

-- | Set notification category
category :: String -> Mod Notification
category t = act (\n -> notify_notification_set_category n t)

-- | Set notification urgency
urgency :: Urgency -> Mod Notification
urgency t = act (\n -> notify_notification_set_urgency n t)

-- | Set notification image
image :: Pixbuf -> Mod Notification
image t = act (\n -> notify_notification_set_image_from_pixbuf n t)

-- | Add a hint to notification
--
-- It's perfectly OK to add multiple hints to a single notification
class Hint v where
  hint :: String -> v -> Mod Notification

instance Hint Int32 where
  hint k v = act (\n -> notify_notification_set_hint_int32 n k v)

instance Hint Double where
  hint k v = act (\n -> notify_notification_set_hint_double n k v)

instance Hint String where
  hint k v = act (\n -> notify_notification_set_hint_string n k v)

instance Hint Word8 where
  hint k v = act (\n -> notify_notification_set_hint_byte n k v)

instance Hint ByteString where
  hint k v = act (\n -> notify_notification_set_hint_byte_array n k v)

-- | Remove all hints from the notification
nohints :: Mod Notification
nohints = act notify_notification_clear_hints

-- | Add an action to notification
--
-- It's perfectly OK to add multiple actions to a single notification
action :: String -> String -> (Notification -> String -> IO a) -> Mod Notification
action a l f =
  Mod Nothing (\n -> notify_notification_add_action (token_ n) a l
    (\p s' -> () <$ f (Notification p) s'))

-- | Remove all actions from the notification
noactions :: Mod Notification
noactions = act notify_notification_clear_actions

-- A helper for making an I/O 'Mod'
act :: (NotifyNotification -> IO ()) -> Mod m
act f = Mod Nothing (f . token_)
