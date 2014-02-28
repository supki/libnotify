{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
-- | High level interface to libnotify API
module Libnotify
  ( -- * Notification API
    Notification
  , display
  , display_
  , close
  , NotifyError(..)
    -- * Modifiers
  , Mod
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
  , reuse
    -- * Concenience re-exports
  , Semigroup(..)
  , Monoid(..)
  ) where

import Control.Applicative ((<$))
import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Semigroup (Semigroup(..), Monoid(..), Option(..), Last(..), option)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf)
import System.Glib.Properties (objectSetPropertyString)

import Libnotify.C.Notify
import Libnotify.C.NotifyNotification

{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Use if" #-}


-- | Notification object
newtype Notification = Notification
  { unNotification :: NotifyNotification
  } deriving (Show, Eq)

-- | Libnotify errors
data NotifyError =
    NotifyInitEmptyAppNameError -- ^ empty string is passed to notify_init()
  | NotifyInitFailedError       -- ^ notify_init() has failed for another reason
    deriving (Show, Eq, Typeable)

instance Exception NotifyError

-- | Display notification
--
-- >>> token <- display (summary "Greeting" <> body "Hello world!" <> icon "face-smile-big")
--
-- <<asset/HelloWorld.png>>
--
-- You can 'reuse' notification tokens:
--
-- >>> display (reuse token <> body "Hey!")
--
-- <<asset/Hey.png>>
display :: Mod Notification -> IO Notification
display (Mod m a) = do
  notify_init "haskell-libnotify"
  n <- option (notify_notification_new "" "" "") (return . unNotification . getLast) m
  let y = Notification n
  a y
  notify_notification_show n
  return y

-- | Display and discard notification token
--
-- >>> display_ (summary "Greeting" <> body "Hello world!" <> icon "face-smile-big")
display_ :: Mod Notification -> IO ()
display_ m = () <$ display m

-- | Close notification
close :: Notification -> IO ()
close n = () <$ do
  notify_init "haskell-libnotify"
  notify_notification_close (unNotification n)

-- | A notification modifier
data Mod a = Mod (Option (Last a)) (a -> IO ())

instance Semigroup (Mod a) where
  Mod m a <> Mod n b = Mod (m <> n) (\x -> a x >> b x)

instance Monoid (Mod a) where
  mempty = Mod mempty (\_ -> return ())
  mappend = (<>)

-- | Set notification summary
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
  Mod mempty (\n -> notify_notification_add_action (unNotification n) a l
    (\p s' -> () <$ f (Notification p) s'))

-- | Remove all actions from the notification
noactions :: Mod Notification
noactions = act notify_notification_clear_actions

-- | Reuse existing notification token, instead of creating a new one
--
-- If you try to reuse multiple tokens, the last one wins, e.g.
--
-- >>> foo <- display (body "foo")
-- >>> bar <- display (body "bar")
-- >>> display (base foo <> base bar)
--
-- will show \"foo\" once and \"bar\" twice
reuse :: Notification -> Mod Notification
reuse n = Mod (Option (Just (Last n))) (\_ -> return ())

-- A helper for making an I/O 'Mod'
act :: (NotifyNotification -> IO ()) -> Mod Notification
act f = Mod mempty (f . unNotification)
