{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  , module Data.Functor.Identity
  , module Data.Semigroup
  ) where

import Control.Applicative (Applicative, pure, (<$))
import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Foldable (Foldable, for_)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int32)
import Data.Semigroup (Semigroup(..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf)

import Libnotify.C.Notify
import Libnotify.C.NotifyNotification

{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Use if" #-}


-- | Notification token
data Notification m = Notification
  { token_   :: m NotifyNotification
  , summary_ :: String
  , body_    :: String
  , icon_    :: String
  }

deriving instance Show (m NotifyNotification) => Show (Notification m)
deriving instance Eq (m NotifyNotification) => Eq (Notification m)

-- | Libnotify errors
data NotifyError =
    NotifyInitEmptyAppNameError -- ^ empty string is passed to notify_init()
  | NotifyInitFailedError       -- ^ notify_init() has failed for another reason
  | NotifyShowEmptySummaryError -- ^ notify_init() has failed for another reason
    deriving (Show, Eq, Typeable)

instance Exception NotifyError

-- | Display notification
--
-- >>> display (summary "Greeting" <> body "Hello world!" <> icon "face-smile-big")
display :: Mod Notification -> IO (Notification Identity)
display (Mod f a) = do
  notify_init "haskell-libnotify"
  let x = f empty
  n <- case token_ x of
    Nothing -> notify_notification_new (summary_ x) (body_ x) (icon_ x)
    Just n  -> do
      notify_notification_update n (summary_ x) (body_ x) (icon_ x)
      return n
  let y = x { token_ = Identity n }
  a y
  notify_notification_show n
  return y
 where empty = Notification Nothing "" "" ""

-- | Close notification
close :: Foldable m => Notification m -> IO ()
close n = do
  notify_init "haskell-libnotify"
  for_ (token_ n) notify_notification_close

data Mod a = Mod (a Maybe -> a Maybe) (Notification Identity -> IO ())

instance Semigroup (Mod a) where
  Mod f a <> Mod g b = Mod (g . f) (\x -> a x >> b x)

-- | Modify existing notification token, instead of creating a new one
base :: Notification Identity -> Mod Notification
base n = passing (\_ -> nmap (Just . runIdentity) n)

-- | Set notification summary
--
-- Summary should not be an empty string
summary :: String -> Mod Notification
summary t = passing (\n -> n { summary_ = t })

-- | Set notification body
body :: String -> Mod Notification
body t = passing (\n -> n { body_ = t })

-- | Set notification icon
--
-- The argument is either icon name or file name
icon :: String -> Mod Notification
icon t = passing (\n -> n { icon_ = t })

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
action :: String -> String -> (Notification Identity -> String -> IO a) -> Mod Notification
action a l f =
  Mod id (\n -> notify_notification_add_action (runToken n) a l
    (\p s' -> () <$ f (n { token_ = Identity p }) s'))

-- | Remove all actions from the notification
noactions :: Mod Notification
noactions = act notify_notification_clear_actions

-- A helper for making a pure 'Mod'
passing :: (m Maybe -> m Maybe) -> Mod m
passing f = Mod f (\_ -> pure ())

-- A helper for making an I/O 'Mod'
act :: (NotifyNotification -> IO ()) -> Mod m
act f = Mod id (f . runToken)

-- Get a libnotify pointer from the Notification
runToken :: Notification Identity -> NotifyNotification
runToken = runIdentity . token_

-- Notification morphism
nmap :: (forall a. m a -> n a) -> Notification m -> Notification n
nmap f n = n { token_ = f (token_ n) }
