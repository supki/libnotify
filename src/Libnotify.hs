{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | High level interface to libnotify API
module Libnotify
  ( -- * Notification API
    Notification
  , display
  , display_
  , close
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
  , appName
  , reuse
  ) where

import Control.Applicative ((<$))
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..), Last(..))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8)
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf)
import System.Glib.Properties (objectSetPropertyString)

import Libnotify.C.Notify
import Libnotify.C.NotifyNotification

{-# ANN module "HLint: ignore Avoid lambda" #-}


-- | Notification object
data Notification = Notification
  { token :: !NotifyNotification
  , name  :: !String
  } deriving (Show, Eq)

-- | Display notification
--
-- >>> token <- display (summary "Greeting" <> body "Hello world!" <> icon "face-smile-big")
--
-- <<asset/HelloWorld.png>>
--
-- You can 'reuse' notification tokens:
--
-- >>> display_ (reuse token <> body "Hey!")
--
-- <<asset/Hey.png>>
display :: Mod Notification -> IO Notification
display (Mod (Last reusedToken) (Last named) acts) = do
  let name = fromMaybe defaultAppName named
  _ <- notify_init name
  token <- maybe (notify_notification_new "" "" "") return reusedToken
  acts token name
  _ <- notify_notification_show token
  return Notification {token, name}

-- | Display and discard notification token
--
-- >>> display_ (summary "Greeting" <> body "Hello world!" <> icon "face-smile-big")
display_ :: Mod Notification -> IO ()
display_ m = () <$ display m

-- | Close notification
close :: Notification -> IO ()
close Notification {name, token} = () <$ do
  _ <- notify_init name
  notify_notification_close token

-- | A notification modifier
data Mod a = -- the unused type parameter cannot be removed without breaking backward compatibility
  Mod (Last NotifyNotification) (Last String) (NotifyNotification -> String -> IO ())

#if MIN_VERSION_base(4,9,0)
instance Semigroup (Mod a) where
  Mod u x fx <> Mod v y fy =
    Mod (u <> v) (x <> y) (\token name -> fx token name >> fy token name)
#endif

instance Monoid (Mod a) where
  mempty = Mod mempty mempty (\_ _ -> return ())
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
  mappend = (<>)
#else
  mappend (Mod u x fx) (Mod v y fy) =
    Mod (mappend u v) (mappend x y) (\token name -> fx token name >> fy token name)
#endif

-- | Set notification summary
--
-- >>> display_ (summary "Hello!")
--
-- <<asset/summary.png>>
summary :: String -> Mod Notification
summary t = act (\n -> objectSetPropertyString "summary" n t)

-- | Set notification body
--
-- >>> display_ (body "Hello world!")
--
-- <<asset/body.png>>
body :: String -> Mod Notification
body t = act (\n -> objectSetPropertyString "body" n t)

-- | Set notification icon
--
-- >>> display_ (icon "face-smile")
--
-- The argument is either icon name or file name
--
-- <<asset/icon.png>>
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
--
-- >>> display_ (action "hello" "Hello world!" (\_ _ -> return ()))
--
-- <<asset/action.png>>
action
  :: String                         -- ^ Name
  -> String                         -- ^ Button label
  -> (Notification -> String -> IO a) -- ^ Callback
  -> Mod Notification
action a l f =
  Mod mempty mempty
    (\token name -> notify_notification_add_action token a l (\p s' -> () <$ f (Notification p name) s'))

-- | Remove all actions from the notification
--
-- >>> let callback _ _ = return ()
-- >>> display_ (summary "No hello for you!" <> action "hello" "Hello world!" callback <> noactions)
--
-- <<asset/noactions.png>>
noactions :: Mod Notification
noactions = act notify_notification_clear_actions

-- | Set the application name.
appName :: String -> Mod Notification
appName name = Mod mempty (Last (Just name)) (\_ _ -> return ())

-- | Reuse existing notification token, instead of creating a new one
--
-- If you try to reuse multiple tokens, the last one wins, e.g.
--
-- >>> foo <- display (body "foo")
-- >>> bar <- display (body "bar")
-- >>> display_ (base foo <> base bar)
--
-- will show only \"bar\"
--
-- <<asset/reuse.png>>
reuse :: Notification -> Mod Notification
reuse Notification {token, name} = Mod (Last (Just token)) (Last (Just name)) (\_ _ -> return ())

-- A helper for making an I/O 'Mod'
act :: (NotifyNotification -> IO ()) -> Mod Notification
act f = Mod mempty mempty (\token _name -> f token)

-- The default application name used unless the user specifies the preferred one with 'appName'.
defaultAppName :: String
defaultAppName = "haskell-libnotify"
