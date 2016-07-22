{-# LANGUAGE FlexibleInstances #-}
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
  , appname
  , reuse
    -- * Convenience re-exports
  , Monoid(..), (<>)
  ) where

import Control.Applicative ((<$))
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Monoid (Monoid(..), (<>), Last(..))
import Data.Word (Word8)
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf)
import System.Glib.Properties (objectSetPropertyString)

import Libnotify.C.Notify
import Libnotify.C.NotifyNotification

{-# ANN module "HLint: ignore Avoid lambda" #-}


-- | Notification object
data Notification = Notification
  { getNotification :: NotifyNotification
  , getAppName      :: String
  } deriving (Show, Eq)

defaultAppName :: String
defaultAppName = "haskell-libnotify"

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
display (Mod m a) = do
  n <- maybe (notify_init defaultAppName >> notify_notification_new "" "" "") (return . getNotification) (getLast m)
  let y = Notification n defaultAppName
  y' <- a y
  notify_init $ getAppName y'
  notify_notification_show $ getNotification y'
  return y'

-- | Display and discard notification token
--
-- >>> display_ (summary "Greeting" <> body "Hello world!" <> icon "face-smile-big")
display_ :: Mod Notification -> IO ()
display_ m = () <$ display m

-- | Close notification
close :: Notification -> IO ()
close n = () <$ do
  notify_init $ getAppName n
  notify_notification_close (getNotification n)

-- | A notification modifier
data Mod a = Mod (Last a) (a -> IO a)

instance Monoid (Mod a) where
  mempty = Mod mempty return
  mappend (Mod x fx) (Mod y fy) = Mod (x <> y) (fx >=> fy)

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
  Mod mempty (\n -> n <$ notify_notification_add_action (getNotification n) a l
    (\p s' -> () <$ f (n { getNotification = p }) s'))

-- | Remove all actions from the notification
--
-- >>> let callback _ _ = return ()
-- >>> display_ (summary "No hello for you!" <> action "hello" "Hello world!" callback <> noactions)
--
-- <<asset/noactions.png>>
noactions :: Mod Notification
noactions = act notify_notification_clear_actions

-- | Set the name used to send the notification
appname :: String -> Mod Notification
appname a = Mod mempty (\n -> return $ n { getAppName = a } )

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
reuse n = Mod (Last (Just n)) return

-- A helper for making an I/O 'Mod'
act :: (NotifyNotification -> IO ()) -> Mod Notification
act f = Mod mempty (\n -> n <$ f (getNotification n))
