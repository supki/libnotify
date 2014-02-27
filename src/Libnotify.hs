{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-- | System.Libnotify module deals with notification session processing.
{-# OPTIONS_HADDOCK prune #-}
module Libnotify
  ( oneShot
  , Notify
  , NotifyState
  , NotifyError(..)
  , withNotifications
  , new, continue, update, render, close
  , setTimeout, setCategory, setUrgency
  , addHint, removeHints
  , addAction, removeActions
  , setImageFromPixbuf
  ) where

import Control.Exception (Exception, throwIO, finally)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf)

import Libnotify.C.Notify
import Libnotify.C.NotifyNotification

{-# ANN module "HLint: ignore Use if" #-}


-- | Type synonim for notification title.
type Title = String
-- | Type synonim for notification body.
type Body = String
-- | Type synonim for notification icon.
type Icon = String

-- | Hint is some setting (server-dependent) which comes with notification.
data Hint = HintInt String Int32
          | HintDouble String Double
          | HintString String String
          | HintByte String Word8
          | HintArray String ByteString
-- | Notification state. Contains next rendered notification data.
data NotifyState = NotifyState Title Body Icon

-- | Libnotify errors.
data NotifyError =
    NotifyInitEmptyAppNameError -- ^ empty string is passed to notify_init()
  | NotifyInitFailedError       -- ^ notify_init() has failed for another reason
    deriving (Show, Eq, Typeable)

instance Exception NotifyError

-- | Notification monad. Saves notification context.
newtype Notify a = Notify
  { runNotify :: StateT NotifyState
                (ReaderT NotifyNotification IO) a }
    deriving (Functor, Monad, MonadIO)

-- | One-time notification
--
-- Handles @notify_init()@/@notify_uninit()@ business
oneShot
  :: String -- ^ Application name
  -> String -- ^ Summary
  -> String -- ^ Body
  -> String -- ^ Icon name or file name
  -> [Hint] -- ^ List of server hints
  -> IO ()
oneShot n t b i hs = do
  withNotifications n . new t b i $ do
    mapM_ addHint hs
    render
  return ()

-- | Initialize libnotify for the inner 'IO' action
--
-- Use this if you plan to do something more involved than 'oneShot'
--
-- Throws:
--
--   * 'NotifyInitEmptyAppNameError' if application name is an empty string
--
--   * 'NotifyInitFailedError' if @notify_init()@ failed for another reason
withNotifications :: String -> IO a -> IO a
withNotifications "" _ = throwIO NotifyInitEmptyAppNameError
withNotifications notifier io = do
  ret <- notify_init notifier
  case ret of
    False -> throwIO NotifyInitFailedError
    True  -> io `finally` notify_uninit

-- | Creates new notification session. Inside 'new' call one can manage current notification via 'update' or 'render' calls.
-- Returns notification pointer. This could be useful if one wants to 'update' or 'close' the same notification after some time (see 'continue').
new :: Title -> Body -> Icon -> Notify t -> IO (NotifyNotification, NotifyState)
new t b i f = do
  n <- notify_notification_new t b i
  s <- continue (n, NotifyState t b i) f
  return (n, s)

-- | Continues old notification session.
continue :: (NotifyNotification, NotifyState) -> Notify a -> IO NotifyState
continue (n, s) f = runReaderT (execStateT (runNotify f) s) n

-- | Updates notification 'Title', 'Body' and 'Icon'.
-- User can update notification partially, passing Nothing to arguments that should not changed.
update :: Maybe Title -> Maybe Body -> Maybe Icon -> Notify Bool
update mt mb mi = Notify $
  do n <- ask
     NotifyState t b i <- get
     let nt = fromMaybe t mt
         nb = fromMaybe b mb
         ni = fromMaybe i mi
     put (NotifyState nt nb ni)
     liftIO $ notify_notification_update n nt nb ni

-- | Shows notification to user.
render :: Notify Bool
render = withNotification notify_notification_show

-- | Closes notification.
close :: Notify Bool
close = withNotification notify_notification_close

-- | Sets notification 'Timeout'.
setTimeout :: Timeout -> Notify ()
setTimeout = withNotification . flip notify_notification_set_timeout

-- | Sets notification 'Category'.
setCategory :: String -> Notify ()
setCategory = withNotification . flip notify_notification_set_category

-- | Sets notification 'Urgency'.
setUrgency :: Urgency -> Notify ()
setUrgency = withNotification . flip notify_notification_set_urgency

-- | Sets notification image from pixbuf
setImageFromPixbuf :: Pixbuf -> Notify ()
setImageFromPixbuf = withNotification . flip notify_notification_set_image_from_pixbuf

-- | Adds 'Hint' to notification.
addHint :: Hint -> Notify ()
addHint (HintInt k v) = withNotification $ \n -> notify_notification_set_hint_int32 n k v
addHint (HintDouble k v) = withNotification $ \n -> notify_notification_set_hint_double n k v
addHint (HintString k v) = withNotification $ \n -> notify_notification_set_hint_string n k v
addHint (HintByte k v) = withNotification $ \n -> notify_notification_set_hint_byte n k v
addHint (HintArray k v) = withNotification $ \n -> notify_notification_set_hint_byte_array n k v

-- | Removes hints from notification.
removeHints :: Notify ()
removeHints = withNotification notify_notification_clear_hints

-- | Adds action to notification.
addAction :: String -> String -> (NotifyNotification -> String -> IO ()) -> Notify ()
addAction a l c = withNotification $ \n -> notify_notification_add_action n a l c

-- | Removes actions from notification.
removeActions :: Notify ()
removeActions = withNotification notify_notification_clear_actions


withNotification :: (NotifyNotification -> IO a) -> Notify a
withNotification f = Notify $ ask >>= liftIO . f
