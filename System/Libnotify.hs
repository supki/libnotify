{-# LANGUAGE FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving  #-}
-- | System.Libnotify module deals with notification session processing.
{-# OPTIONS_HADDOCK prune #-}
module System.Libnotify
  ( oneShot, withNotifications
  , new, continue, update, render, close
  , setTimeout, setCategory, setUrgency
  , Hint(..), GeneralHint, removeHints
  , addAction, removeActions
  , notifyErrorHandler
  , setIconFromPixbuf, setImageFromPixbuf
  ) where

import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf)
import System.IO (stderr, hPutStrLn)

import System.Libnotify.Internal (Notification)
import qualified System.Libnotify.Internal as N
import System.Libnotify.Types

-- | Notification state. Contains next rendered notification data.
data NotifyState = NotifyState Title Body Icon

-- | Notification monad. Saves notification context.
newtype Notify a = Notify { runNotify :: StateT NotifyState (ReaderT Notification IO) a } deriving (Functor, Monad, MonadIO)

{-|
  Initializes and uninitializes libnotify API.
  Any notifications API calls should be wrapped into @withNotifications@, i.e.

  > main = withNotifications (Just "api-name") $ do { ... here are notification API calls ... }
-}
withNotifications :: Maybe String -> IO a -> IO ()
withNotifications a x = (N.initNotify . fromMaybe " ") a >>= \initted ->
                        if initted
                          then x >> N.uninitNotify
                          else throw NotifyInitHasFailed

-- | Function for one-time notification with hints perhaps. Should be enough for a vast majority of applications.
oneShot :: Hint a => Title -> Body -> Icon -> [a] -> IO ()
oneShot t b i hs = withNotifications Nothing . new t b i $ mapM_ addHint hs >> render

-- | Creates new notification session. Inside 'new' call one can manage current notification via 'update' or 'render' calls.
-- Returns notification pointer. This could be useful if one wants to 'update' or 'close' the same notification after some time (see 'continue').
new :: Title -> Body -> Icon -> Notify t -> IO Notification
new t b i f = N.isInitted >>= \initted ->
              if initted
                then do n <- N.newNotify t (listToMaybe b) (listToMaybe i)
                        continue t b i n f
                        return n
                else throw NewCalledBeforeInit

-- | Continues old notification session.
continue :: Title -> Body -> Icon -> Notification -> Notify a -> IO ()
continue t b i s f = void $ (runReaderT (runStateT (runNotify f) (NotifyState t b i)) s)

-- | Updates notification 'Title', 'Body' and 'Icon'.
-- User can update notification partially, passing Nothing to arguments that should not changed.
update :: Maybe Title -> Maybe Body -> Maybe Icon -> Notify Bool
update mt mb mi = Notify $
  do n <- ask
     NotifyState t b i <- get
     let nt = fromMaybe t mt
         nb = fromMaybe b mb
         ni = fromMaybe i mi
     r <- liftIO $ N.updateNotify n nt (listToMaybe nb) (listToMaybe ni)
     put (NotifyState nt nb ni)
     return r

-- | Shows notification to user.
render :: Notify Bool
render = Notify $ ask >>= liftIO . N.showNotify

-- | Closes notification.
close :: Notify Bool
close = Notify $ ask >>= liftIO . N.closeNotify

-- | Sets notification 'Timeout'.
setTimeout :: Timeout -> Notify ()
setTimeout t = Notify $ ask >>= liftIO . N.setTimeout t

-- | Sets notification 'Category'.
setCategory :: Category -> Notify ()
setCategory c = Notify $ ask >>= liftIO . N.setCategory c

-- | Sets notification 'Urgency'.
setUrgency :: Urgency -> Notify ()
setUrgency u = Notify $ ask >>= liftIO . N.setUrgency u

-- | Sets notification icon from pixbuf
setIconFromPixbuf :: Pixbuf -> Notify ()
setIconFromPixbuf p = Notify $ ask >>= liftIO . N.setIconFromPixbuf p

-- | Sets notification image from pixbuf
setImageFromPixbuf :: Pixbuf -> Notify ()
setImageFromPixbuf p = Notify $ ask >>= liftIO . N.setImageFromPixbuf p

-- | Instance of 'Hint' class. Useful for 'oneShot' calls with empty hint list.
data GeneralHint = HintInt String Int32 | HintDouble String Double | HintString String String | HintByte String Word8 | HintArray String BS.ByteString

-- | Hint is some setting (server-dependent) which comes with notification.
class Hint a where
  -- | Adds 'Hint' to notification.
  addHint :: a -> Notify ()
  -- | Generalizes 'Hint' to some type. Could be useful if one wants to pass list of distinct hints types.
  generalize :: a -> GeneralHint

instance Hint GeneralHint where
  addHint (HintInt k v) = addHint (k,v)
  addHint (HintDouble k v) = addHint (k,v)
  addHint (HintString k v) = addHint (k,v)
  addHint (HintByte k v) = addHint (k,v)
  addHint (HintArray k v) = addHint (k,v)
  generalize = id

instance Hint (Key,Int32) where
  addHint (k,v) = Notify $ ask >>= \s -> liftIO $ N.setHintInt32 s k v
  generalize (k,v) = HintInt k v

instance Hint (Key,Double) where
  addHint (k,v) = Notify $ ask >>= \s -> liftIO $ N.setHintDouble s k v
  generalize (k,v) = HintDouble k v

instance Hint (Key,String) where
  addHint (k,v) = Notify $ ask >>= \s -> liftIO $ N.setHintString s k v
  generalize (k,v) = HintString k v

instance Hint (Key,Word8) where
  addHint (k,v) = Notify $ ask >>= \s -> liftIO $ N.setHintByte s k v
  generalize (k,v) = HintByte k v

instance Hint (Key,BS.ByteString) where
  addHint (k,v) = Notify $ ask >>= \s -> liftIO $ N.setHintByteArray s k v
  generalize (k,v) = HintArray k v

-- | Removes hints from notification.
removeHints :: Notify ()
removeHints = Notify $ ask >>= liftIO . N.clearHints

-- | Adds action to notification.
addAction :: String -> String -> (Notification -> String -> IO ()) -> Notify ()
addAction a l c = Notify $ ask >>= \s -> liftIO $ N.addAction s a l c

-- | Removes actions from notification.
removeActions :: Notify ()
removeActions = Notify $ ask >>= liftIO . N.clearActions

-- | Libnotify error handler
notifyErrorHandler :: NotifyError -> IO ()
notifyErrorHandler NotifyInitHasFailed = hPutStrLn stderr "withNotifications: init has failed."
notifyErrorHandler NewCalledBeforeInit = hPutStrLn stderr "new: Libnotify is not initialized properly."

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe xs = Just xs
