{-# LANGUAGE FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving  #-}
-- | System.Libnotify module deals with notification session processing.
{-# OPTIONS_HADDOCK prune #-}
module System.Libnotify
  ( Notify, NotifyState, NotifyError (..)
  , oneShot, withNotifications
  , new, continue, update, render, close
  , setTimeout, setCategory, setUrgency
  , addHint, removeHints
  , addAction, removeActions
  , setIconFromPixbuf, setImageFromPixbuf
  , module System.Libnotify.Types
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf)

import System.Libnotify.Internal (Notification)
import qualified System.Libnotify.Internal as N
import System.Libnotify.Types

-- | Notification state. Contains next rendered notification data.
data NotifyState = NotifyState Title Body Icon

-- | Libnotify errors.
data NotifyError
  = NotifyInitHasFailed  -- ^ notify_init() has failed.
  | NewCalledBeforeInit  -- ^ 'new' has called before notify_init().
  deriving Show

-- | Notification monad. Saves notification context.
newtype Notify a = Notify { runNotify :: StateT NotifyState (ReaderT Notification IO) a } deriving (Functor, Monad, MonadIO)

{-|
  Initializes and uninitializes libnotify API.
  Any notifications API calls should be wrapped into @withNotifications@, i.e.

  > main = withNotifications (Just "api-name") $ do { ... here are notification API calls ... }
-}
withNotifications :: Maybe String -> IO a -> IO (Either NotifyError ())
withNotifications a x = (N.initNotify . fromMaybe " ") a >>= \initted ->
                        if initted
                          then Right <$> (x >> N.uninitNotify)
                          else return $ Left NotifyInitHasFailed

-- | Function for one-time notification with hints perhaps. Should be enough for a vast majority of applications.
oneShot :: Title -> Body -> Icon -> Maybe [Hint] -> IO (Either NotifyError ())
oneShot t b i hs = withNotifications Nothing . new t b i $ mapM_ addHint (fromMaybe [] hs) >> render

-- | Creates new notification session. Inside 'new' call one can manage current notification via 'update' or 'render' calls.
-- Returns notification pointer. This could be useful if one wants to 'update' or 'close' the same notification after some time (see 'continue').
new :: Title -> Body -> Icon -> Notify t -> IO (Either NotifyError (Notification, NotifyState))
new t b i f = N.isInitted >>= \initted ->
              if initted
                then do n <- N.newNotify t (listToMaybe b) (listToMaybe i)
                        s <- continue (n, NotifyState t b i) f
                        return $ Right (n, s)
                else return $ Left NewCalledBeforeInit

-- | Continues old notification session.
continue :: (Notification, NotifyState) -> Notify a -> IO NotifyState
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
     liftIO $ N.updateNotify n nt (listToMaybe nb) (listToMaybe ni)

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

-- | Adds 'Hint' to notification.
addHint :: Hint -> Notify ()
addHint (HintInt k v) =  Notify $ ask >>= liftIO . N.setHintInt32 k v
addHint (HintDouble k v) = Notify $ ask >>= liftIO . N.setHintDouble k v
addHint (HintString k v) = Notify $ ask >>= liftIO . N.setHintString k v
addHint (HintByte k v) = Notify $ ask >>= liftIO . N.setHintByte k v
addHint (HintArray k v) = Notify $ ask >>= liftIO . N.setHintByteArray k v

-- | Removes hints from notification.
removeHints :: Notify ()
removeHints = Notify $ ask >>= liftIO . N.clearHints

-- | Adds action to notification.
addAction :: String -> String -> (Notification -> String -> IO ()) -> Notify ()
addAction a l c = Notify $ ask >>= liftIO . N.addAction a l c

-- | Removes actions from notification.
removeActions :: Notify ()
removeActions = Notify $ ask >>= liftIO . N.clearActions

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe xs = Just xs
