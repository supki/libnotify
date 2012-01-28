{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | System.Libnotify module deals with notification session processing.
{-# OPTIONS_HADDOCK prune #-}
module System.Libnotify
  ( oneShot, withNotifications
  , new, continue, update, render, close
  , setTimeout, setCategory, setUrgency
  , Hint(..), GeneralHint, removeHints
  , addAction, removeActions
  , notifyErrorHandler
  ) where

import Control.Exception (throw)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, liftIO, runReaderT, ask)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import System.IO (stderr, hPutStrLn)

import qualified System.Libnotify.Internal as N
import System.Libnotify.Types

-- | Notification session. Saves notification initial state.
data Session = Session
                 { notification :: N.Notification
                 , title :: Title
                 , body :: Body
                 , icon :: Icon
                 }

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
oneShot t b i hs = withNotifications Nothing $
                     new t b i $
                       mapM_ addHint hs >> render

-- | Creates new notification session. Inside 'new' call one can manage current notification via 'update' or 'render' calls.
-- Returns notification pointer. This could be useful if one wants to 'update' or 'close' the same notification after some time (see 'continue').
new :: Title -> Body -> Icon -> ReaderT Session IO t -> IO Session
new t b i f = N.isInitted >>= \initted ->
              if initted
                then do n <- N.newNotify t (listToMaybeAll b) (listToMaybeAll i)
                        continue (Session n t b i) f
                        return (Session n t b i)
                else throw NewCalledBeforeInit

-- | Continues old notification session.
continue :: Session -> ReaderT Session IO a -> IO ()
continue s f = runReaderT f s >> return ()

-- | Updates notification 'Title', 'Body' and 'Icon'.
-- User can update notification partially, passing Nothing to arguments that should not changed.
update :: (MonadIO m, MonadReader Session m) => Maybe Title -> Maybe Body -> Maybe Icon -> m Bool
update nt nb ni = ask >>= \(Session n t b i) ->
                    liftIO $ N.updateNotify n
                               (fromMaybe t nt)
                               (listToMaybeAll (fromMaybe b nb))
                               (listToMaybeAll (fromMaybe i ni))

-- | Shows notification to user.
render :: (MonadIO m, MonadReader Session m) => m Bool
render = ask >>= liftIO . N.showNotify . notification

-- | Closes notification.
close :: (MonadIO m, MonadReader Session m) => m Bool
close = ask >>= liftIO . N.closeNotify . notification

-- | Sets notification 'Timeout'.
setTimeout :: (MonadIO m, MonadReader Session m) => Timeout -> m ()
setTimeout t = ask >>= liftIO . N.setTimeout t . notification

-- | Sets notification 'Category'.
setCategory :: (MonadIO m, MonadReader Session m) => Category -> m ()
setCategory c = ask >>= liftIO . N.setCategory c . notification

-- | Sets notification 'Urgency'.
setUrgency :: (MonadIO m, MonadReader Session m) => Urgency -> m ()
setUrgency u = ask >>= liftIO . N.setUrgency u . notification

-- | Instance of 'Hint' class. Useful for 'oneShot' calls with empty hint list.
data GeneralHint = HintInt String Int32 | HintDouble String Double | HintString String String | HintByte String Word8 | HintArray String BS.ByteString

-- | Hint is some setting (server-dependent) which comes with notification.
class Hint a where
  -- | Adds 'Hint' to notification.
  addHint :: a -> (MonadIO m, MonadReader Session m) => m ()
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
  addHint (k,v) = ask >>= \s -> liftIO $ N.setHintInt32 (notification s) k v
  generalize (k,v) = HintInt k v

instance Hint (Key,Double) where
  addHint (k,v) = ask >>= \s -> liftIO $ N.setHintDouble (notification s) k v
  generalize (k,v) = HintDouble k v

instance Hint (Key,String) where
  addHint (k,v) = ask >>= \s -> liftIO $ N.setHintString (notification s) k v
  generalize (k,v) = HintString k v

instance Hint (Key,Word8) where
  addHint (k,v) = ask >>= \s -> liftIO $ N.setHintByte (notification s) k v
  generalize (k,v) = HintByte k v

instance Hint (Key,BS.ByteString) where
  addHint (k,v) = ask >>= \s -> liftIO $ N.setHintByteArray (notification s) k v
  generalize (k,v) = HintArray k v

-- | Removes hints from notification.
removeHints :: (MonadIO m, MonadReader Session m) => m ()
removeHints = ask >>= liftIO . N.clearHints . notification

-- | Adds action to notification.
addAction :: (MonadIO m, MonadReader Session m) => String -> String -> (N.Notification -> String -> IO ()) -> m ()
addAction a l c = ask >>= \s -> liftIO $ N.addAction (notification s) a l c

-- | Removes actions from notification.
removeActions :: (MonadIO m, MonadReader Session m) => m ()
removeActions = ask >>= liftIO . N.clearActions . notification

-- | Libnotify error handler
notifyErrorHandler :: NotifyError -> IO ()
notifyErrorHandler NotifyInitHasFailed = hPutStrLn stderr "withNotifications: init has failed."
notifyErrorHandler NewCalledBeforeInit = hPutStrLn stderr "new: Libnotify is not initialized properly."

listToMaybeAll :: [a] -> Maybe [a]
listToMaybeAll [] = Nothing
listToMaybeAll xs = Just xs
