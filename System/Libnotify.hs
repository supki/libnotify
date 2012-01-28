{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | System.Libnotify module deals with notification session processing.
{-# OPTIONS_HADDOCK prune #-}
module System.Libnotify
  ( withNotifications
  , new, session, update, render, close
  , setTimeout, setCategory, setUrgency
  , Hint(..), Key, removeHints
  , addAction, removeActions
  , oneShot
  ) where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, liftIO, runReaderT, ask)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import Foreign (Ptr)

import qualified System.Libnotify.Internal as N
import System.Libnotify.Types

{-|
  Initializes and Uninitializes libnotify API.
  Any notifications API calls should be wrapped into @withNotifications@, i.e.

  > main = withNotifications (Just "api-name") $ do { ... here are notification API calls ... }
-}
withNotifications :: Maybe String -> IO a -> IO ()
withNotifications a x = (N.initNotify . fromMaybe " ") a >>= \initted ->
                        if initted then x >> N.uninitNotify
                                   else error "withNotifications: init has failed."

-- | Function for one-time notification with hints perhaps. Should be enough for a vast majority of applications.
oneShot :: Hint a => Title -> Body -> Icon -> [a] -> IO ()
oneShot t b i hs = withNotifications Nothing $
                   new t b i $
                   mapM_ addHint hs >> render

-- | Creates new notification session. Inside 'new' call one can manage current notification via 'update' or 'render' calls.
-- Returns notification pointer. This could be useful if one wants to 'update' or 'close' the same notification after some business logic.
new :: Title -> Body -> Icon -> ReaderT (Ptr Notification) IO t -> IO (Ptr Notification)
new t b i f = N.isInitted >>= \initted ->
              if initted
                then do n <- N.newNotify t b i
                        session f n
                        return n
                else error "new: Libnotify is not initialized properly."

-- | Continues old notification session.
session :: ReaderT (Ptr Notification) IO a -> (Ptr Notification) -> IO ()
session f n = runReaderT f n >> return ()

-- | Updates notification 'Title', 'Body' and 'Icon'.
update :: (MonadIO m, MonadReader (Ptr Notification) m) => Title -> Body -> Icon -> m Bool
update t b i = ask >>= \n -> liftIO $ N.updateNotify n t b i

-- | Shows notification to user.
render :: (MonadIO m, MonadReader (Ptr Notification) m) => m Bool
render = ask >>= liftIO . N.showNotify

-- | Closes notification.
close :: (MonadIO m, MonadReader (Ptr Notification) m) => m Bool
close = ask >>= liftIO . N.closeNotify

-- | Sets notification 'Timeout'.
setTimeout :: (MonadIO m, MonadReader (Ptr Notification) m) => Timeout -> m ()
setTimeout t = ask >>= liftIO . N.setTimeout t

-- | Sets notification 'Category'.
setCategory :: (MonadIO m, MonadReader (Ptr Notification) m) => Category -> m ()
setCategory c = ask >>= liftIO . N.setCategory c

-- | Sets notification 'Urgency'.
setUrgency :: (MonadIO m, MonadReader (Ptr Notification) m) => Urgency -> m ()
setUrgency u = ask >>= liftIO . N.setUrgency u

data GeneralHint = HintInt String Int32 | HintDouble String Double | HintString String String | HintByte String Word8 | HintArray String BS.ByteString

-- | Hint is some setting (server-dependent) which comes with notification.
class Hint a where
  -- | Adds 'Hint' to notification.
  addHint :: a -> (MonadIO m, MonadReader (Ptr Notification) m) => m ()
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
  addHint (k,v) = ask >>= \n -> liftIO $ N.setHintInt32 n k v
  generalize (k,v) = HintInt k v

instance Hint (Key,Double) where
  addHint (k,v) = ask >>= \n -> liftIO $ N.setHintDouble n k v
  generalize (k,v) = HintDouble k v

instance Hint (Key,String) where
  addHint (k,v) = ask >>= \n -> liftIO $ N.setHintString n k v
  generalize (k,v) = HintString k v

instance Hint (Key,Word8) where
  addHint (k,v) = ask >>= \n -> liftIO $ N.setHintByte n k v
  generalize (k,v) = HintByte k v

instance Hint (Key,BS.ByteString) where
  addHint (k,v) = ask >>= \n -> liftIO $ N.setHintByteArray n k v
  generalize (k,v) = HintArray k v

-- | Removes hints from notification.
removeHints :: (MonadIO m, MonadReader (Ptr Notification) m) => m ()
removeHints = ask >>= liftIO . N.clearHints

-- | Adds action to notification.
addAction :: (MonadIO m, MonadReader (Ptr Notification) m) => String -> String -> (Ptr Notification -> String -> IO ()) -> m ()
addAction a l c = ask >>= \n -> liftIO $ N.addAction n a l c

-- | Removes actions from notification.
removeActions :: (MonadIO m, MonadReader (Ptr Notification) m) => m ()
removeActions = ask >>= liftIO . N.clearActions
