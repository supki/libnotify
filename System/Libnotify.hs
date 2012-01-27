{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module System.Libnotify
  ( Title, Body, Icon
  , withNotifications
  , new, update, render, close
  , timeout, category, urgency
  , addHint, generalize, removeHints
  , addAction, removeActions
  , oneShot) where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, liftIO, runReaderT, ask)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import Foreign (Ptr)

import qualified System.Libnotify.Internal as N
import System.Libnotify.Types

withNotifications :: Maybe String -> IO a -> IO ()
withNotifications a x = (N.initNotify . fromMaybe " ") a >>= \initted ->
                        if initted then x >> N.uninitNotify
                                   else error "withNotifications: init has failed."

oneShot :: Hint a => Title -> Body -> Icon -> [a] -> IO ()
oneShot t b i hs = withNotifications Nothing $
                   new t b i $
                   mapM_ addHint hs >> render

new :: Title -> Body -> Icon -> ReaderT (Ptr Notification) IO t -> IO (Ptr Notification)
new t b i f = do n <- N.newNotify t b i
                 _ <- runReaderT f n
                 return n

update :: (MonadIO m, MonadReader (Ptr Notification) m) => Title -> Body -> Icon -> m Bool
update t b i = ask >>= \n -> liftIO $ N.updateNotify n t b i

render :: (MonadIO m, MonadReader (Ptr Notification) m) => m Bool
render = ask >>= liftIO . N.showNotify

close :: (MonadIO m, MonadReader (Ptr Notification) m) => m Bool
close = ask >>= liftIO . N.closeNotify

timeout :: (MonadIO m, MonadReader (Ptr Notification) m) => Timeout -> m ()
timeout t = ask >>= liftIO . N.setTimeout t

category :: (MonadIO m, MonadReader (Ptr Notification) m) => Category -> m ()
category c = ask >>= liftIO . N.setCategory c

urgency :: (MonadIO m, MonadReader (Ptr Notification) m) => Urgency -> m ()
urgency u = ask >>= liftIO . N.setUrgency u

data GeneralHint = HintInt String Int32 | HintDouble String Double | HintString String String | HintByte String Word8 | HintArray String BS.ByteString
type Key = String

class Hint a where
  addHint :: a -> (MonadIO m, MonadReader (Ptr Notification) m) => m ()
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

removeHints :: (MonadIO m, MonadReader (Ptr Notification) m) => m ()
removeHints = ask >>= liftIO . N.clearHints

addAction :: (MonadIO m, MonadReader (Ptr Notification) m) => String -> String -> (Ptr Notification -> String -> IO ()) -> m ()
addAction a l c = ask >>= \n -> liftIO $ N.addAction n a l c

removeActions :: (MonadIO m, MonadReader (Ptr Notification) m) => m ()
removeActions = ask >>= liftIO . N.clearActions
