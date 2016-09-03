{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Monad.InWorkDir
  ( MonadInWorkDir(..)
  , WorkDirT
  , WorkDir
  , workDirInIO
  , inWorkDirT
  , inWorkDir
  , runWorkDirT
  , runWorkDir
  ) where

import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Identity
import           Control.Monad.Writer
import           Data.Functor.Identity
import           Path
import           Path.IO

class Monad m => MonadInWorkDir m where
    -- | Get the currently stored working directory
    getWorkDir  :: m (Path Abs Dir)

    -- | Set the currently stored working directory such that subsequent calls
    -- to 'getWorkDir' will return the given filepath
    setWorkDir  :: Path Abs Dir -> m ()

    -- | Change the working directory so that calls to 'getWorkdir' in the
    -- second argument will return the given filepath (unless it is changed
    -- again there).
    withWorkDir :: Path Abs Dir -> m a -> m a
    withWorkDir wd action = do
        before <- getWorkDir
        setWorkDir wd
        result <- action
        setWorkDir before
        return result

newtype WorkDirT m a = WorkDirT { unWorkDirT :: StateT (Path Abs Dir) m a }
   deriving (Functor, Applicative, Monad, MonadTrans)

type WorkDir = WorkDirT Identity

-- | Run a 'WorkDirT' in IO by initialising with the current working and
-- finalising by setting the working directory at the end.
workDirInIO :: MonadIO io => WorkDirT io a -> io a
workDirInIO func = do
    wd <- liftIO getCurrentDir
    (result, wdAfter) <- runWorkDirT func wd
    liftIO $ setCurrentDir wdAfter
    return result

inWorkDirT :: Functor m => Path Abs Dir -> WorkDirT m a -> m a
inWorkDirT wd func = fst <$> runWorkDirT func wd

inWorkDir :: Path Abs Dir -> WorkDir a -> a
inWorkDir wd func = fst $ runWorkDir func wd

runWorkDirT :: WorkDirT m a -> Path Abs Dir -> m (a, Path Abs Dir)
runWorkDirT (WorkDirT sa) = runStateT sa

runWorkDir :: WorkDir a -> Path Abs Dir -> (a, Path Abs Dir)
runWorkDir (WorkDirT sa) p = runIdentity $ runStateT sa p

instance Monad m => MonadInWorkDir (WorkDirT m) where
    getWorkDir = WorkDirT get
    setWorkDir wd = WorkDirT $ put wd

instance MonadIO m => MonadIO (WorkDirT m) where
    liftIO = WorkDirT . liftIO

instance MonadInWorkDir m => MonadInWorkDir (ExceptT e m) where
    getWorkDir = lift getWorkDir
    setWorkDir = lift . setWorkDir

instance MonadInWorkDir m => MonadInWorkDir (ReaderT c m) where
    getWorkDir = lift getWorkDir
    setWorkDir = lift . setWorkDir

instance MonadInWorkDir m => MonadInWorkDir (IdentityT m) where
    getWorkDir = lift getWorkDir
    setWorkDir = lift . setWorkDir

instance MonadInWorkDir m => MonadInWorkDir (StateT s m) where
    getWorkDir = lift getWorkDir
    setWorkDir = lift . setWorkDir

instance (Monoid w, MonadInWorkDir m) => MonadInWorkDir (WriterT w m) where
    getWorkDir = lift getWorkDir
    setWorkDir = lift . setWorkDir

instance MonadBase b m => MonadBase b (WorkDirT m) where
    liftBase = liftBaseDefault

instance MonadState s m => MonadState s (WorkDirT m) where
    get = lift get
    put = lift . put

instance MonadError e m => MonadError e (WorkDirT m) where
    throwError = lift . throwError
    catchError (WorkDirT sa) handler
        = WorkDirT $ catchError sa $ \e -> unWorkDirT $ handler e

instance MonadReader c m => MonadReader c (WorkDirT m) where
    ask = lift ask
    local f (WorkDirT sa) = WorkDirT $ local f sa

instance (Monoid w, MonadWriter w m) => MonadWriter w (WorkDirT m) where
    tell = lift . tell
    listen (WorkDirT sa) = WorkDirT $ listen sa
    pass (WorkDirT sa) = WorkDirT $ pass sa

instance MonadTransControl WorkDirT where
    type StT WorkDirT a = StT (StateT (Path Abs Dir)) a
    liftWith = defaultLiftWith WorkDirT unWorkDirT
    restoreT = defaultRestoreT WorkDirT

instance MonadBaseControl b m => MonadBaseControl b (WorkDirT m) where
    type StM (WorkDirT m) a = ComposeSt WorkDirT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

