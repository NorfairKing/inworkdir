{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Monad.InWorkDir
  ( MonadInWorkDir(..)
  , workDirInIO
  , inWorkDirT
  , runWorkDirT
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Base
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Identity
import           System.Directory             (getCurrentDirectory,
                                               setCurrentDirectory)

class Monad m => MonadInWorkDir m where
    -- | Get the currently stored working directory
    getWorkDir  :: m FilePath

    -- | Set the currently stored working directory such that subsequent calls
    -- to 'getWorkDir' will return the given filepath
    setWorkDir  :: FilePath -> m ()

    -- | Change the working directory so that calls to 'getWorkdir' in the
    -- second argument will return the given filepath (unless it is changed
    -- again there).
    withWorkDir :: FilePath -> m a -> m a
    withWorkDir wd action = do
        before <- getWorkDir
        setWorkDir wd
        result <- action
        setWorkDir before
        return result

newtype WorkDirT m a = WorkDirT { unWorkDirT :: StateT FilePath m a }
   deriving (Functor, Applicative, Monad, MonadTrans)

-- | Run a 'WorkDirT' in IO by initialising with the current working and
-- finalising by setting the working directory at the end.
workDirInIO :: MonadIO io => WorkDirT io a -> io a
workDirInIO func = do
    wd <- liftIO getCurrentDirectory
    (result, wdAfter) <- runWorkDirT func wd
    liftIO $ setCurrentDirectory wdAfter
    return result

inWorkDirT :: Functor m => FilePath -> WorkDirT m a -> m a
inWorkDirT wd func = fst <$> runWorkDirT func wd

runWorkDirT :: WorkDirT m a -> FilePath -> m (a, FilePath)
runWorkDirT (WorkDirT sa) = runStateT sa

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
    type StT WorkDirT a = StT (StateT FilePath) a
    liftWith = defaultLiftWith WorkDirT unWorkDirT
    restoreT = defaultRestoreT WorkDirT

instance MonadBaseControl b m => MonadBaseControl b (WorkDirT m) where
    type StM (WorkDirT m) a = ComposeSt WorkDirT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

