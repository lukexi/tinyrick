{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module TinyRick.Recompiler2 where
import TinyRick.SubHalive
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad
import GHC
import Unsafe.Coerce

atomicallyIO :: MonadIO m => STM a -> m a
atomicallyIO = liftIO . atomically

readTChanIO :: MonadIO m => TChan a -> m a
readTChanIO = atomicallyIO . readTChan

writeTChanIO :: MonadIO m => TChan a -> a -> m ()
writeTChanIO chan = atomicallyIO . writeTChan chan

tryReadTChanIO :: MonadIO m => TChan a -> m (Maybe a)
tryReadTChanIO = atomicallyIO . tryReadTChan

data CompilationRequest = CompilationRequest
    { crFilePath         :: FilePath
    , crExpressionString :: String
    , crResultTChan      :: TChan (Either [String] HValue)
    }

startGHC :: MonadIO m => [FilePath] -> m (TChan CompilationRequest)
startGHC importPaths_ = liftIO $ do
    ghcChan <- newTChanIO

    _ <- forkOS . void . withGHCSession importPaths_ . forever $ do
        CompilationRequest{..} <- readTChanIO ghcChan
        
        result <- recompileTargets2 crFilePath crExpressionString
        writeTChanIO crResultTChan result
    return ghcChan

-- We defer the unsafeCoerce to the last moment to avoid making the CompilationRequest/ResultTChan polymorphic
tryReadResultTChanIO :: (Functor f, MonadIO m) => TChan (f a) -> m (Maybe (f b))
tryReadResultTChanIO resultTChan = (fmap (fmap unsafeCoerce)) <$> tryReadTChanIO resultTChan

recompilerForExpression :: MonadIO m => (TChan CompilationRequest) -> FilePath -> String -> m (TChan (Either [String] HValue))
recompilerForExpression ghcChan filePath expressionString = liftIO $ do

    resultTChan <- newTChanIO
    let compilationRequest = CompilationRequest 
            { crFilePath         = filePath
            , crExpressionString = expressionString 
            , crResultTChan      = resultTChan
            }

    fileEventListener <- eventListenerForFile filePath
    
    -- Compile immediately
    writeTChanIO ghcChan compilationRequest

    _ <- forkIO . forever $ do
        _ <- readChan fileEventListener
        writeTChanIO ghcChan compilationRequest

    return resultTChan
