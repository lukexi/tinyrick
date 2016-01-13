{-# LANGUAGE RecordWildCards #-}
module TinyRick.Recompiler2 where
import TinyRick.SubHalive
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad

atomicallyIO :: MonadIO m => STM a -> m a
atomicallyIO = liftIO . atomically
readTChanIO = atomicallyIO . readTChan
writeTChanIO chan = atomicallyIO . writeTChan chan
tryReadTChanIO chan = atomicallyIO . tryReadTChan

data CompilationRequest r = CompilationRequest
    { crFilePath         :: FilePath
    , crExpressionString :: String
    , crResultTChan      :: TChan (Either [String] r)
    }

startGHC :: MonadIO m => [FilePath] -> m (TChan (CompilationRequest r))
startGHC importPaths_ = liftIO $ do
    ghcChan <- newTChanIO

    _ <- forkOS . void . withGHCSession importPaths_ . forever $ do
        CompilationRequest{..} <- readTChanIO ghcChan
        
        result <- recompileTargets crFilePath crExpressionString
        writeTChanIO crResultTChan result
    return ghcChan


recompilerForExpression :: MonadIO m => TChan (CompilationRequest r) -> FilePath -> String -> m (TChan (Either [String] r))
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
