{-# LANGUAGE RecordWildCards #-}
module TinyRick.Recompiler1 where

import Graphics.GL.Pal as Exports
import Graphics.GL.Freetype
import Graphics.GL.TextBuffer

import Control.Monad
import Control.Lens.Extra
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.DeepSeq

import TinyRick.SubHalive

{-
The arrangement of the recompiler as used in Cubensis.
-}

data CompilationRequest r = CompilationRequest
    { crFilePath         :: FilePath
    , crExpressionString :: String
    , crResultTVar       :: TVar r
    , crErrorsTVar       :: TVar [String]
    }

recompilerForExpression :: Chan (CompilationRequest t) -> FilePath -> String -> t -> IO (TVar t, TVar [String])
recompilerForExpression ghcChan filePath expressionString defaultValue = do

    resultTVar <- newTVarIO defaultValue
    errorsTVar <- newTVarIO []
    let compilationRequest = CompilationRequest 
            { crFilePath = filePath
            , crExpressionString = expressionString 
            , crResultTVar = resultTVar
            , crErrorsTVar = errorsTVar
            }

    fileEventListener <- eventListenerForFile filePath
    
    -- Compile immediately
    writeChan ghcChan compilationRequest

    -- 
    _ <- forkIO . forever $ do
        _ <- liftIO (readChan fileEventListener)
        writeChan ghcChan compilationRequest

    return (resultTVar, errorsTVar)

startGHC :: [FilePath] -> IO (Chan (CompilationRequest r))
startGHC importPaths_ = do
    ghcChan <- newChan

    _ <- forkOS . void . withGHCSession importPaths_ . forever $ do
        CompilationRequest{..} <- liftIO (readChan ghcChan)
        
        result <- recompileTargets crFilePath crExpressionString
        liftIO . atomically $ case result of
            Right validResult -> do
                let noErrors = []
                writeTVar crResultTVar validResult
                writeTVar crErrorsTVar noErrors
            -- If we get a failure, leave the old result alone so it can still be used
            -- until the errors are fixed.
            Left  newErrors   -> do
                writeTVar crErrorsTVar newErrors
    return ghcChan




data Editor a = Editor
    { edText     :: TVar TextRenderer
    , edErrors   :: TVar [String]
    , edExpr     :: TVar a
    , edDefault  :: a
    , edModelM44 :: M44 GLfloat
    }

makeExpressionEditor :: Chan (CompilationRequest e)
                     -> Font
                     -> FilePath
                     -> String
                     -> e
                     -> M44 GLfloat
                     -> IO (Editor e)
makeExpressionEditor ghcChan font fileName exprString defaultValue modelM44 = do
    textTVar              <- newTVarIO =<< textRendererFromFile font fileName
    (exprTVar, errorTVar) <- recompilerForExpression ghcChan fileName exprString defaultValue
  
    return (Editor textTVar errorTVar exprTVar defaultValue modelM44)

-- Evaluates the latest value of the Editor's expression with some transformation,
-- catching any exceptions that may occur
getEditorValue :: NFData b => Editor t -> b -> (t -> b) -> IO b
getEditorValue Editor{..} defaultValue transformValue = do
    -- Protect against runtime exceptions (e.g. divide by zero)
    value <- liftIO . atomically $ readTVar edExpr

    (checkedValue, runtimeErrors) <- handle
        (\e -> return (defaultValue, show (e::SomeException))) 
        (return $!! (transformValue value, ""))

    -- Update the errors list with any runtime errors
    when (not (null runtimeErrors)) $ liftIO . atomically $ do
        errors <- readTVar edErrors
        writeTVar edErrors (runtimeErrors:errors)

    return checkedValue

editEditorText :: Editor t -> (TextRenderer -> IO TextRenderer) -> IO ()
editEditorText Editor{..} f = do
  text <- atomically $ readTVar edText
  newText <- f text
  atomically $ writeTVar edText newText

renderEditor :: MonadIO m => Editor t -> M44 GLfloat -> m ()
renderEditor Editor{..} projViewM44 = do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    (errors, textRenderer) <- liftIO . atomically $ 
        (,) <$> readTVar edErrors <*> readTVar edText

    

    let textMVP = projViewM44 !*! edModelM44 -- !*! textM44
    renderText textRenderer textMVP (V3 1 1 1)

    let errorsMVP = projViewM44 
                    !*! edModelM44 
                    !*! (identity & translation .~ V3 1 0 0)
                    -- !*! textM44
    errorRenderer <- createTextRenderer (textRenderer ^. txrFont) (textBufferFromString "noFile" (unlines errors))
    renderText errorRenderer errorsMVP (V3 1 0.5 0.5)

    glDisable GL_BLEND
