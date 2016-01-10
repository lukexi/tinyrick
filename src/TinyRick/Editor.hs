{-# LANGUAGE RecordWildCards #-}
module TinyRick.Editor where

import Graphics.GL.Pal as Exports
import TinyRick.Render
import Graphics.GL.Freetype

import Control.Monad
import Control.Lens.Extra
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.DeepSeq

import TinyRick.SubHalive

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
makeExpressionEditor ghcChan font fileName expr defaultValue modelM44 = do
    textTVar              <- newTVarIO =<< textRendererFromFile font fileName
    (funcTVar, errorTVar) <- recompilerForExpression ghcChan fileName expr defaultValue
  
    return (Editor textTVar errorTVar funcTVar defaultValue modelM44)

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
