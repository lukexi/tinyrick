{-# LANGUAGE LambdaCase #-}
module ShaderRick where

import Graphics.GL.Pal
import Data.IORef
import Control.Monad.Trans

shaderRecompiler :: MonadIO m => FilePath -> FilePath -> (Program -> IO r) -> m (IO (r, String))
shaderRecompiler vertShaderPath fragShaderPath makeResult = liftIO $ do

  (shader, anyError) <- createShaderProgram' vertShaderPath fragShaderPath
  result          <- makeResult shader
  resultRef       <- newIORef (result, anyError)

  lookForChange <- watchFiles [vertShaderPath, fragShaderPath]

  return $ do
    lookForChange >>= \case
      Nothing -> return ()
      Just _ -> do
        (newShader, newError) <- createShaderProgram' vertShaderPath fragShaderPath
        goodResult <- if null newError 
          then makeResult newShader
          else fst <$> readIORef resultRef
        writeIORef resultRef (goodResult, newError)

    readIORef resultRef
