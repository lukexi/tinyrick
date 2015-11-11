{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module TinyRick.Render where

import Graphics.UI.GLFW.Pal

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State

import TinyRick.Buffer


bufferFromFile :: MonadIO m => FilePath -> m Buffer
bufferFromFile filePath = liftIO $ do
  text <- readFile filePath
  return $ bufferFromString filePath text

saveBuffer :: (MonadIO m) => Buffer -> m ()
saveBuffer buffer = do
  liftIO $ putStrLn $ "Saving " ++ bufPath buffer ++ "..."
  liftIO $ writeFile (bufPath buffer) (stringFromBuffer buffer)

handleBufferEvent :: (MonadState s m, MonadIO m) => Window -> Event -> (Traversal' s Buffer) -> m ()
handleBufferEvent win e bufferLens = do
  superIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftSuper
  -- shiftIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftShift
  if  | superIsDown -> do
          onKeyDown e Key'S      $ maybe (return ()) saveBuffer =<< preuse bufferLens
          onKeyDown e Key'C      $ do
            mBuffer <- preuse bufferLens
            forM_ mBuffer $ \buffer -> 
              setClipboardString win (selectionFromBuffer buffer)
          onKeyDown e Key'V      $ do
            mString <- getClipboardString win
            forM_ mString $ \string -> 
              bufferLens %= insertString string
      | otherwise -> do

          onChar e $ \char      -> bufferLens %= insertChar char
          onKey  e Key'Enter     $ bufferLens %= insertChar '\n'
          onKey  e Key'Backspace $ bufferLens %= backspace

          onKey  e Key'Left      $ bufferLens %= moveLeft
          onKey  e Key'Right     $ bufferLens %= moveRight
          onKey  e Key'Down      $ bufferLens %= moveDown
          onKey  e Key'Up        $ bufferLens %= moveUp

          onKeyWithMods e [ModKeyShift] Key'Left  $ bufferLens %= selectLeft
          onKeyWithMods e [ModKeyShift] Key'Right $ bufferLens %= selectRight
