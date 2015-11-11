{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module TinyRick.Render where

import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Freetype

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State
import Halive.Utils

import TinyRick.Buffer

type RickID = Int

data TinyRick = TinyRick
  { _trPose   :: Pose GLfloat
  , _trBuffer :: Buffer
  , _trPath   :: FilePath
  }
makeLenses ''TinyRick

tinyRickFromFile :: MonadIO m => FilePath -> Pose GLfloat -> m TinyRick
tinyRickFromFile filePath pose = liftIO $ do
  text <- readFile filePath
  return TinyRick 
    { _trPose = pose
    , _trBuffer = bufferFromString text
    , _trPath = filePath
    }

saveTinyRick :: (MonadIO m) => TinyRick -> m ()
saveTinyRick tinyRick = do
  liftIO $ putStrLn $ "Saving " ++ (tinyRick ^. trPath) ++ "..."
  liftIO $ writeFile (tinyRick ^. trPath) (tinyRick ^. trBuffer . to stringFromBuffer)

handleTinyRickEvent :: (MonadState s m, MonadIO m) => Window -> Event -> (Traversal' s TinyRick) -> m ()
handleTinyRickEvent win e rickLens = do
  superIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftSuper
  -- shiftIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftShift
  if  | superIsDown -> do
          onKeyDown e Key'S      $ maybe (return ()) saveTinyRick =<< preuse rickLens
          onKeyDown e Key'C      $ do
            mTinyRick <- preuse rickLens
            forM_ mTinyRick $ \tinyRick -> 
              setClipboardString win (selectionFromBuffer (tinyRick ^. trBuffer))
          onKeyDown e Key'V      $ do
            mString <- getClipboardString win
            forM_ mString $ \string -> 
              rickLens . trBuffer %= insertString string
      | otherwise -> do

          onChar e $ \char      -> rickLens . trBuffer %= insertChar char
          onKey  e Key'Enter     $ rickLens . trBuffer %= insertChar '\n'
          onKey  e Key'Backspace $ rickLens . trBuffer %= backspace

          onKey  e Key'Left      $ rickLens . trBuffer %= moveLeft
          onKey  e Key'Right     $ rickLens . trBuffer %= moveRight
          onKey  e Key'Down      $ rickLens . trBuffer %= moveDown
          onKey  e Key'Up        $ rickLens . trBuffer %= moveUp

          onKeyWithMods e [ModKeyShift] Key'Left  $ rickLens . trBuffer %= selectLeft
          onKeyWithMods e [ModKeyShift] Key'Right $ rickLens . trBuffer %= selectRight
