{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module TinyRick.TextInput where

import Graphics.UI.GLFW.Pal

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State

import Graphics.GL.Freetype
import Data.Char

-- | Recognize certain control characters and react to them appropriately
isBackspaceChar :: Char -> Bool
isBackspaceChar = (== 8) . ord

textRendererFromFile :: MonadIO m => Font -> FilePath -> m TextRenderer
textRendererFromFile font filePath = liftIO $ do
    text <- readFile filePath
    createTextRenderer font (textBufferFromString filePath text)

saveTextBuffer :: (MonadIO m) => TextBuffer -> m ()
saveTextBuffer buffer = do
    liftIO $ putStrLn $ "Saving " ++ bufPath buffer ++ "..."
    liftIO $ writeFile (bufPath buffer) (stringFromTextBuffer buffer)

handleTextBufferEvent :: forall s m. (MonadState s m, MonadIO m) 
                      => Window -> Event -> (Traversal' s TextRenderer) -> m ()
handleTextBufferEvent win e rendererLens = do
    -- let bufferLens = rendererLens . txrTextBuffer :: (Traversal' s TextBuffer)
    superIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftSuper
    -- shiftIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftShift
    if  | superIsDown -> do
            onKeyDown e Key'S      $ maybe (return ()) saveTextBuffer =<< preuse (rendererLens . txrTextBuffer)
            onKeyDown e Key'C      $ do
                mTextBuffer <- preuse (rendererLens . txrTextBuffer)
                forM_ mTextBuffer $ \buffer -> 
                    setClipboardString win (selectionFromTextBuffer buffer)
            onKeyDown e Key'V      $ do
                mString <- getClipboardString win
                forM_ mString $ \string -> 
                    (rendererLens . txrTextBuffer) %= insertString string
            onKeyDown e Key'Z      $ 
                (rendererLens . txrTextBuffer) %= undo
        | otherwise -> do

            onChar e $ \case 
                (isBackspaceChar -> True) -> (rendererLens . txrTextBuffer) %= backspace
                char                      -> (rendererLens . txrTextBuffer) %= insertChar char
            onKey  e Key'Enter     $ (rendererLens . txrTextBuffer) %= insertChar '\n'
            onKey  e Key'Backspace $ (rendererLens . txrTextBuffer) %= backspace

            onKey  e Key'Left      $ (rendererLens . txrTextBuffer) %= moveLeft
            onKey  e Key'Right     $ (rendererLens . txrTextBuffer) %= moveRight
            onKey  e Key'Down      $ (rendererLens . txrTextBuffer) %= moveDown
            onKey  e Key'Up        $ (rendererLens . txrTextBuffer) %= moveUp

            onKeyWithMods e [ModKeyShift] Key'Left  $ (rendererLens . txrTextBuffer) %= selectLeft
            onKeyWithMods e [ModKeyShift] Key'Right $ (rendererLens . txrTextBuffer) %= selectRight

    -- Continuously save the file
    let updateBuffer save = do
          maybeRenderer <- preuse rendererLens

          forM_ maybeRenderer $ \renderer -> do 

            newRenderer <- updateMetrics renderer
            rendererLens .= newRenderer
            when save $ saveTextBuffer (newRenderer ^. txrTextBuffer)
    onChar e         $ \_ -> updateBuffer True
    onKey  e Key'Enter     $ updateBuffer True
    onKey  e Key'Backspace $ updateBuffer True
    onKey  e Key'Up        $ updateBuffer False
    onKey  e Key'Down      $ updateBuffer False
    onKey  e Key'Left      $ updateBuffer False
    onKey  e Key'Right     $ updateBuffer False
    onKeyWithMods e [ModKeyShift] Key'Left $ updateBuffer False
    onKeyWithMods e [ModKeyShift] Key'Right $ updateBuffer False
    onKeyWithMods e [ModKeySuper] Key'Z $ updateBuffer True
    onKeyWithMods e [ModKeySuper] Key'V $ updateBuffer True
