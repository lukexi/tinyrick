{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Freetype

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map

import Halive.Utils

import TinyRick

fontFile :: FilePath
fontFile = "fonts/SourceCodePro-Regular.ttf"
-- fontFile = "fonts/Vera.ttf"
-- fontFile = "fonts/Lobster-Regular.ttf"
-- fontFile = "fonts/LuckiestGuy.ttf"

type RickID = Int

data TinyRick = TinyRick
    { _trPose   :: Pose GLfloat
    , _trBuffer :: TextBuffer
    }
makeLenses ''TinyRick

data AppState = AppState 
    { _appRicks        :: Map RickID TinyRick
    , _appActiveRickID :: RickID
    }
makeLenses ''AppState

newAppState :: AppState
newAppState = AppState { _appRicks = mempty, _appActiveRickID = 0 }

main :: IO ()
main = do

    (win, events) <- reacquire 0 $ createWindow "Tiny Rick" 1024 768

    glyphQuadProg <- createShaderProgram "src/TinyRick/glyph.vert" "src/TinyRick/glyph.frag"
    font          <- createFont fontFile 30 glyphQuadProg

    glClearColor 0.1 0.1 0.1 1
    glEnable GL_DEPTH_TEST
    glDisable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    
    let files = [ "app/Main.hs"
                , "src/TinyRick/glyph.vert"
                , "src/TinyRick/glyph.frag"
                ]

    void . flip runStateT newAppState $ do
        forM_ (zip [0..] files) $ \(i, filePath) -> do
            let position = V3 (-8 + fromIntegral i * 5) 6 (-11)
                pose = newPose & posPosition .~ position
            buffer <- bufferFromFile font filePath
            appRicks . at i ?= TinyRick pose buffer
        whileWindow win $ mainLoop win events

mainLoop :: (MonadState AppState m, MonadIO m) => Window -> Events -> m ()
mainLoop win events = do
    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    proj44 <- getWindowProjection win 45 0.01 1000

    -- Get mouse/keyboard/OS events from GLFW
    activeRickID <- use appActiveRickID
    processEvents events $ \e -> do
        closeOnEscape win e
        
        -- Switch which rick has focus on Tab
        ricks <- use appRicks
        onKey e Key'Tab $ appActiveRickID %= (`mod` Map.size ricks) . succ

        -- Pass events to the active rickID
        handleTextBufferEvent win e (appRicks . ix activeRickID . trBuffer)
        maybeBuffer <- preuse (appRicks . ix activeRickID . trBuffer)
        forM_ maybeBuffer updateIndicesAndOffsets
    
    immutably $ do
        -- Clear the framebuffer
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        -- Render our scene
        let view44 = viewMatrixFromPose newPose
        
        ricks <- use appRicks
        forM_ (Map.toList ricks) $ \(rickID, rick) -> do
            let rot = axisAngle (V3 0 1 0) $ if   rickID /= activeRickID 
                                             then 0.5
                                             else 0
            let model44      = transformationFromPose (rotateBy rot (rick ^. trPose))
                mvp          = proj44 !*! view44 !*! model44
                buffer = rick ^. trBuffer
                font   = bufFont buffer
            renderText font (V3 1 1 1) (bufText buffer) mvp
        
        swapBuffers win

