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
import Data.Maybe

fontFile :: FilePath
-- fontFile = "fonts/Vera.ttf"
-- fontFile = "fonts/Lobster-Regular.ttf"
-- fontFile = "fonts/LuckiestGuy.ttf"
fontFile = "fonts/SourceCodePro-Regular.ttf"

type RickID = Int

data TinyRick = TinyRick
  { _trPose   :: Pose GLfloat
  , _trBuffer :: Buffer
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

    glyphQuadProg <- createShaderProgram "src/TinyRick/glyphQuad.vert" "src/TinyRick/glyphQuad.frag"
    font          <- makeGlyphs fontFile 30 glyphQuadProg

    glClearColor 0.1 0.1 0.1 1
    glEnable GL_DEPTH_TEST
    glDisable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    
    let files = [ "app/Main.hs"
                , "src/TinyRick/glyphQuad.vert"
                , "src/TinyRick/glyphQuad.frag"
                ]

    void . flip runStateT newAppState $ do
      forM_ (zip [0..] files) $ \(i, filePath) -> do
        let iF = fromIntegral i
            rickID = i
            position = V3 (-8 + iF*5) 6 (-11)
            pose = newPose & posPosition .~ position
        buffer <- bufferFromFile filePath
        appRicks . at rickID ?= TinyRick pose buffer
        appActiveRickID .= rickID
      whileWindow win $ mainLoop win events font 

rotateActiveRick :: MonadState AppState m => m ()
rotateActiveRick = do
  activeRickID <- use appActiveRickID
  ricks        <- use appRicks
  let !lastIndex  = fromMaybe 0 $ Map.lookupIndex activeRickID ricks
      !newIndex   = (lastIndex + 1) `mod` Map.size ricks
      (!k, _)     = Map.elemAt newIndex ricks
  appActiveRickID .= k

mainLoop :: (MonadState AppState m, MonadIO m) => Window -> Events -> Font -> m ()
mainLoop win events font = do
    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    projection44 <- getWindowProjection win 45 0.01 1000

    -- Get mouse/keyboard/OS events from GLFW
    activeRickID <- use appActiveRickID
    processEvents events $ \e -> do
        closeOnEscape win e
        
        -- Switch which rick has focus on Tab
        onKey e Key'Tab rotateActiveRick

        -- Pass events to the active rickID
        handleBufferEvent win e (appRicks . ix activeRickID . trBuffer)
    
    immutably $ do
        -- Clear the framebuffer
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        -- Render our scene
        let view44 = viewMatrixFromPose newPose
        
        ricks <- use appRicks
        forM_ (Map.toList ricks) $ \(rickID, rick) -> do
          let rot = axisAngle (V3 0 1 0) $ if rickID /= activeRickID 
                                              then 0.5
                                              else 0
          let model44      = (transformationFromPose (rotateBy rot (rick ^. trPose)))
                                !*! scaleMatrix 0.003
              mvp          = projection44 !*! view44 !*! model44
              buffer = rick ^. trBuffer
          renderText font (bufText buffer) (bufSelection buffer) mvp
        
        swapBuffers win

