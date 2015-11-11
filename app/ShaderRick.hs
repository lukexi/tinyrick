{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
fontFile = "fonts/SourceCodePro-Regular.ttf"

data ShaderPlaneUniforms = ShaderPlaneUniforms
  { uMVP2 :: UniformLocation (M44 GLfloat)
  } deriving (Data)

type RickID = Int

data TinyRick = TinyRick
  { _trBuffer :: Buffer
  , _trPose   :: Pose GLfloat
  , _trFont   :: Font
  , _trShape  :: IO (Shape ShaderPlaneUniforms)
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

    -- planeGeometry size normal up subdivisions
    planeGeo <- planeGeometry (V2 1 1) (V3 0 0 1) (V3 0 1 0) 1

    glClearColor 0.1 0.1 0.1 1
    glEnable GL_DEPTH_TEST
    glDisable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    
    let shaders = [ "app/shader.frag"
                  ]

    void . flip runStateT newAppState $ do

      -- Create an editor instance for each fragment shader      
      forM_ (zip [0..] shaders) $ \(i, shaderPath) -> do
        let pose = newPose & posPosition .~ position
            -- position = V3 (-8 + fromIntegral i * 5) 6 (-11)
            position = (V3 0 0 (-1))

        getPlane <- liftIO $ withReshaderProgram "app/geo.vert" shaderPath $ makeShape planeGeo

        buffer <- bufferFromFile shaderPath
        appRicks . at i ?= TinyRick buffer pose font getPlane

        appActiveRickID .= i

      whileWindow win $ 
        mainLoop win events 



mainLoop :: (MonadState AppState m, MonadIO m) => Window -> Events -> m ()
mainLoop win events = do
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

        -- Continuously save the file
        let save = maybe (return ()) saveBuffer =<< preuse (appRicks . ix activeRickID . trBuffer)
        onChar e $ \_ -> save
        onKey  e Key'Enter     $ save
        onKey  e Key'Backspace $ save
    
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
              shift = V3 0 0 0
              model44      = transformationFromPose . shiftBy shift . rotateBy rot $ rick ^. trPose
              mvp          = projView44 !*! model44
              planeMVP     = mvp
              textMVP      = mvp !*! translateMatrix (V3 (-0.5) (0.5) 0)
              projView44   = projection44 !*! view44
              buffer       = rick ^. trBuffer
              font         = rick ^. trFont

          shape <- liftIO $ rick ^. trShape
          withShape shape $ do
            uniformM44 (uMVP2 (sUniforms shape)) planeMVP
            drawShape

          renderText font (bufText buffer) (bufSelection buffer) (textMVP !*! scaleMatrix 0.001)
        
        swapBuffers win

rotateActiveRick :: MonadState AppState m => m ()
rotateActiveRick = do
  activeRickID <- use appActiveRickID
  ricks        <- use appRicks
  let !lastIndex  = fromMaybe 0 $ Map.lookupIndex activeRickID ricks
      !newIndex   = (lastIndex + 1) `mod` Map.size ricks
      (!k, _)     = Map.elemAt newIndex ricks
  appActiveRickID .= k
