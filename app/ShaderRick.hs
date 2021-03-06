{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Freetype

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Time

import Halive.Utils

import TinyRick

fontFile :: FilePath
fontFile = "fonts/SourceCodePro-Regular.ttf"

data ShaderPlaneUniforms = ShaderPlaneUniforms
  { uMVP  :: UniformLocation (M44 GLfloat)
  , uTime :: UniformLocation GLfloat
  , uTex  :: UniformLocation GLint
  } deriving (Data)

type RickID = Int

data TinyRick = TinyRick
  { _trBuffer  :: TextBuffer
  , _trPose    :: Pose GLfloat
  , _trShape   :: IO (Shape ShaderPlaneUniforms, String)
  , _trScroll  :: GLfloat
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

    glyphProg <- createShaderProgram "app/glyph.vert" "app/glyph.frag"
    font      <- createFont fontFile 30 glyphProg

    -- planeGeometry size normal up subdivisions
    planeGeo <- planeGeometry (V2 1 1) (V3 0 0 1) (V3 0 1 0) 1

    glClearColor 0.1 0.1 0.1 1
    glEnable GL_DEPTH_TEST
    glDisable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    
    let shaders = [ "app/shader-simple.frag"
                  ]

    initialState <- reacquire 1 $ flip execStateT newAppState $ do
      -- Create an editor instance for each fragment shader
      forM_ (zip [0..] shaders) $ \(i, fragShaderPath) -> do
        let pose = newPose & posPosition .~ position
            position = (V3 0 0 (-11))
            vertShaderPath = "app/geo.vert"
        getPlane <- shaderRecompiler vertShaderPath fragShaderPath (makeShape planeGeo)

        buffer <- bufferFromFile font fragShaderPath
        appRicks . at i ?= TinyRick buffer pose getPlane 0

    void . flip runStateT initialState $ do

      whileWindow win $ 
        mainLoop win events



mainLoop :: (MonadState AppState m, MonadIO m) => Window -> Events -> m ()
mainLoop win events = do
    persistState 1

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

        -- Scroll the active rick
        onScroll e $ \_ scrollY -> do
          appRicks . ix activeRickID . trScroll %= \s ->
            min 100 (max (-1000) (s + scrollY))
          -- appRicks . ix activeRickID . trScroll .= 0

        -- Pass events to the active rickID
        handleTextBufferEvent win e (appRicks . ix activeRickID . trBuffer)

        -- Continuously save the file
        let save = do
              persistState 1
              maybeBuffer <- preuse (appRicks . ix activeRickID . trBuffer)
              forM_ maybeBuffer $ \buffer -> do 
                updateIndicesAndOffsets buffer
                saveTextBuffer          buffer
        onChar e $ \_ -> save
        onKey  e Key'Enter     $ save
        onKey  e Key'Backspace $ save
        onKey  e Key'Up        $ save
        onKey  e Key'Down      $ save
        onKey  e Key'Left      $ save
        onKey  e Key'Right     $ save
    
    immutably $ do
        -- Clear the framebuffer
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        -- Render our scene
        let view44 = viewMatrixFromPose newPose
            projView44   = proj44 !*! view44
        
        ricks <- use appRicks
        forM_ (Map.toList ricks) $ \(rickID, rick) -> do
            let shift        = V3 0 0 (-0.1)
                model44      = transformationFromPose . shiftBy shift $ rick ^. trPose
                mvp          = projView44 !*! model44
                planeMVP     = mvp !*! translateMatrix (V3 0.5 0 10)
                textMVP      = mvp !*! translateMatrix (V3 (-8) (0.5 - scroll*0.01 + 1) 0)
                font         = bufFont buffer
                buffer       = rick ^. trBuffer
                scroll       = rick ^. trScroll

            (shape, shaderErrors) <- liftIO (rick ^. trShape)

            -- Draw the Shader plane
            withShape shape $ do
                let ShaderPlaneUniforms{..} = sUniforms shape
                uniformM44 uMVP planeMVP
                -- Pass time uniform
                uniformF uTime =<< realToFrac . utctDayTime <$> liftIO getCurrentTime
                drawShape

            -- Draw the source code
            renderText font (V3 1 1 1) (bufText buffer) textMVP

            -- Draw any errors
            when (not (null shaderErrors)) $ do

                renderText font (V3 1 0.5 0.5) shaderErrors (planeMVP !*! translateMatrix (V3 (-1) 0.5 0) !*! scaleMatrix 0.1)
        
        swapBuffers win
