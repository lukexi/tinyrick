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
import Data.IORef

import Halive.Utils

import TinyRick
import Data.Maybe

fontFile :: FilePath
fontFile = "fonts/SourceCodePro-Regular.ttf"

data ShaderPlaneUniforms = ShaderPlaneUniforms
  { uMVP2 :: UniformLocation (M44 GLfloat)
  , uTime :: UniformLocation GLfloat
  } deriving (Data)

type RickID = Int

data TinyRick = TinyRick
  { _trBuffer  :: Buffer
  , _trPose    :: Pose GLfloat
  , _trFont    :: Font
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

    glyphQuadProg <- createShaderProgram "src/TinyRick/glyphQuad.vert" "src/TinyRick/glyphQuad.frag"
    font          <- createFont fontFile 30 glyphQuadProg

    -- planeGeometry size normal up subdivisions
    planeGeo <- planeGeometry (V2 1 1) (V3 0 0 1) (V3 0 1 0) 1

    glClearColor 0.1 0.1 0.1 1
    glEnable GL_DEPTH_TEST
    glDisable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    
    let shaders = [ "app/shader.frag"
                  ]

    initialState <- reacquire 1 $ flip execStateT newAppState $ do
      -- Create an editor instance for each fragment shader
      forM_ (zip [0..] shaders) $ \(i, fragShaderPath) -> do
        let pose = newPose & posPosition .~ position
            -- position = V3 (-8 + fromIntegral i * 5) 6 (-11)
            position = (V3 0 0 (-1.1))
            vertShaderPath = "app/geo.vert"
        getPlane <- liftIO $ makeRecompiler vertShaderPath fragShaderPath planeGeo 

        buffer <- bufferFromFile fragShaderPath
        appRicks . at i ?= TinyRick buffer pose font getPlane 0

        appActiveRickID .= i

    void . flip runStateT initialState $ do

      whileWindow win $ 
        mainLoop win events 

makeRecompiler vertShaderPath fragShaderPath planeGeo  = do

  (shader, result) <- createShaderProgram' vertShaderPath fragShaderPath
  shape <- makeShape planeGeo shader
  shapeRef <- newIORef (shape, result)

  lookForChange <- watchFiles [vertShaderPath, fragShaderPath]

  return $ do
    lookForChange >>= \case
      Nothing -> return ()
      Just _ -> do
        (newShader, result) <- createShaderProgram' vertShaderPath fragShaderPath
        goodShape <- if null result 
          then makeShape planeGeo newShader 
          else fst <$> readIORef shapeRef
        writeIORef shapeRef (goodShape, result)

    readIORef shapeRef


mainLoop :: (MonadState AppState m, MonadIO m) => Window -> Events -> m ()
mainLoop win events = do
    persistState 1

    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    projection44 <- getWindowProjection win 45 0.01 1000

    -- Get mouse/keyboard/OS events from GLFW
    activeRickID <- use appActiveRickID
    processEvents events $ \e -> do
        closeOnEscape win e
        
        -- Switch which rick has focus on Tab
        onKey e Key'Tab rotateActiveRick

        -- Scroll the active rick
        onScroll e $ \_ y -> do
          appRicks . ix activeRickID . trScroll %= \s ->
            min 100 (max (-1000) (s + y))
          -- appRicks . ix activeRickID . trScroll .= 0

        -- Pass events to the active rickID
        handleBufferEvent win e (appRicks . ix activeRickID . trBuffer)

        -- Continuously save the file
        let save = do
              persistState 1
              maybe (return ()) saveBuffer =<< preuse (appRicks . ix activeRickID . trBuffer)
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
                                              else 0.0
              shift = V3 0 0 (-0.1)
              model44      = transformationFromPose . shiftBy shift . rotateBy rot $ rick ^. trPose
              mvp          = projView44 !*! model44
              planeMVP     = mvp !*! translateMatrix (V3 0.5 0 0)
              textMVP      = mvp !*! translateMatrix (V3 (-1) (0.5 - scroll*0.01 + 1) 0)
              projView44   = projection44 !*! view44
              buffer       = rick ^. trBuffer
              font         = rick ^. trFont
              scroll       = rick ^. trScroll

          (shape, shaderErrors) <- liftIO (rick ^. trShape)
          withShape shape $ do
            let ShaderPlaneUniforms{..} = sUniforms shape
            uniformM44 uMVP2 planeMVP
            -- Pass time uniform
            uniformF uTime =<< realToFrac . utctDayTime <$> liftIO getCurrentTime
            drawShape

          renderText font (bufText buffer) (bufSelection buffer) (textMVP !*! scaleMatrix 0.0005)
          when (not (null shaderErrors)) $ do
            renderText font shaderErrors (0,0) (planeMVP !*! translateMatrix (V3 (-0.5) 0.5 0) !*! scaleMatrix 0.0005)
        
        swapBuffers win

rotateActiveRick :: MonadState AppState m => m ()
rotateActiveRick = do
  activeRickID <- use appActiveRickID
  ricks        <- use appRicks
  let !lastIndex  = fromMaybe 0 $ Map.lookupIndex activeRickID ricks
      !newIndex   = (lastIndex + 1) `mod` Map.size ricks
      (!k, _)     = Map.elemAt newIndex ricks
  appActiveRickID .= k
