{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Graphics.GL.Pal
import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Freetype

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import System.FilePath
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
  { _trRenderer  :: TextRenderer
  , _trPose    :: Pose GLfloat
  , _trShape   :: Maybe (IO (Shape ShaderPlaneUniforms, String))
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

    vrPal@VRPal{..} <- reacquire 0 $ initVRPal "VR Rick" [UseOpenVR]

    glyphProg <- createShaderProgram "src/TinyRick/glyph.vert" "src/TinyRick/glyph.frag"
    font      <- createFont fontFile 50 glyphProg

    --                        size     normal     up         subdivisions
    planeGeo <- planeGeometry (V2 1 1) (V3 0 0 1) (V3 0 1 0) 1

    -- The vert shader to use for Shader planes
    let vertShaderPath = "app/geo.vert"

    cubeGeo      <- cubeGeometry (V3 1 1 1) (V3 1 1 1)
    getCubeShape <- shaderRecompiler vertShaderPath "app/shader-simple.frag" (makeShape cubeGeo)

    glClearColor 0.0 0.1 0.0 1
    glEnable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    
    let shaders = [ "app/shader-simple.frag"
                  , "app/VRRick.hs"
                  ]

    initialState <- reacquire 1 $ flip execStateT newAppState $ do
    -- initialState <- flip execStateT newAppState $ do
        -- Create an editor instance for each fragment shader
        forM_ (zip [0..] shaders) $ \(i, filePath) -> do
            
            
            (y, getPlane) <- if takeExtension filePath == ".frag"
                then (1,) . Just <$> shaderRecompiler vertShaderPath filePath (makeShape planeGeo)
                else return (7, Nothing)

            let position = V3 (fromIntegral i + 1) y (-0.5)
                pose     = newPose & posPosition .~ position

            buffer   <- textRendererFromFile font filePath
            appRicks . at i ?= TinyRick buffer pose getPlane 0


    -- showHandKeyboard vrPal 
    void . flip runStateT initialState . whileVR vrPal $ 
        mainLoop vrPal getCubeShape



mainLoop :: (MonadState AppState m, MonadIO m) 
         => VRPal 
         -> IO (Shape ShaderPlaneUniforms, String) 
         -> M44 GLfloat
         -> [Hand]
         -> m ()
mainLoop vrPal@VRPal{..} getCubeShape headM44 hands = do
    persistState 1

    -- Get mouse/keyboard/OS events from GLFW
    activeRickID <- use appActiveRickID
    processEvents gpEvents $ \e -> do
        closeOnEscape gpWindow e
        ricks <- use appRicks

        -- Allow picking
        onMouseDown e $ \_ -> do
            winProj44 <- getWindowProjection gpWindow 45 0.1 1000
            ray <- cursorPosToWorldRay gpWindow winProj44 newPose
            forM_ (Map.toList ricks) $ \(rickID, rick) -> do
                let model44 = transformationFromPose (rick ^. trPose)
                updatedBuffer <- castRayToBuffer ray (rick ^. trRenderer) model44
                appRicks . ix rickID . trRenderer .= updatedBuffer

        
        -- Switch which rick has focus on Tab
        onKey e Key'Tab $ appActiveRickID %= (`mod` Map.size ricks) . succ

        -- Scroll the active rick
        onScroll e $ \_ scrollY -> 
            appRicks . ix activeRickID . trScroll %= \s ->
                min 0 (max (-1000) (s + scrollY))

        -- Pass events to the active rickID
        handleTextBufferEvent gpWindow e (appRicks . ix activeRickID . trRenderer)
    
    let player = newPose

    immutably . renderWith vrPal player headM44 (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)) $ \proj44 eyeView44 -> do
        -- Render our scene
        let projView44   = proj44 !*! eyeView44
        now <- realToFrac . utctDayTime <$> liftIO getCurrentTime
        
        (cubeShape,_) <- liftIO getCubeShape
        withShape cubeShape $
            forM_ hands $ \hand -> do
                let ShaderPlaneUniforms{..} = sUniforms cubeShape
                uniformM44 uMVP (projView44 !*! hand ^. hndMatrix !*! scaleMatrix 0.1)
                uniformF uTime now
                drawShape

        ricks <- use appRicks
        activeRickID <- use appActiveRickID
        forM_ (Map.toList ricks) $ \(rickID, rick) -> do
            let model44      = transformationFromPose (rick ^. trPose)
                mvp          = projView44 !*! model44
                font         = rick ^. trRenderer . txrFont
                buffer       = rick ^. trRenderer . txrTextBuffer
                _scroll      = rick ^. trScroll

            case rick ^. trShape of
                Just getShape -> do 
                    (shape, shaderErrors) <- liftIO getShape

                    -- Draw the Shader plane
                    withShape shape $ do
                        let ShaderPlaneUniforms{..} = sUniforms shape
                        uniformM44 uMVP mvp
                        uniformF uTime now
                        drawShape

                    when (not (null shaderErrors)) $ do
                        renderer <- liftIO $ createTextRenderer font (textBufferFromString "" shaderErrors)
                        renderText renderer (mvp !*! translateMatrix (V3 1 0 0)) (V3 1 0.5 0.5)
                Nothing -> return ()

            -- Draw the source code
            let color = if rickID == activeRickID then V3 1 1 1 else V3 0.5 0.5 0.5
            renderText (rick ^. trRenderer) (mvp !*! translateMatrix (V3 0 (-1) 0)) color
