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
  { _trBuffer  :: TextBuffer
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

    -- planeGeometry size normal up subdivisions
    planeGeo <- planeGeometry (V2 1 1) (V3 0 0 1) (V3 0 1 0) 1
    -- The vert shader to use for Shader planes
    let vertShaderPath = "app/geo.vert"

    cubeGeo      <- cubeGeometry (V3 1 1 1) (V3 1 1 1)
    getCubeShape <- shaderRecompiler vertShaderPath "app/shader-simple.frag" (makeShape cubeGeo)

    glClearColor 0.0 0.1 0.0 1
    glEnable GL_DEPTH_TEST
    --

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

            buffer   <- bufferFromFile font filePath
            appRicks . at i ?= TinyRick buffer pose getPlane 0


    -- showHandKeyboard vrPal 
    void . flip runStateT initialState . whileWindow gpWindow $ 
        mainLoop vrPal getCubeShape

correctionMatrixForFont :: Fractional a => Font -> M44 a
correctionMatrixForFont Font{..} = correctedMVP
  where
    -- Ensures the characters are always the same 
    -- size no matter what point size was specified
    resolutionCompensationScale = realToFrac (1 / fntPointSize / charWidth)
    -- Also scale by the width of a wide character
    charWidth = gmAdvanceX (glyMetrics (fntGlyphForChar '_'))
    correctedMVP = 
                    translateMatrix (V3 (-0.5) (0.5) 0) 
                !*! 
                    scaleMatrix resolutionCompensationScale

-- mainLoop :: (MonadState AppState m, MonadIO m) => VRPal -> m ()

mainLoop vrPal@VRPal{..} getCubeShape = do
    persistState 1

    (hands, _) <- getHands vrPal
    -- Get mouse/keyboard/OS events from GLFW
    activeRickID <- use appActiveRickID
    processEvents gpEvents $ \e -> do
        closeOnEscape gpWindow e
        ricks <- use appRicks

        -- Allow picking
        onMouseDown e $ \_ -> do
            winProj44 <- getWindowProjection gpWindow 45 0.1 1000
            ray <- cursorPosToWorldRay gpWindow winProj44 newPose
            let rayDir = directionFromRay ray :: V3 GLfloat
            forM_ (Map.toList ricks) $ \(rickID, rick) -> do
                let model44      = transformationFromPose (rick ^. trPose)
                    textModel44  = model44 !*! correctionMatrixForFont font
                    font         = bufFont buffer
                    buffer       = rick ^. trBuffer
                    aabb         = (0, V3 1 (-1) 0)
                    intersection = rayOBBIntersection ray aabb textModel44

                case intersection of
                    Nothing -> return ()
                    Just intersectionDistance -> do
                        let worldPoint = rayDir ^* intersectionDistance
                            modelPoint = worldPointToModelPoint textModel44 worldPoint
                            (_indices, offsets) = calculateIndicesAndOffsets buffer
                            (cursX, cursY) = (modelPoint ^. _x, modelPoint ^. _y)
                            hits = filter (\(_i, V2 x y) -> 
                                           cursX > x 
                                        && cursX < (x + fntPointSize font) 
                                        && cursY > y 
                                        && cursY < (y + fntPointSize font)) (zip [0..] offsets)
                            numChars = length offsets
                        forM_ hits $ \(i, _) -> do
                            let realIndex = numChars - i
                            appRicks . ix rickID . trBuffer %= \b ->
                                updateCurrentColumn (
                                    b {bufSelection = (realIndex, realIndex)}
                                    )
                            mapM_ updateIndicesAndOffsets =<< (preuse (appRicks . ix rickID . trBuffer))
                        printIO hits
                        putStrLnIO $ "World: " ++ show worldPoint
                        putStrLnIO $ "Model: " ++ show modelPoint
                printIO $ intersection

        
        -- Switch which rick has focus on Tab
        onKey e Key'Tab $ appActiveRickID %= (`mod` Map.size ricks) . succ

        -- Scroll the active rick
        onScroll e $ \_ scrollY -> do
          appRicks . ix activeRickID . trScroll %= \s ->
            min 100 (max (-1000) (s + scrollY))

        -- Pass events to the active rickID
        handleTextBufferEvent gpWindow e (appRicks . ix activeRickID . trBuffer)

        -- Continuously save the file
        let updateBuffer save = do
              maybeBuffer <- preuse (appRicks . ix activeRickID . trBuffer)

              forM_ maybeBuffer $ \buffer -> do 

                updateIndicesAndOffsets buffer
                when save $ saveTextBuffer buffer
        onChar e         $ \_ -> updateBuffer True
        onKey  e Key'Enter     $ updateBuffer True
        onKey  e Key'Backspace $ updateBuffer True
        onKey  e Key'Up        $ updateBuffer False
        onKey  e Key'Down      $ updateBuffer False
        onKey  e Key'Left      $ updateBuffer False
        onKey  e Key'Right     $ updateBuffer False
    
    let view44 = viewMatrixFromPose newPose

    immutably . renderWith vrPal view44 (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)) $ \proj44 eyeView44 -> do
        -- Render our scene
        let projView44   = proj44 !*! eyeView44
        
        (cubeShape,_) <- liftIO getCubeShape
        withShape cubeShape $
            forM_ hands $ \hand -> do
                let ShaderPlaneUniforms{..} = sUniforms cubeShape
                uniformM44 uMVP (projView44 !*! hand ^. hndMatrix !*! scaleMatrix 0.1)
                uniformF uTime =<< realToFrac . utctDayTime <$> liftIO getCurrentTime
                drawShape

        ricks <- use appRicks
        activeRickID <- use appActiveRickID
        forM_ (Map.toList ricks) $ \(rickID, rick) -> do
            let model44      = transformationFromPose (rick ^. trPose)
                mvp          = projView44 !*! model44
                font         = bufFont buffer
                buffer       = rick ^. trBuffer
                _scroll      = rick ^. trScroll

            case rick ^. trShape of
                Just getShape -> do 
                    (shape, shaderErrors) <- liftIO getShape

                    -- Draw the Shader plane
                    withShape shape $ do
                        let ShaderPlaneUniforms{..} = sUniforms shape
                        uniformM44 uMVP mvp
                        -- Pass time uniform
                        uniformF uTime =<< realToFrac . utctDayTime <$> liftIO getCurrentTime
                        drawShape

                    when (not (null shaderErrors)) $
                        renderText' font (V3 1 0.5 0.5) (bufText buffer) (mvp !*! translateMatrix (V3 1 0 0))
                Nothing -> return ()

            -- Draw the source code
            let color = if rickID == activeRickID then V3 1 1 1 else V3 0.5 0.5 0.5
            renderText' font color (bufText buffer) (mvp !*! translateMatrix (V3 0 (-1) 0))


renderText' :: (Foldable f, MonadIO m) 
            => Font -> V3 GLfloat -> f Char -> M44 GLfloat -> m ()
renderText' font@Font{..} color string mvp = do
    useProgram fntShader
    glBindTexture GL_TEXTURE_2D (unTextureID fntTextureID)

    let GlyphUniforms{..} = fntUniforms
        correctedMVP = mvp !*! correctionMatrixForFont font
                           
    uniformM44 uMVP     correctedMVP
    uniformI   uTexture 0
    uniformV3  uColor   color


    let numVertices  = 4
        -- Add 1 to ensure we still render the cursor
        numInstances = fromIntegral (length string + 1)
    withVAO fntVAO $ 
      glDrawArraysInstanced GL_TRIANGLE_STRIP 0 numVertices numInstances
    return ()
