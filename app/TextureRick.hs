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


main :: IO ()
main = do

    (win, events) <- reacquire 0 $ createWindow "Tiny Rick" 1024 768

    texture <- loadTexture "textures/SightToSound-09.png" SRGB
    -- texture <- loadTexture "textures/SightToSound-01.png" SRGB
    -- texture <- loadTexture "textures/Clock-Black-01.png" SRGB

    glyphProg <- createShaderProgram "src/TinyRick/glyph.vert" "src/TinyRick/glyph.frag"
    font      <- createFont fontFile 30 glyphProg

    -- planeGeometry size normal up subdivisions
    planeGeo <- planeGeometry (V2 1 1) (V3 0 0 1) (V3 0 1 0) 1

    glClearColor 0.1 0.1 0.1 1
    glDisable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    
    getPlane <- shaderRecompiler "app/geo.vert" "app/shader-texture.frag" (makeShape planeGeo)

    startTime <- getNow
    whileWindow win $ 
        mainLoop win events getPlane texture startTime

getNow :: MonadIO m => m GLfloat
getNow = realToFrac . utctDayTime <$> liftIO getCurrentTime

mainLoop :: MonadIO m 
         => Window
         -> Events
         -> IO (Shape ShaderPlaneUniforms, t)
         -> TextureObject
         -> GLfloat
         -> m ()
mainLoop win events getShape texture startTime = do

    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    proj44 <- getWindowProjection win 45 0.01 1000

    -- Get mouse/keyboard/OS events from GLFW
    processEvents events $ \e -> do
        closeOnEscape win e
    
    
    -- Clear the framebuffer
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- Render our scene
    let view44 = viewMatrixFromPose newPose
        projView44   = proj44 !*! view44
    
    
    let model44      = transformationFromPose $ newPose & posPosition . _z .~ (-1)
        mvp          = projView44 !*! model44

    (shape, _shaderErrors) <- liftIO getShape

    -- Draw the Shader plane
    withShape shape $ do
        let ShaderPlaneUniforms{..} = sUniforms shape
        uniformM44 uMVP mvp
        -- Pass time uniform
        now <- getNow
        uniformF uTime (now - startTime)
        -- Pass texture uniform
        uniformI uTex 0
        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D (unTextureObject texture)
        drawShape

    
    swapBuffers win
