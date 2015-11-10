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
  , _trPath   :: FilePath
  }
makeLenses ''TinyRick

data AppState = AppState 
    { _appRicks        :: Map RickID TinyRick
    , _appActiveRickID :: RickID
    }
makeLenses ''AppState

newAppState :: AppState
newAppState = AppState { _appRicks = mempty, _appActiveRickID = 0 }

tinyRickFromFile :: MonadIO m => FilePath -> Pose GLfloat -> m TinyRick
tinyRickFromFile filePath pose = liftIO $ do
  text <- readFile filePath
  return TinyRick 
    { _trPose = pose
    , _trBuffer = bufferFromString text
    , _trPath = filePath
    }

saveTinyRick :: (Foldable t, MonadIO m) => t TinyRick -> m ()
saveTinyRick mTinyRick = forM_ mTinyRick $ \tinyRick -> do
  liftIO $ putStrLn $ "Saving " ++ (tinyRick ^. trPath) ++ "..."
  liftIO $ writeFile (tinyRick ^. trPath) (tinyRick ^. trBuffer . to stringFromBuffer)

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
        let z = (-11)
        tinyRick <- tinyRickFromFile filePath $ 
          newPose 
            & posPosition .~ (V3 (-8 + iF*5) 6 z)
            -- & posOrientation .~ axisAngle (V3 0 1 0) 0.5
        let rickID = i
        appRicks . at rickID ?= tinyRick
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
    -- glGetErrors

    -- Get mouse/keyboard/OS events from GLFW
    activeRickID <- use appActiveRickID
    let activeRick    = appRicks . ix activeRickID
        activeRickBuf = appRicks . ix activeRickID . trBuffer
    processEvents events $ \e -> do
        closeOnEscape win e

        superIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftSuper
        shiftIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftShift
        if 
            | superIsDown ->
                onKeyDown e Key'S      $ saveTinyRick =<< preuse activeRick
            | shiftIsDown -> do
                onKey e Key'Left       $ activeRickBuf %= selectLeft
                onKey e Key'Right      $ activeRickBuf %= selectRight
            | otherwise -> do
                onChar e $ \char      -> activeRickBuf %= insertChar char
                onKey  e Key'Backspace $ activeRickBuf %= backspace
                onKey  e Key'Enter     $ activeRickBuf %= insertChar '\n'
                onKey  e Key'Left      $ activeRickBuf %= moveLeft
                onKey  e Key'Right     $ activeRickBuf %= moveRight
                onKey  e Key'Down      $ activeRickBuf %= moveDown
                onKey  e Key'Up        $ activeRickBuf %= moveUp
                onKey  e Key'Tab       $ rotateActiveRick
                  

    immutably $ do
        -- Clear the framebuffer
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        -- Render our scene
        let view44       = viewMatrixFromPose newPose
        
        ricks        <- use appRicks
        forM_ (Map.toList ricks) $ \(rickID, rick) -> do
          let rot = if rickID /= activeRickID 
                then newPose & posOrientation .~ (axisAngle (V3 0 1 0) 0.5)
                else newPose
          let model44      = (transformationFromPose $ addPoses rot (rick ^. trPose))
                                !*! scaleMatrix 0.003
              mvp          = projection44 !*! view44 !*! model44
              buffer = rick ^. trBuffer
          renderText font (bufText buffer) (bufSelection buffer) mvp
        
        swapBuffers win



