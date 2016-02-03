{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module TinyRick.SubHalive where

import GHC
import Linker
import Packages
import DynFlags
import Exception
import ErrUtils
import HscTypes
import GHC.Paths
import Outputable
import Unsafe.Coerce

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Control.Concurrent
import TinyRick.FindPackageDBs

import System.FSNotify
import System.Directory

fileModifiedPredicate :: FilePath -> Event -> Bool
fileModifiedPredicate fileName event = case event of
    Modified path _ -> path == fileName
    _               -> False

eventListenerForFile :: FilePath -> IO (Chan Event)
eventListenerForFile fileName = do
    fileNameCanon <- canonicalizePath fileName
    let predicate = fileModifiedPredicate fileNameCanon
    eventChan <- newChan
    _ <- forkIO . withManager $ \manager -> do
        let watchDirectory = "."
        _stop <- watchTreeChan manager watchDirectory predicate eventChan
        forever (threadDelay 10000000)
    return eventChan

-- Starts up a GHC session and then runs the given action within it
withGHCSession :: [FilePath] -> Ghc a -> IO a
withGHCSession importPaths_ action = do
    -- defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
    runGhc (Just libdir) $ do
        -- Get the default dynFlags
        dflags0 <- getSessionDynFlags
        
        -- If there's a sandbox, add its package DB
        dflags1 <- updateDynFlagsWithCabalSandbox dflags0

        -- If this is a stack project, add its package DBs
        dflags2 <- updateDynFlagsWithStackDB dflags1

        -- Make sure we're configured for live-reload
        let dflags3 = dflags2 { hscTarget   = HscInterpreted
                              , ghcLink     = LinkInMemory
                              , ghcMode     = CompManager
                              , importPaths = importPaths_
                              } 
                              -- turn off the GHCi sandbox
                              -- since it breaks OpenGL/GUI usage
                              `gopt_unset` Opt_GhciSandbox 
                              -- GHC seems to try to "debounce" compilations within
                              -- about a half second (i.e., it won't recompile) 
                              -- This fixes that, but probably isn't quite what we want
                              -- since it will cause extra files to be recompiled...
                              `gopt_set` Opt_ForceRecomp
        
        -- We must call setSessionDynFlags before calling initPackages or any other GHC API
        _ <- setSessionDynFlags dflags3

        -- Initialize the package database
        (dflags4, _) <- liftIO (initPackages dflags3)

        -- Initialize the dynamic linker
        liftIO (initDynLinker dflags4)

        

        action

-- See note below - this isn't actually called right now
gatherErrors :: GhcMonad m => SourceError -> m [String]
gatherErrors sourceError = do
    printException sourceError
    dflags <- getSessionDynFlags
    let errorSDocs = pprErrMsgBagWithLoc (srcErrorMessages sourceError)
        errorStrings = map (showSDoc dflags) errorSDocs
    return errorStrings


-- Recompiles the current targets
recompileTargets :: FilePath -> String -> Ghc (Either [String] a)
recompileTargets fileName expression = 
    -- NOTE: handleSourceError doesn't actually seem to do anything, and we use
    -- the IORef + log_action solution instead. The API docs claim 'load' should
    -- throw SourceErrors but it doesn't afaict.
    catchExceptions . handleSourceError (fmap Left . gatherErrors) $ do

        setTargets =<< sequence [guessTarget fileName Nothing]

        errorsRef <- liftIO (newIORef "")
        dflags <- getSessionDynFlags
        _ <- setSessionDynFlags dflags { log_action = logHandler errorsRef }

        -- Get the dependencies of the main target
        graph <- depanal [] False

        -- Reload the main target
        loadSuccess <- load LoadAllTargets

        if failed loadSuccess 
            then do
                errors <- liftIO (readIORef errorsRef)
                return (Left [errors])
            else do
                -- We must parse and typecheck modules before they'll be available for usage
                forM_ graph (typecheckModule <=< parseModule)
                
                -- Load the dependencies of the main target
                setContext (IIModule . ms_mod_name <$> graph)

                result <- unsafeCoerce <$> compileExpr expression

                return (Right result)

-- | This version returns the uncoerced HValue, which lets me send polymorphic values back through channels
-- in Recompiler2
recompileTargets2 :: FilePath -> String -> Ghc (Either [String] HValue)
recompileTargets2 fileName expression = 
    -- NOTE: handleSourceError doesn't actually seem to do anything, and we use
    -- the IORef + log_action solution instead. The API docs claim 'load' should
    -- throw SourceErrors but it doesn't afaict.
    catchExceptions . handleSourceError (fmap Left . gatherErrors) $ do

        setTargets =<< sequence [guessTarget fileName Nothing]

        errorsRef <- liftIO (newIORef "")
        dflags <- getSessionDynFlags
        _ <- setSessionDynFlags dflags { log_action = logHandler errorsRef }

        -- Get the dependencies of the main target
        graph <- depanal [] False

        -- Reload the main target
        loadSuccess <- load LoadAllTargets

        if failed loadSuccess 
            then do
                errors <- liftIO (readIORef errorsRef)
                return (Left [errors])
            else do
                -- We must parse and typecheck modules before they'll be available for usage
                forM_ graph (typecheckModule <=< parseModule)
                
                -- Load the dependencies of the main target
                setContext (IIModule . ms_mod_name <$> graph)

                result <- compileExpr expression

                return (Right result)

catchExceptions :: ExceptionMonad m => m (Either [String] a) -> m (Either [String] a)
catchExceptions a = gcatch a 
    (\(_x :: SomeException) -> do
        liftIO (putStrLn ("Caught exception during recompileTargets: " ++ show _x))
        return (Left [show _x]))


typecheckTargets :: GhcMonad m => m () -> m ()
typecheckTargets onFailure = handleSourceError (\e -> onFailure >> printException e) $ do
    -- Get the dependencies of the main target
    graph <- depanal [] False

    -- Reload the main target
    loadSuccess <- load LoadAllTargets
    if failed loadSuccess 
        then onFailure
        else 
            -- Parse and typecheck modules to trigger any SourceErrors therein
            forM_ graph (typecheckModule <=< parseModule)



-- A helper from interactive-diagrams to print out GHC API values, 
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx

logHandler :: IORef String -> LogAction
logHandler ref dflags severity srcSpan style msg =
  case severity of
     SevError   ->  modifyIORef' ref (++ ('\n':printDoc))
     SevFatal   ->  modifyIORef' ref (++ ('\n':printDoc))
     SevWarning ->  modifyIORef' ref (++ ('\n':printDoc))
     _          ->  return () -- ignore the rest
  where cntx = initSDocContext dflags style
        locMsg = mkLocMessage severity srcSpan msg
        printDoc = show (runSDoc locMsg cntx) 
