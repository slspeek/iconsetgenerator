{-# LANGUAGE DeriveDataTypeable, CPP #-}
module IconsetGenerator.CmdLine
       (multiMain
       , Cairo
       ) where

import IconsetGenerator.Icons
import Data.List (intercalate)
import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.Cairo

-- Below hack is needed because GHC 7.0.x has a bug regarding export
-- of data family constructors; see comments in Diagrams.Backend.Cairo
#if __GLASGOW_HASKELL__ < 702 || __GLASGOW_HASKELL__ >= 704
import Diagrams.Backend.Cairo.Internal
#endif

import System.Console.CmdArgs.Implicit hiding (args)

import Prelude hiding      (catch)

import Data.Maybe          (fromMaybe)
import Control.Monad       (when, forM_, mplus)
import Data.List.Split

import Text.Printf

import System.Environment  (getArgs, getProgName)
import System.Directory    (getModificationTime)
import System.FilePath     (addExtension, splitExtension)
import System.Process      (runProcess, waitForProcess)
import System.IO           (openFile, hClose, IOMode(..),
                            hSetBuffering, BufferMode(..), stdout)
import System.Exit         (ExitCode(..))
import Control.Concurrent  (threadDelay)
import Control.Exception   (catch, SomeException(..), bracket)

#ifdef CMDLINELOOP
import System.Posix.Process (executeFile)
#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock (UTCTime,getCurrentTime)
type ModuleTime = UTCTime
getModuleTime :: IO  ModuleTime
getModuleTime = getCurrentTime
#else
import System.Time         (ClockTime, getClockTime)
type ModuleTime = ClockTime
getModuleTime :: IO  ModuleTime
getModuleTime = getClockTime
#endif
#endif

data DiagramOpts = DiagramOpts
                   { width     :: Maybe Int
                   , height    :: Maybe Int
                   , maincolor :: String
                   , bgcolor   :: String
                   , linecolor :: String
                   , onbackground :: Bool
                   , shadow :: Bool
                   , output    :: FilePath
                   , list      :: Bool
                   , icon :: Maybe String
                   , fpu       :: Double
#ifdef CMDLINELOOP
                   , loop      :: Bool
                   , src       :: Maybe String
                   , interval  :: Int
#endif
                   }
  deriving (Show, Data, Typeable)

diagramOpts :: String -> DiagramOpts
diagramOpts prog  = DiagramOpts
  { width =  def
             &= typ "INT"
             &= help "Desired width of the output image"

  , height = def
             &= typ "INT"
             &= help "Desired height of the output image"

  , maincolor = def
                &= typ "NAME"
                &= help "Desired foreground color as a six digit hexadecimal string"

  , bgcolor = def
                &= typ "NAME"
                &= help "Desired background color as a six digit hexadecimal string"

  , linecolor = def
                &= typ "NAME"
                &= help "Desired line color as a six digit hexadecimal string"

  , onbackground = def
         &= help "Draw the icon on a background"

  , shadow = def
         &= help "Use a shadowing effect"

  , output = def
           &= typFile
           &= help "Output file"

  , icon = def
              &= help "Name of the diagram to render"
              &= typ "NAME"

  , list = def
         &= help "List all available diagrams"

  , fpu = 30
          &= typ "FLOAT"
          &= help "Frames per unit time (for animations)"
#ifdef CMDLINELOOP
  , loop = False
            &= help "Run in a self-recompiling loop"
  , src  = def
            &= typFile
            &= help "Source file to watch"
  , interval = 1 &= typ "SECONDS"
                 &= help "When running in a loop, check for changes every n seconds."
#endif
  }
  &= summary "Iconset generator (http://code.google.com/p/iconsetgenerator) "
  &= program prog


chooseRender :: DiagramOpts -> Diagram Cairo R2 -> IO ()
chooseRender opts d =
  case splitOn "." (output opts) of
    [""] -> putStrLn "No output file given."
    ps | last ps `elem` ["png", "ps", "pdf", "svg"] -> do
           let outTy = case last ps of
                 "png" -> PNG
                 "ps"  -> PS
                 "pdf" -> PDF
                 "svg" -> SVG
                 _     -> PDF
           fst $ renderDia
                   Cairo
                   ( CairoOptions
                     (output opts)
                     (mkSizeSpec
                       (fromIntegral <$> width opts)
                       (fromIntegral <$> height opts)
                     )
                     outTy
                     False
                   )
                   d
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

-- | @multiMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams paired with names as input.
--   The generated executable then takes a @--selection@ option
--   specifying the name of the diagram that should be rendered.  The
--   list of available diagrams may also be printed by passing the
--   option @--list@.
--
--   Example usage:
--
-- @
-- $ ghc --make MultiTest
-- [1 of 1] Compiling Main             ( MultiTest.hs, MultiTest.o )
-- Linking MultiTest ...
-- $ ./MultiTest --list
-- Available diagrams:
--   foo bar
-- $ ./MultiTest --selection bar -o Bar.png -w 200
-- @

multiMain :: IO ()
multiMain  = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog)
  let ds = prepareAll (maincolor opts) (bgcolor opts) (linecolor opts) (shadow opts) (onbackground opts)
  if list opts
    then showDiaList (map fst ds)
    else
      case icon opts of
        Nothing  -> putStrLn "No diagram selected." >> showDiaList (map fst ds)
        Just sel -> case lookup sel ds of
          Nothing -> putStrLn $ "Unknown diagram: " ++ sel
          Just d  -> chooseRender opts d

-- | Display the list of diagrams available for rendering.
showDiaList :: [String] -> IO ()
showDiaList ds = do
  putStrLn "Available diagrams:"
  putStrLn $ unwords ds

#ifdef CMDLINELOOP
waitForChange :: Maybe ModuleTime -> DiagramOpts -> String -> [String] -> IO ()
waitForChange lastAttempt opts prog args = do
    hSetBuffering stdout NoBuffering
    go lastAttempt
  where go lastAtt = do
          threadDelay (1000000 * interval opts)
          -- putStrLn $ "Checking... (last attempt = " ++ show lastAttempt ++ ")"
          (newBin, newAttempt) <- recompile lastAtt prog (src opts)
          if newBin
            then executeFile prog False args Nothing
            else go $ newAttempt `mplus` lastAtt

-- | @recompile t prog@ attempts to recompile @prog@, assuming the
--   last attempt was made at time @t@.  If @t@ is @Nothing@ assume
--   the last attempt time is the same as the modification time of the
--   binary.  If the source file modification time is later than the
--   last attempt time, then attempt to recompile, and return the time
--   of this attempt.  Otherwise (if nothing has changed since the
--   last attempt), return @Nothing@.  Also return a Bool saying
--   whether a successful recompilation happened.
recompile :: Maybe ModuleTime -> String -> Maybe String -> IO (Bool, Maybe ModuleTime)
recompile lastAttempt prog mSrc = do
  let errFile = prog ++ ".errors"
      srcFile = fromMaybe (prog ++ ".hs") mSrc
  binT <- maybe (getModTime prog) (return . Just) lastAttempt
  srcT <- getModTime srcFile
  if (srcT > binT)
    then do
      putStr "Recompiling..."
      status <- bracket (openFile errFile WriteMode) hClose $ \h ->
        waitForProcess =<< runProcess "ghc" ["--make", srcFile]
                           Nothing Nothing Nothing Nothing (Just h)

      if (status /= ExitSuccess)
        then putStrLn "" >> putStrLn (replicate 75 '-') >> readFile errFile >>= putStr
        else putStrLn "done."

      curTime <- getModuleTime
      return (status == ExitSuccess, Just curTime)

    else return (False, Nothing)

 where getModTime f = catch (Just <$> getModificationTime f)
                            (\(SomeException _) -> return Nothing)
#endif
