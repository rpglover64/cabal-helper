-- ghc-mod: Making Haskell development *more* fun
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP, FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric #-}

module Distribution.Helper (
    Programs(..)

  -- * Running Queries
  , Query
  , runQuery
  , runQuery'

  -- * Queries against Cabal\'s on disk state

  , entrypoints
  , sourceDirs
  , ghcOptions
  , ghcSrcOptions
  , ghcPkgOptions
  , ghcLangOptions

  -- * Result types
  , ChModuleName(..)
  , ChComponentName(..)
  , ChEntrypoint(..)

  -- * General information
  , buildPlatform

  -- * Managing @dist/@
  , reconfigure
  , writeAutogenFiles

  -- * $libexec related error handling
  , LibexecNotFoundError(..)
  , libexecNotFoundError
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Exception as E
import Data.Char
import Data.Monoid
import Data.List
import Data.Default
import Data.Typeable
import System.Environment
import System.FilePath
import System.Directory
import System.Process
import Text.Printf
import GHC.Generics

import Paths_cabal_helper (getLibexecDir)
import CabalHelper.Types

-- | Paths or names of various programs we need.
data Programs = Programs {
      cabalProgram  :: FilePath,
      ghcProgram    :: FilePath,
      ghcPkgProgram :: FilePath
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Default Programs where
    def = Programs "cabal" "ghc" "ghc-pkg"

data SomeLocalBuildInfo = SomeLocalBuildInfo {
      slbiEntrypoints   :: [(ChComponentName, ChEntrypoint)],
      slbiSourceDirs    :: [(ChComponentName, [String])],
      slbiGhcOptions    :: [(ChComponentName, [String])],
      slbiGhcSrcOptions :: [(ChComponentName, [String])],
      slbiGhcPkgOptions :: [(ChComponentName, [String])],
      slbiGhcLangOptions :: [(ChComponentName, [String])]
    } deriving (Eq, Ord, Read, Show)

-- | Caches helper executable result so it doesn't have to be run more than once
-- as reading in Cabal's @LocalBuildInfo@ datatype from disk is very slow but
-- running all possible queries against it at once is cheap.
newtype Query m a = Query { unQuery :: StateT (Maybe SomeLocalBuildInfo)
                                         (ReaderT (Programs, FilePath) m) a }
    deriving (Functor, Applicative, Monad)

type MonadQuery m = ( MonadIO m
                    , MonadState (Maybe SomeLocalBuildInfo) m
                    , MonadReader (Programs, FilePath) m)

run r s action =
    try $ evaluate $ flip runReaderT r (flip evalStateT s (unQuery action))

-- | @runQuery query distdir@. Run a 'Query'. @distdir@ is where Cabal's
-- @setup-config@ file is located.
runQuery :: Monad m
         => FilePath -- ^ Path to @dist/@
         -> Query m a
         -> m (Either ChError a)
runQuery fp action = run (def, fp) Nothing action

runQuery' :: Monad m
         => Programs
         -> FilePath -- ^ Path to @dist/@
         -> Query m a
         -> m (Either ChError a)
runQuery' progs fp action = run (progs, fp) Nothing action

getSlbi :: MonadQuery m => m SomeLocalBuildInfo
getSlbi = do
  s <- get
  case s of
    Nothing -> do
            slbi <- getSomeConfigState
            put (Just slbi)
            return slbi
    Just slbi -> return slbi

-- | Modules or files Cabal would have the compiler build directly. Can be used
-- to compute the home module closure for a component.
entrypoints   :: MonadIO m => Query m [(ChComponentName, ChEntrypoint)]

-- | A component's @source-dirs@ field, beware as if this is empty implicit
-- behaviour in GHC kicks in.
sourceDirs    :: MonadIO m => Query m [(ChComponentName, [FilePath])]

-- | All options cabal would pass to GHC.
ghcOptions    :: MonadIO m => Query m [(ChComponentName, [String])]

-- | Only search path related GHC options.
ghcSrcOptions :: MonadIO m => Query m [(ChComponentName, [String])]

-- | Only package related GHC options, sufficient for things don't need to
-- access any home modules.
ghcPkgOptions :: MonadIO m => Query m [(ChComponentName, [String])]

-- | Only language related options, i.e. @-XSomeExtension@
ghcLangOptions :: MonadIO m => Query m [(ChComponentName, [String])]

entrypoints   = Query $ slbiEntrypoints   `liftM` getSlbi
sourceDirs    = Query $ slbiSourceDirs    `liftM` getSlbi
ghcOptions    = Query $ slbiGhcOptions    `liftM` getSlbi
ghcSrcOptions = Query $ slbiGhcSrcOptions `liftM` getSlbi
ghcPkgOptions = Query $ slbiGhcPkgOptions `liftM` getSlbi
ghcLangOptions = Query $ slbiGhcLangOptions `liftM` getSlbi

-- | Run @cabal configure@
reconfigure :: MonadIO m
            => Programs -- ^ Program paths
            -> [String] -- ^ Command line arguments to be passed to @cabal@
            -> m ()
reconfigure progs cabalOpts = do
    let progOpts =
            [ "--with-ghc=" ++ ghcProgram progs ]
            -- Only pass ghc-pkg if it was actually set otherwise we
            -- might break cabal's guessing logic
            ++ if ghcPkgProgram progs /= ghcPkgProgram def
                 then [ "--with-ghc-pkg=" ++ ghcPkgProgram progs ]
                 else []
            ++ cabalOpts
    _ <- liftIO $ readProcess (cabalProgram progs) ("configure":progOpts) ""
    return ()

getSomeConfigState :: MonadQuery m => m SomeLocalBuildInfo
getSomeConfigState = ask >>= \(progs, distdir) -> do
  let progArgs = [ "--with-ghc="     ++ ghcProgram progs
                 , "--with-ghc-pkg=" ++ ghcPkgProgram progs
                 , "--with-cabal="   ++ cabalProgram progs
                 ]

  let args = [ "entrypoints"
             , "source-dirs"
             , "ghc-options"
             , "ghc-src-options"
             , "ghc-pkg-options"
             , "ghc-lang-options"
             ] ++ progArgs

  res <- liftIO $ do
    exe  <- findLibexecExe "cabal-helper-wrapper"
    (rv, out, err) <- readProcessWithExitCode exe (distdir:args) ""
    let msgs = map read $ lines out

    evaluate msgs `E.catch`


    evaluate (read out) `E.catch` \(SomeException _) ->
      error $ concat ["getSomeConfigState", ": ", exe, " "
                     , intercalate " " (map show $ distdir:args)
                     , " (read failed)"]

  let [ Just (ChResponseEntrypoints eps),
        Just (ChResponseStrings srcDirs),
        Just (ChResponseStrings ghcOpts),
        Just (ChResponseStrings ghcSrcOpts),
        Just (ChResponseStrings ghcPkgOpts),
        Just (ChResponseStrings ghcLangOpts) ] = res

  return $ SomeLocalBuildInfo
             eps srcDirs ghcOpts ghcSrcOpts ghcPkgOpts ghcLangOpts

-- | Create @cabal_macros.h@ and @Paths_\<pkg\>@ possibly other generated files
-- in the usual place.
writeAutogenFiles :: MonadIO m
                  => FilePath -- ^ Path to the @dist/@ directory
                  -> m ()
writeAutogenFiles distdir = liftIO $ do
  exe  <- findLibexecExe "cabal-helper-wrapper"
  void $ readProcess exe [distdir, "write-autogen-files"] ""

buildPlatform :: IO String
buildPlatform = do
  exe  <- findLibexecExe "cabal-helper-wrapper"
  dropWhileEnd isSpace <$> readProcess exe ["print-build-platform"] ""
 where
   -- dropWhileEnd is not provided prior to base 4.5.0.0.
   dropWhileEnd :: (a -> Bool) -> [a] -> [a]
   dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

-- | This exception is thrown by all 'runQuery' functions if the internal
-- wrapper executable cannot be found. You may catch this and present the user
-- an appropriate error message however the default is to print
-- 'libexecNotFoundError'.
data LibexecNotFoundError = LibexecNotFoundError String FilePath
                          deriving (Typeable)
instance Exception LibexecNotFoundError
instance Show LibexecNotFoundError where
  show (LibexecNotFoundError exe dir) =
    libexecNotFoundError exe dir "https://github.com/DanielG/cabal-helper/issues"

findLibexecExe :: String -> IO FilePath
findLibexecExe "cabal-helper-wrapper" = do
    libexecdir <- getLibexecDir
    let exeName = "cabal-helper-wrapper"
        exe = libexecdir </> exeName

    exists <- doesFileExist exe

    if exists
       then return exe
       else do
         mdir <- tryFindCabalHelperTreeLibexecDir
         case mdir of
           Nothing ->
               error $ throw $ LibexecNotFoundError exeName libexecdir
           Just dir ->
               return $ dir </> "dist" </> "build" </> exeName </> exeName
findLibexecExe exe = error $ "findLibexecExe: Unknown executable: " ++ exe

tryFindCabalHelperTreeLibexecDir :: IO (Maybe FilePath)
tryFindCabalHelperTreeLibexecDir = do
  exe <- getExecutablePath'
  dir <- case takeFileName exe of
    "ghc" -> do -- we're probably in ghci; try CWD
        getCurrentDirectory
    _ ->
        return $ (!!4) $ iterate takeDirectory exe
  exists <- doesFileExist $ dir </> "cabal-helper.cabal"
  return $ if exists
             then Just dir
             else Nothing

libexecNotFoundError :: String   -- ^ Name of the executable we were trying to
                                 -- find
                     -> FilePath -- ^ Path to @$libexecdir@
                     -> String   -- ^ URL the user will be directed towards to
                                 -- report a bug.
                     -> String
libexecNotFoundError exe dir reportBug = printf
 ( "Could not find $libexecdir/%s\n"
 ++"\n"
 ++"If you are a developer set the environment variable\n"
 ++"`cabal_helper_libexecdir' to override $libexecdir[1]. The following will\n"
 ++"work in the cabal-helper source tree:\n"
 ++"\n"
 ++"    $ export cabal_helper_libexecdir=$PWD/dist/build/%s\n"
 ++"\n"
 ++"[1]: %s\n"
 ++"\n"
 ++"If you don't know what I'm talking about something went wrong with your\n"
 ++"installation. Please report this problem here:\n"
 ++"\n"
 ++"    %s") exe exe dir reportBug

getExecutablePath' :: IO FilePath
getExecutablePath' =
#if MIN_VERSION_base(4,6,0)
    getExecutablePath
#else
    getProgName
#endif

chErrorMessage (ChErrorSetupConfigHeader cfgf) = printf "\
\Could not read Cabal's persistent setup configuration header\n\
\- Check first line of: %s\n\
\- Maybe try: $ cabal configure" cfgf

chErrorMessage (ChErrorGhcVersion lbiVer exeVer) = printf "\
\GHC major version changed! (was %s, now %s)\n\
\- Please reconfigure the project: $ cabal clean && cabal configure\
\ " (sver lbiVer) (sver exeVer)

chErrorMessage (ChErrorInstallCabalLibrary ver) = printf "\
\Installing Cabal version %s failed.\n\
\\n\
\You have the following choices to fix this:\n\
\\n\
\- The easiest way to try and fix this is just reconfigure the project and try\n\
\  again:\n\
\        $ cabal clean && cabal configure\n\
\\n\
\- If that fails you can try to install the version of Cabal mentioned above\n\
\  into your global/user package-db somehow, you'll probably have to fix\n\
\  something otherwise it wouldn't have failed above:\n\
\        $ cabal install Cabal --constraint 'Cabal == %s'\n\
\\n\
\- If you're using `Build-Type: Simple`:\n\
\  - You can see if you can reinstall your cabal-install executable while\n\
\    having it linked to a version of Cabal that's available in you\n\
\    package-dbs or can be built automatically:\n\
\        $ ghc-pkg list | grep Cabal  # find an available Cabal version\n\
\        $ cabal install cabal-install --constraint 'Cabal == $the_found_version'\n\
\    Afterwards you'll have to reconfigure your project:\n\
\        $ cabal clean && cabal configure\n\
\\n\
\- If you're using `Build-Type: Custom`:\n\
\  - Have cabal-install rebuild your Setup.hs executable with a version of the\n\
\    Cabal library that you have available in your global/user package-db:\n\
\        $ cabal clean && cabal configure\n\
\    You might also have to install some version of the Cabal to do this:\n\
\        $ cabal install Cabal\n\
\\n" (sver ver) (sver ver)

sver = showVersion

chErrorMessage (ChProcess fn exe args rv) =
    concat [fn, ": ", exe, " "
           , intercalate " " (map show args)
           , " (exit " ++ show rv ++ ")"]


chErrorSuggestions (ChErrorSetupConfigHeader cfgf) = [ChReconfigure]
chErrorSuggestions (ChErrorGhcVersion lbiVer exeVer) = [ChReconfigure]
chErrorSuggestions (ChErrorInstallCabalLibrary ver) = concat $
  [ [ ChReconfigure ]
  , ChInstallCabalLibrary ChPrivate (ChCVExactVersion ver)
  , map reconfigureAfter $ concat [
        [ ChUpdateCabalInstall (ChCVExactVersion ver) ]
      , map buildTypeSimple [
          ChUpdateCabalInstall ChCVAnyAvailableVersion
        , installCabalThen (ChCVSameMajorVersion ver) ChUpdateCabalInstall
        , installCabalThen (ChCVLaterThanOrEqual ver) ChUpdateCabalInstall
        ]
      ]
  , map buildTypeCustom $
      [ installCabalThen (ChCVSameMajorVersion ver) ChReconfigure
      , installCabalThen (ChCVLaterThanOrEqual ver) ChReconfigure
      ]
  ]
 where
   buildTypeSimple = ChSugConditional (ChBuildType Simple)
   buildTypeCustom = ChSugConditional (ChBuildType Custom)
   cabalLibAvailable = ChSugConditional ChCabalLibraryNotAvailable
   installCabalThen v th = ChSugSequence [
                            ChInstallCabalLibrary ChUserPackageDb v
                           , th v
                           ]
   reconfigureAfter s = ChSugSequence [s, ChReconfigure]

chErrorSuggestions _ = []


chExecuteSuggestion :: Programs -> ChSuggestion -> Maybe (ExceptT String IO ())
chExecuteSuggestion opts (ChUpdateCabalInstall cabalVer) = Nothing
chExecuteSuggestion opts ChReconfigure = Just $ fmap Right $
  cabalClean opts >> cabalConfigure opts
chExecuteSuggestion opts (ChInstallCabalLibrary loc cabalVer) = Just $ do
  installCabal opts loc cabalVer
chExecuteSuggestion opts (ChSugConditional cond sug) = Just $ do
    b <- chEvalCond cond
    when b $ chExecuteSuggestion opts sug
chExecuteSuggestion opts (ChSugSequence sugs) =
    Just $ mapM_ (chExecuteSuggestion opts) sugs


cabalClean opts =
  cabal opts ["clean"]

cabalConfigure opts = do
  cabal opts $ (cabalProgArgs opts) ++ ["configure"]

cabalInstall opts mbindir = do
  cabal opts $ (cabalProgArgs opts) ++ ["install"]

cabalProgArgs opts = [ "--with-ghc=" ++ ghcProgram opts ]
                  ++ [ "--with-ghc-pkg=" ++ ghcPkgProgram opts
                     | ghcPkgProgram opts /= ghcPkgProgram defaultOptions
                     ]

cabal = cabal' Nothing
cabal' mcwd opts args =
    callProcessStderr mcwd (cabalProgram opts) $ progArgs ++ args


installCabal :: Options -> ChLocation -> Version -> IO FilePath
installCabal opts loc ver = do
  appdir <- appDataDir
  let sver = showVersion ver

  notify $ ChInstallingCabalLibrary loc ver

  db <- createPkgDb opts ver
  cabal' (Just "/") opts $
    packageDbArgs loc ++ [ "-v0", "install", "Cabal-"++(sver ver) ]

  hPutStrLn stderr "done"
  return db

 where
  packageDbArgs ChPrivate = [ "--package-db=clear"
                            , "--package-db=global"
                            , "--package-db=" ++ db
                            , "--prefix=" ++ db </> "prefix"
                            ]
  packageDbArgs ChUserPackageDb = [ "--package-db=global"
                                  , "--package-db=user"
                                  ]


cabalInfoMessage (ChInstallingCabalLibrary ChPrivate ver) = unlines [
 "Installing a private copy of Cabal, this might take a while but will only \
 \happen once per Cabal version.",
 "",
 "If you want to avoid this automatic installation altogether install \
 \version %s of Cabal manually (into your user or global package-db):",
 "    $ cabal install Cabal %s",
 "",
 "Building Cabal-%s..."
 ]

cabalInfoMessage (ChInstallingCabalLibrary ChUserPackageDb ver) = unlines [
 "Installing Cabal into your user package database, this might take a while.",
 "",
 "If you want to avoid this automatic installation altogether install \
 \version %s of Cabal manually (into your user or global package-db):",
 "    $ cabal install Cabal %s",
 "",
 "Building Cabal-%s..."
 ]


chCabalVersionToConstraint (ChCVExactVersion v) =
    Just $ "Cabal == " ++ showVersion v
chCabalVersionToConstraint (ChCVSameMajorVersion v) =
    Just $ "Cabal == " ++ showVersion (majorVer v) ++ ".*"
chCabalVersionToConstraint (ChCVLaterThanOrEqual v) =
    Just $ "Cabal >= " ++ showVersion v
chCabalVersionToConstraint ChCVAnyAvailableVersion =
    Nothing
