{-# LANGUAGE CPP #-}

module Main (main) where

import Common
import Common.System
import qualified Common.Text as T

#if (defined(VERSION_lens_aeson))
import Control.Lens
import Data.Aeson.Lens
#else
import Lens.Micro
import Lens.Micro.Aeson
#endif
#if (defined(VERSION_aeson_pretty))
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Conduit (queryString)
#endif

import qualified Data.HashMap.Lazy as H
import Data.Ini.Config
import Distribution.Fedora.Branch
import Options.Applicative (ReadM, eitherReader)
import SimpleCmd.Git
import SimpleCmdArgs
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.IO (BufferMode (NoBuffering), hIsTerminalDevice, hSetBuffering, stdin, stdout)
import Web.Fedora.Copr (coprGetProject)

--import Package (Package)

--type Package = String

main :: IO ()
main = do
  tty <- hIsTerminalDevice stdin
  when tty $ hSetBuffering stdout NoBuffering
  activeBranches <- getFedoraBranches
  gitdir <- isGitDir "."
  dispatchCmd gitdir activeBranches

data BuildBy = SingleBuild | ValidateByRelease | ValidateByArch | BuildByRelease
  deriving (Eq)

dispatchCmd :: Bool -> [Branch] -> IO ()
dispatchCmd _ activeBranches =
  -- FIXME package version
  simpleCmdArgs Nothing "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
--    subcommands $
--    [
-- --      Subcommand "create" "create copr" $
-- --      cloneCmd <$> optional branchOpt <*> (strArg "COPRNAME")
-- --    , Subcommand "switch" "Switch branch" $
-- --      switchCmd <$> (anyBranchOpt <|> anyBranchArg) <*> many (pkgArg "PACKAGE...")
--      Subcommand "buildsrc" "Build from spec/srpm" $
      buildSrcCmd <$> dryrun <*> buildByOpt <*> many branchOpt <*> archOpts <*> strArg "PROJECT" <*> strArg "SRPM/SPEC"
-- --    , Subcommand "status" "Status package/branch status" $
-- --      statusCmd <$> switchWith 'r' "reviews" "Status of reviewed packages" <*> branchesPackages
-- --    , Subcommand "merge" "Merge from newer branch" $
-- --      mergeCmd <$> branchesPackages
-- --      Subcommand "build" "Build package(s)" $
-- --      buildCmd <$> mergeOpt <*> targetOpt <*> branchesPackages,
-- --      Subcommand "pull" "Git pull packages" $
-- --      pullPkgs <$> some (pkgArg "PACKAGE...")
--    ]
  where
    dryrun = switchWith 'n' "dry-run" "Just print commands that would be executed"

    buildByOpt = flagWith' SingleBuild 'S' "single" "Non-progressive normal single build" <|> flagWith' BuildByRelease 'R' "by-release" "Builds by release" <|> flagWith ValidateByRelease ValidateByArch 'A' "by-arch" "Build across latest release archs first (default is across releases for primary arch)"

    branchOpt :: Parser Branch
    branchOpt = optionWith branchM 'b' "branch" "BRANCH" "branch"

    -- branchArg :: Parser Branch
    -- branchArg = argumentWith branchM "BRANCH.."

    branchM :: ReadM Branch
    branchM = eitherReader (eitherActiveBranch activeBranches)

    archOpts :: Parser [String]
    archOpts = many (strOptionWith 'a' "arch" "ARCH" "archs to build for")

    -- anyBranchOpt :: Parser Branch
    -- anyBranchOpt = optionWith anyBranchM 'b' "branch" "BRANCH" "branch"

    -- anyBranchArg :: Parser Branch
    -- anyBranchArg = argumentWith anyBranchM "BRANCH.."

    -- anyBranchM :: ReadM Branch
    -- anyBranchM = eitherReader eitherBranch

    -- pkgArg :: String -> Parser Package
    -- pkgArg lbl = removeSuffix "/" <$> strArg lbl

    -- pkgOpt :: Parser String
    -- pkgOpt = removeSuffix "/" <$> strOptionWith 'p' "package" "PKG" "package"

    -- branchesPackages :: Parser ([Branch],[Package])
    -- branchesPackages = if gitdir then
    --   pair <$> (many branchOpt <|> many branchArg) <*> pure []
    --   else pair <$> many branchOpt <*> many (pkgArg "PACKAGE..") <|>
    --        pair <$> many branchArg <*> some pkgOpt

    -- pair a b = (a,b)

    -- mergeOpt = switchWith 'm' "merge" "merge from newer branch"



-- FIXME make project optional ?
-- FIXME repo config with a setup command?
buildSrcCmd :: Bool -> BuildBy -> [Branch] -> [String] -> String -> FilePath -> IO ()
buildSrcCmd dryrun buildBy brs archs project src = do
  -- pkg <- takeFileName <$> getCurrentDirectory
  username <- getUsername
  chroots <- coprChroots username project
  let branches =
        if null brs
        then (map (releaseBranch . T.pack) . nub . map removeArch) chroots
        else brs
      buildroots =
        reverseSort $
          if null archs
          then [chroot | chroot <- chroots, removeArch chroot `elem` map branchRelease branches]
          else [chroot | arch <- archs, br <- branches, let chroot = branchRelease br ++ "-" ++ arch, chroot `elem` chroots]
  if null buildroots
    then error' "No chroots chosen"
    else do
    -- FIXME hack to avoid generating srpm for dryrun
    srpm <- if "spec" `isExtensionOf` src && not dryrun
            then generateSrpm src
            else return src
    case buildBy of
                SingleBuild -> coprBuild dryrun project srpm buildroots
                -- FIXME or default to secondary parallel to previous primary
                ValidateByRelease -> do
                  let initialChroots =
                        let primaryArch = releaseArch $ head buildroots
                        in map pure $ filter (isArch primaryArch) buildroots
                      remainingChroots = buildroots \\ concat initialChroots
                  staggerBuilds srpm initialChroots remainingChroots
                ValidateByArch -> do
                  let initialChroots =
                        let newestRelease = removeArch $ head buildroots
                        in map pure $ filter (newestRelease `isPrefixOf`) buildroots
                      remainingChroots = buildroots \\ concat initialChroots
                  staggerBuilds srpm initialChroots remainingChroots
                BuildByRelease -> do
                  let initialChroots = groupBy sameRelease buildroots
                      remainingChroots = buildroots \\ concat initialChroots
                  staggerBuilds srpm initialChroots remainingChroots
  where
    staggerBuilds srpm initialChroots remainingChroots = do
      mapM_ (coprBuild dryrun project srpm) initialChroots
      unless (null remainingChroots) $
        coprBuild dryrun project srpm remainingChroots

    removeArch relarch = init $ dropWhileEnd (/= '-') relarch

    releaseArch relarch = takeWhileEnd (/= '-') relarch

    isArch arch release = releaseArch release == arch

    sameRelease r1 r2 = removeArch r1 == removeArch r2

    reverseSort = reverse . sort

    -- from extra
    takeWhileEnd :: (a -> Bool) -> [a] -> [a]
    takeWhileEnd f = reverse . takeWhile f . reverse

branchRelease :: Branch -> String
branchRelease Master = "fedora-rawhide"
branchRelease (Fedora n) = "fedora-" ++ show n
branchRelease (EPEL n) = "epel-" ++ show n

--data Chroot = Chroot Release Arch

-- FIXME Chroot type
coprChroots :: String -> String -> IO [String]
coprChroots owner project = do
  proj <- coprGetProject coprServer owner project
  case proj ^? key "chroot_repos" . _Object of
    Nothing -> error' "chroot_repos not found"
    Just obj -> return $ (map T.unpack . reverse . sort . H.keys) obj

coprServer :: String
coprServer = "copr.fedorainfracloud.org"

getUsername :: IO String
getUsername = do
  rc <- getUserConfigDir "copr"
  readIniConfig rc rcParser id
  where
    rcParser :: IniParser String
    rcParser =
      section "copr-cli" $
      fieldOf "username" string

-- changed from fbrnch/Bugzilla.hs
readIniConfig :: FilePath -> IniParser a -> (a -> b) -> IO b
readIniConfig inifile iniparser record = do
  havefile <- doesFileExist inifile
  if not havefile
    then error' $ inifile ++ " not found: maybe GET /api from copr"
    else do
    ini <- T.readFile inifile
    let config = parseIniFile ini iniparser
    return $ either error' record config

coprBuild :: Bool -> String -> FilePath -> [String] -> IO ()
coprBuild _ _ _ [] = error' "No chroots chosen"
coprBuild dryrun project srpm buildroots = do
  let chrootargs = mconcat [["-r", bldrt] | bldrt <- buildroots]
      buildargs = ["build", "--nowait"] ++ chrootargs ++ [project, srpm]
  putStrLn ""
  cmdN "copr" buildargs
  unless dryrun $ do
    output <- cmd "copr" buildargs
    putStrLn output
    let bid = last $ words $ last $ lines output
    cmd_ "copr" ["watch-build", bid]

-- coprBuilds :: Bool -> [String] -> String -> String -> IO ()
-- coprBuilds _ [] _ _ = return ()
-- coprBuilds dryrun (chroot:chroots) project srpm = do
--   let chrootargs = mconcat [["-r", bldrt] |  bldrt <- buildroots]
--       buildargs = ["build", "--nowait"] ++ chrootargs ++ [project,srpm]
--   if dryrun then
--     cmdN "copr" buildargs
--     else do
--     output <- cmd "copr" $ buildargs
--     putStrLn output
--     let bid = last $ words $ last $ lines output
--     cmd_ "copr" ["watch-build", bid]

-- adapted from fbrnch Package
generateSrpm :: FilePath -> IO FilePath
generateSrpm spec = do
  let distopt = ["--undefine", "dist"]
  sources <- map sourceFieldFile <$> cmdLines "spectool" ["-S", spec]
  forM_ sources $ \ src ->
    unlessM (doesFileExist src) $
    cmd_ "spectool" ["-g", "-S", "-C", ".", spec]
  srpmfile <- cmd "rpmspec" $ ["-q", "--srpm"] ++ distopt ++ ["--qf", "%{name}-%{version}-%{release}.src.rpm", spec]
  let srpm = srpmfile
      rpmsrcopts = ["--define", "_srcrpmdir ."] ++ ["--define", "_sourcedir ."]
  ifM (notM $ doesFileExist srpm)
    (buildSrpm (distopt ++ rpmsrcopts)) $
    do
    specTime <- getModificationTime spec
    srpmTime <- getModificationTime srpm
    if srpmTime > specTime
      then do
      -- pretty print with ~/
      putStrLn $ srpm ++ " is up to date"
      return srpm
      else buildSrpm (distopt ++ rpmsrcopts)
  where
    buildSrpm opts = do
      srpm <- last . words <$> cmd "rpmbuild" (opts ++ ["-bs", spec])
      putStrLn $ "Created " ++ takeFileName srpm
      return srpm

    sourceFieldFile :: String -> FilePath
    sourceFieldFile field' =
      if null field' then
        -- should be impossible
        error "empty source field!"
      else (takeFileName . last . words) field'

#if !MIN_VERSION_simple_cmd(0,1,4)
error' :: String -> a
#if MIN_VERSION_base(4,9,0)
error' = errorWithoutStackTrace
#else
error' = error
#endif
#endif

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
