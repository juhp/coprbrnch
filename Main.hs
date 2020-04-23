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

data BuildBy = SingleBuild | ValidateByRelease | ValidateByArch
  deriving (Eq)

dispatchCmd :: Bool -> [Branch] -> IO ()
dispatchCmd _ activeBranches =
  -- FIXME package version
  simpleCmdArgs Nothing "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    subcommands
    [
--      Subcommand "create" "create copr" $
--      cloneCmd <$> optional branchOpt <*> (strArg "COPRNAME")
--    , Subcommand "switch" "Switch branch" $
--      switchCmd <$> (anyBranchOpt <|> anyBranchArg) <*> many (pkgArg "PACKAGE...")
      Subcommand "buildsrc" "Build from spec/srpm" $
      buildSrcCmd <$> dryrun <*> buildByOpt <*> many branchOpt <*> archOpts <*> strArg "PROJECT" <*> strArg "SRPM/SPEC"
--    , Subcommand "status" "Status package/branch status" $
--      statusCmd <$> switchWith 'r' "reviews" "Status of reviewed packages" <*> branchesPackages
--    , Subcommand "merge" "Merge from newer branch" $
--      mergeCmd <$> branchesPackages
--      Subcommand "build" "Build package(s)" $
--      buildCmd <$> mergeOpt <*> targetOpt <*> branchesPackages,
--      Subcommand "pull" "Git pull packages" $
--      pullPkgs <$> some (pkgArg "PACKAGE...")
    ]
  where
    dryrun = switchWith 'n' "dry-run" "Do not actually do anything"

    buildByOpt = flagWith' SingleBuild 'S' "single" "Non-progressive normal single build" <|> flagWith ValidateByRelease ValidateByArch 'A' "by-arch" "Build across latest release archs first (default is across releases for primary arch)"

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
    let initialChroots =
              case buildBy of
                SingleBuild -> []
                ValidateByRelease ->
                  let primaryArch = releaseArch $ head buildroots
                   in filter (isArch primaryArch) buildroots
                ValidateByArch ->
                  let newestRelease = removeArch $ head buildroots
                   in filter (newestRelease `isPrefixOf`) buildroots
    forM_ initialChroots $ \chroot ->
      coprBuild dryrun [chroot] project src
    let remainingChroots = buildroots \\ initialChroots
    unless (null remainingChroots) $
      coprBuild dryrun remainingChroots project src
  where
    removeArch relarch = init $ dropWhileEnd (/= '-') relarch

    releaseArch relarch = takeWhileEnd (/= '-') relarch

    isArch arch release = releaseArch release == arch

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

coprBuild :: Bool -> [String] -> String -> String -> IO ()
coprBuild _ [] _ _ = error' "No chroots chosen"
coprBuild dryrun buildroots project src = do
  let chrootargs = mconcat [["-r", bldrt] | bldrt <- buildroots]
      buildargs = ["build", "--nowait"] ++ chrootargs ++ [project, src]
  cmdN "copr" buildargs
  unless dryrun $ do
    output <- cmd "copr" $ buildargs
    putStrLn output
    let bid = last $ words $ last $ lines output
    cmd_ "copr" ["watch-build", bid]

-- coprBuilds :: Bool -> [String] -> String -> String -> IO ()
-- coprBuilds _ [] _ _ = return ()
-- coprBuilds dryrun (chroot:chroots) project src = do
--   let chrootargs = mconcat [["-r", bldrt] |  bldrt <- buildroots]
--       buildargs = ["build", "--nowait"] ++ chrootargs ++ [project,src]
--   if dryrun then
--     cmdN "copr" buildargs
--     else do
--     output <- cmd "copr" $ buildargs
--     putStrLn output
--     let bid = last $ words $ last $ lines output
--     cmd_ "copr" ["watch-build", bid]

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,1,4))
#else
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif
#endif
