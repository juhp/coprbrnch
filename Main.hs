module Main (main) where

import Common
import Common.System

import Distribution.Fedora.Branch
import Options.Applicative (eitherReader, ReadM)
import SimpleCmd.Git
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, hIsTerminalDevice, stdin, stdout)

--import Package (Package)

--type Package = String

main :: IO ()
main = do
  tty <- hIsTerminalDevice stdin
  when tty $ hSetBuffering stdout NoBuffering
  activeBranches <- getFedoraBranches
  gitdir <- isGitDir "."
  dispatchCmd gitdir activeBranches

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
      buildSrcCmd <$> many branchOpt <*> many (strOptionWith 'a' "arch" "ARCH" "archs to build for") <*> strArg "SRPM/SPEC"
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
    branchOpt :: Parser Branch
    branchOpt = optionWith branchM 'b' "branch" "BRANCH" "branch"

    -- branchArg :: Parser Branch
    -- branchArg = argumentWith branchM "BRANCH.."

    branchM :: ReadM Branch
    branchM = eitherReader (eitherActiveBranch activeBranches)

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

-- FIXME --repo
-- default to repo = package = dir
-- FIXME repo config: maybe setup command
buildSrcCmd :: [Branch] -> [String] -> FilePath -> IO ()
buildSrcCmd branches archs src = do
  pkg <- takeFileName <$> getCurrentDirectory
  let buildroots = [branchRelease br ++ "-" ++ arch | arch <- archs, br <- branches]
      args = ["build", "--nowait"] ++ mconcat [["-r", bldrt] |  bldrt <- buildroots] ++ [pkg, src]
  cmdN "copr" args
  output <- cmd "copr" args
  putStrLn output
  let bid = last $ words $ last $ lines output
  cmd_ "copr" ["watch-build", bid]

branchRelease :: Branch -> String
branchRelease Master = "fedora-rawhide"
branchRelease (Fedora n) = "fedora-" ++ show n
branchRelease (EPEL n) = "epel-" ++ show n
