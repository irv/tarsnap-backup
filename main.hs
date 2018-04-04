module Main where

import TarsnapBackup
import System.Exit
import System.Console.CmdArgs
import Data.Time.Clock
import System.FilePath

main :: IO ExitCode
main = do
  cmd <- cmdArgsRun $ cmdArgsMode tb
  time <- getCurrentTime
  let day = utctDay time
      cl = cleanup cmd
      fr = autoFrequency (frequency cmd) day
      cl_type = whatCleanup fr
      n = retain cmd
  rc <- doBackup fr (dryrun cmd) (verbose cmd) (exclude cmd) day (dir cmd)
  case rc of
    ExitFailure _ -> exitWith rc
    ExitSuccess ->
      if cl
        then case cl_type of
               Nothing -> exitWith rc
               Just f ->
                 doCleanup f (dryrun cmd) (verbose cmd) (last (splitDirectories (dir cmd))) n >> exitWith rc
        else exitWith rc
