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
    let d = utctDay time
    let cl = cleanup cmd
    let cl_type = whatCleanup (whichType (frequency cmd) d)
    rc <- doBackup (show (whichType (frequency cmd) d)) d (dir cmd)
    let n = retain cmd
    case rc of
         ExitFailure _ -> exitWith rc
         ExitSuccess -> if cl
                           then case cl_type of
                                    Nothing -> exitWith rc
                                    Just f -> doCleanup 
                                                (last 
                                                (splitDirectories (dir cmd))
                                                ) 
                                                f n >> exitWith rc
                        else exitWith rc