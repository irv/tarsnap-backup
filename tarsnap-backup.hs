{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}

{-

(c) Andy Irving 2010
This produces archives in the following format:

Basename-Frequency-YYYY-MM-DD

where Frequency is Daily/Weekly/Monthly

If cleanup is not disabled, archives of the next lower type will be deleted.
So if the Frequency is Monthyl, any Daily archives for this Basename are
deleted.

-}

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Calendar.OrdinalDate
import Data.List
import System.Process
import "mtl" Control.Monad.Trans
import System.Console.CmdArgs
import System.FilePath
import Data.Maybe
import System.Exit

data Frequency = Daily | Weekly | Monthly | Auto
    deriving (Read, Show, Eq, Data, Typeable)

doBackup :: String -> Day -> FilePath -> IO ExitCode
doBackup f d b = readProcessWithExitCode "tarsnap" ["-c", "-f", archive_name b, b] [] >>= checkRes
    where checkRes (rc, out, err) = case rc of
                                            ExitFailure eno -> error $ "Unable to backup:\n" ++ err
                                            ExitSuccess -> return rc
          basename d =  last (splitDirectories d) -- /home/irv/blah == blah
          archive_name b = concat $ intersperse "-" [basename b, f, showGregorian d] -- blah-Frequency-2010-09-11

-- When doing the cleanup, remove backups which are of the lower frequency
whatCleanup :: Frequency -> Maybe Frequency
whatCleanup f = whatType f
    where whatType Daily = Nothing
          whatType Weekly = Just Daily
          whatType Monthly = Just Weekly

doCleanup :: String -> Frequency -> IO ExitCode
doCleanup b f = readProcessWithExitCode "tarsnap" ["--list-archives"] [] >>= continueCleanup
    where continueCleanup (rc, out, err) = case rc of
                                                ExitFailure eno -> error $ "Unable to list archives for cleanup:\n" ++ err
                                                ExitSuccess -> case (getCleanupList b f (lines out)) of
                                                                    [] -> return rc
                                                                    (xs) -> execCleanup xs;

execCleanup :: [String] -> IO ExitCode
execCleanup l = do
    (rc, out, err) <- readProcessWithExitCode "tarsnap" (["-d", "-f"] ++ intersperse "-f" l) []
    case rc of
         ExitFailure eno ->  error err
         ExitSuccess -> return rc

-- from the input list ["blah-Frequency-2010-09-11", "blah-Frequency-2010-09-12"]
-- strip the "blah-" string if it's an archive of this dir
-- then strip the "Frequency-" string if it's the frequency we're clearing up
-- then remove any Nothing's from the list
getCleanupList :: String -> Frequency -> [String] -> [String]
getCleanupList b f l = sort $ map ((b ++ "-" ++ show f ++ "-") ++) $ mapMaybe (stripPrefix (show f ++ "-")) $ mapMaybe (stripPrefix (b ++ "-")) l

-- using System.Console.CmdArgs on account of System.Console.GetOpt being
-- not unlike stabbed repeatedly in the face
data TarsnapBackup = TarsnapBackup {
        frequency :: Frequency,
        dir :: FilePath,
        cleanup :: Bool
        }
    deriving (Show, Data, Typeable)

tb = TarsnapBackup {
    frequency = Auto &= opt "Auto" &= help "Force a backup of type (Daily, Weekly, Monthly)",
    cleanup = def &= help "Cleanup old backups",
    dir = "" &= args &= typDir
    } &=
    help "This script manages Tarsnap backups" &=
    summary "(c) Andy Irving <andy@soundforsound.co.uk> 2010"

main :: IO ExitCode
main = do
    cmd <- cmdArgsRun $ cmdArgsMode tb
    time <- getCurrentTime
    let d = utctDay time
    let cl = cleanup cmd
    let cl_type = whatCleanup (whichType (frequency cmd) d)
    rc <- doBackup (show (whichType (frequency cmd) d)) d (dir cmd)
    case rc of
         ExitFailure e -> exitWith rc
         ExitSuccess -> if cl
                           then case cl_type of
                                    Nothing -> exitWith rc
                                    Just f -> doCleanup (last (splitDirectories (dir cmd))) f >> exitWith rc
                        else exitWith rc

whichType :: Frequency -> Day -> Frequency
whichType f d = check f
    where check Auto
              | month_day == 1 = Monthly -- first day of the month
              | week_day == 0 = Weekly   -- first day of the week
              | otherwise = Daily
          check _ = f
          (_, _, month_day) = toGregorian d
          (_, week_day) = sundayStartWeek d

