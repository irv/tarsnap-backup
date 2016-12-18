# tarsnap-backup

[![Build Status](https://travis-ci.org/irv/tarsnap-backup.svg?branch=master)](https://travis-ci.org/irv/tarsnap-backup)

This application manages tarsnap backups, and is expected to be run via cron,
although doesn't have to be. In its default mode, it should take 6 daily backups
in a row until Sunday, at which point it should take a weekly backup and remove
those daily backups. It will carry that on until the first day of the month,
when it will take a monthly backup, clearing up any weekly backups. You can
force a backup of a specific type and/or disable the clean up.

The archives will be created in the following format:

Basename-Frequency-YYYY-MM-DD

So, if i was backing up my Mail directory daily:

Mail-Daily-2010-09-18

Or the file world_domination.txt weekly:

world_domination-Weekly-2010-09-19

Requirements:
tarsnap
a tarsnap.rc (system or user level for the user executing it)
GHC 6.12

cabal should sort out any dependencies from hackage.

## Error Codes

The Error Code should be passed through from tarsnap, along with any error text.
Handy tip: code 127 means the tarsnap executable can't be found in the $PATH. On
Unix systems by default tarsnap will be in /usr/local/bin unless you ran
configure with the --prefix option. /usr/local/bin *is* in the path for cron at
system level (on my Debian system), but when running my user's crontab, it
doesn't appear to be. My suggestion is to install tarsnap into /usr not
/usr/local.

# Cron Examples

Note: these examples pipe the output to logger so errors show up in syslog or
however you have your system configured (I don't run a local MTA on my desktop
machine, for example, so cron can't email about failures)

To automatically back things up, at 4AM every day:

0 04 * * * /usr/bin/tarsnap-backup /home/irv/Mail 2>&1 |/usr/bin/logger -t
tarsnap-backup

This will backup my Mail directory every day at 4AM, producing 6 daily backups,
then on a Sunday a weekly backup and remove the daily ones. Then on the 1st of
the month, a monthly backup, deleting all the weekly backups.

If you just want to force a type of backup?

* * * * 0 /usr/bin/tarsnap-backup -f=Weekly /home/irv/Documents 2>&1
|/usr/bin/logger -t tarsnap-backup

Once a week, backup my documents folder. You might want to disable  clean up if
you only ever run a weekly (or monthly) backup; this will save you a tiny amount
of time and bandwidth.

If you don't want to lose the granularity of the daily backup (and have it merge
into a weekly then monthly), you can force the frequency to daily.

Another handy tip: make sure you have a blank line at the end of your crontab or
it won't run!
