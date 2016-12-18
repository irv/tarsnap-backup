{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE RankNTypes #-}

import Test.QuickCheck
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Data.List
import TarsnapBackup
import Data.Time.Calendar

instance Arbitrary Frequency where
  arbitrary = elements [Daily, Weekly, Monthly]

tests :: [Test.Framework.Test]
tests =
  [ testGroup
      "blah"
      [ testProperty "blah_1" prop_correct_auto_frequency
      , testProperty "blah_3" prop_list_cleanup
      , testCase "testSomething" testSomething
      ]
  ]

main :: IO ()
main = defaultMain tests

-- this is rubbish but its a start. fromGregorian will clip the days to fit
instance Arbitrary Day where
  arbitrary = do
    year <- elements [2000 .. 2099]
    month <- elements [1 .. 12]
    day <- elements [1 .. 31]
    return $ fromGregorian year month day

prop_correct_auto_frequency :: Frequency -> Day -> Bool
prop_correct_auto_frequency f d = whichType f d == f

genFiles :: Gen String
genFiles =
  oneof [listOf $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']]

mkBackupList
  :: forall a a1.
     (Show a1, Show a)
  => a -> a1 -> [Char] -> [[Char]]
mkBackupList f d n = intersperse "-" [n, show f, show d]

prop_list_cleanup :: Frequency -> Day -> Property
prop_list_cleanup f d =
  forAll genFiles $ \fn -> null (getCleanupList fn f (mkBackupList f d fn))

testSomething :: Assertion
testSomething = do
  let l = mkBackupList Daily (fromGregorian 2016 12 12) "t"
  l @?= l
