{-# OPTIONS_GHC -fno-warn-orphans  #-}

import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.List
import TarsnapBackup
import Data.Time.Calendar

instance Arbitrary Frequency where
    arbitrary = elements [Daily, Weekly, Monthly, Auto]



tests = [
    testGroup "blah" [
        testProperty "blah_1" prop_correct_auto_frequency,
        testProperty "blah_3" prop_list_cleanup
        ]
    ]

main :: IO ()
main = defaultMain tests

-- this is rubbish but its a start. fromGregorian will clip the days to fit
instance Arbitrary Day where
    arbitrary = do
        year <- elements [2000 .. 2099]
        month <- elements [1..12]
        day <- elements [1..31]
        return $ fromGregorian year month day

prop_correct_auto_frequency :: Frequency -> Day -> Bool
prop_correct_auto_frequency f d = whichType f d == f


genFiles :: Gen String
genFiles = oneof [ listOf $ elements $ ['a'..'z']++['A'..'Z']++['0'..'9'] ]

prop_list_cleanup :: Frequency -> Day -> Property
prop_list_cleanup f d = forAll genFiles $ \fn ->
    getCleanupList fn f (makeArch f d fn) == []
    where makeArch f' d' n' = intersperse "-" [n', show f',show d'] 