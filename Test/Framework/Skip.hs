{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Framework.Skip
    ( skip
    , skipTestCase
    , skipTestGroup
    , skipTestProperty
    ) where

import Control.Arrow (first)
import Test.Framework.Providers.API ((:~>)(..),Test(..),Testlike(..),TestResultlike(..))
import Data.Typeable (Typeable)

data Skip = Skip deriving (Typeable)

instance Show Skip where show _ = "skipped"

instance TestResultlike Skip Skip where
    testSucceeded _ = True

instance Testlike Skip Skip Skip where
    runTest _ _ = return (Finished Skip,return ())
    testTypeName _ = "Skipped"

-- | Causes the given test or test suite to be skipped, though it will still show up as being "skipped" when the suite is run.  If given a test suite, 'skip' recursively applies it itself to the child tests and suites, all of which (except for the groups) will show up as being 'skipped' when the test suite is run.
skip :: Test -> Test
skip (Test name _) = Test name Skip
skip (TestGroup name tests) = TestGroup name (map skip tests)
skip (PlusTestOptions options test) = PlusTestOptions options (skip test)
skip (BuildTestBracketed bracket) = BuildTestBracketed (fmap (first skip) bracket)

-- | Instead of using the 'skip' function, you can change 'testCase' to 'skipTestCase' to cause the test to be skipped.
skipTestCase :: String -> a -> Test
skipTestCase name _ = Test name Skip

-- | Instead of using the 'skip' function, you can change 'testGroup' to 'skipTestGroup' to cause the test suite to be skipped.
skipTestGroup :: String -> [Test] -> Test
skipTestGroup name = TestGroup name . map skip

-- | Instead of using the 'skip' function, you can change 'testProperty' to 'skipTestProperty' to cause the test to be skipped.  (Note that this work regardless of whether you are using QuickCheck or SmallCheck for this particular test.)
skipTestProperty :: String -> a -> Test
skipTestProperty = skipTestCase
