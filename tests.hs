{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (sort)

import Test.Framework
import Test.Framework.Skip
import Test.Framework.Providers.HUnit
import qualified Test.Framework.Providers.QuickCheck2 as Q
import qualified Test.Framework.Providers.SmallCheck as S
import Test.HUnit

main = defaultMain
    [testGroup "skip"
        [skip $ testCase "testCase" $ assertBool "This is totally going to fail..." False
        ,skip $ testGroup "testGroup"
            [testCase "child testCase" $ error "Did I do that?"
            ,Q.testProperty "child testProperty (quickcheck)" (\(x :: Int) -> x * 0 == x)
            ,S.testProperty "child testProperty (smallcheck)" (\(x :: Int) -> x * 0 == x)
            ]
        ,skip $ Q.testProperty "testProperty (quickcheck)" (\(x :: [Int]) -> x == sort x)
        ,skip $ S.testProperty "testProperty (smallcheck)" (\(x :: [Int]) -> x == sort x)
        ]
    ,skipTestCase "skipTestCase" $ assertFailure "Uh oh, I failed!"
    ,skipTestGroup "skipTestGroup"
        [testCase "child testCase" $ assertFailure "I would be so ashamed if somebody noticed me failing!"
        ,Q.testProperty "child testProperty (quickcheck)" False
        ,S.testProperty "child testProperty (smallcheck)" False
        ,testGroup "child testGroup"
            [Q.testProperty "grandchild testProperty (quickcheck)" False
            ,S.testProperty "grandchild testProperty (smallcheck)" False
            ,testCase "grandchild testCase" $ False @?= True
            ]
        ]
    ,skipTestProperty "skipTestProperty" (\(x :: Int) -> x /= x)
    ]
