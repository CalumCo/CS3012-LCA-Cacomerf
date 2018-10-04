
module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import LCA
{- Hunit Tests -}

-- Test to see if the BST can be created in a normal case
BSTtest = BST (1,2)
BSTtest = BST (BSTtest,3)
Tests = test [ "test1" ~: "for (BST create 1)" ~: (True) ~=? (bstContains (BSTtest , 1) ),
               "test2" ~: "for (BST create 2)" ~: (True) ~=? (bstContains (BSTtest , 2) ),
               "test3" ~: "for (BST create 3)" ~: (True) ~=? (bstContains (BSTtest , 3) )]

{-- Test to see if BST return correct error for a empty tree
test3 :: Test
Test3 = testCase (assertBool "for (BST)")

-- Test if bstContains can correctly find a value in a given BST
test2 :: Test
Test2 = testCase (assertBool "for (bstContains)")

-- Test if bstContains will return false correctly
test3 :: Test
Test3 = testCase (assertEqual "for (bstContains)")

main = defaultMain tests
-}
