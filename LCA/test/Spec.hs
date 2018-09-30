
module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import LCA
{- Hunit Tests -}

-- Test to see if the BST can be created in a normal case
test1 :: Test
test1 = testCase (assertEqual "for (BST )")

-- Test to see if BST return correct error for a empty tree
test3 :: Test
Test3 = testCase (assertEqual "for (BST)")

-- Test if bstContains can correctly find a value in a given BST
test2 :: Test
Test2 = testCase (assertEqual "for (bstContains)")

-- Test if bstContains will return false correctly
test3 :: Test
Test3 = testCase (assertEqual "for (bstContains)")
