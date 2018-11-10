module ArbitraryGraph where

import Data.List
import Control.Applicative
import Graph
import Test.Tasty.QuickCheck as QC

instance Arbitrary a => Arbitrary (Graph a) where
    arbitrary = sized arbitrarySizedGraph

instance (Eq a, Arbitrary a) => Arbitrary (FullGraph a) where
    arbitrary = arbGraph

instance Arbitrary GraphDir where
    arbitrary = fmap (\b -> if b then GraphLeft else GraphRight)
        (arbitrary :: Gen Bool)

arbitrarySizedGraph :: Arbitrary a => Int -> Gen (Graph a)
arbitrarySizedGraph n = fst <$> arbSizGra n

isEmpty :: Graph a -> Bool
isEmpty Empty = True
isEmpty _ = False

arbGraph :: (Arbitrary a, Eq a) => Gen (FullGraph a)
arbGraph = listOf arbitrary >>= mergeNodes . nub

arbNodeFromGraph :: Graph a -> Gen (Graph a)
arbNodeFromGraph = elements . graphToList

mergeNodes :: (Arbitrary a, Eq a) => [a] -> Gen (FullGraph a)
mergeNodes = mergeNodesInt [Empty]
    where
        -- ys are nodes we haven't used yet in the graph
        mergeNodesInt :: (Eq a) => [Graph a] -> [a] -> Gen (FullGraph a)
        mergeNodesInt xs [] = pure (FullGraph xs)
        mergeNodesInt xs ys = do
            childTree1 <- elements xs
            childTree2 <- elements xs
            child1 <- arbNodeFromGraph childTree1
            child2 <- arbNodeFromGraph childTree1
            root <- elements ys
            isBlank <- fmap (\i -> mod i 4 == 0) (arbitrary :: Gen Int)

            let newXs = filter (not . flip elem [child2, child2]) xs
            let newYs = filter (/= root) ys

            if isBlank then
                mergeNodesInt (Blank child1 child2 : newXs) ys else
                mergeNodesInt (Node root child1 child2 : newXs) newYs

{- Legacy graph generation, adapted from the old Tree generator -}
arbSizGra :: Arbitrary a => Int -> Gen (Graph a, [Graph a])
arbSizGra 0 = return (Empty, [Empty])
arbSizGra m = let m2 = floor (fromIntegral m * 0.8) in
    do
        el <- arbitrary
        llen <- elements $ 0 : replicate 3 m2
        rlen <- elements $ 0 : replicate 3 m2
        (ll, ls) <- arbSizGra llen
        (rr, rs) <- arbSizGra rlen
        l <- oneof [elements rs, return ll]
        r <- oneof [elements ls, return rr]
        blank <- arbitrary :: Gen Bool

        let n = if blank then
                   Blank l r else
                   Node el l r

        return (n, n : reduceEmpties ls rs)
            where
                reduceEmpties a b = filter (not . isEmpty) a ++
                                    filter (not . isEmpty) b
