module Main where

import Graph
import ArbitraryGraph
import BigGraph
import BigTree
import Data.Maybe
import SampleTree
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

constructorTests = testGroup "Constructors"
    [ testCase "Empty == Empty" (Empty @=? (Empty :: Graph Int))
    , QC.testProperty "finalNode a == Node a Empty Empty" testfinalNode
    , testCase "sampleTree converts to correct string"
        (show sampleTree @?=
            "Node 0 (Node 3 Empty (Node 5 (Node 100 Empty (Node 1 " ++
            "Empty Empty)) (Node 7 Empty (Node 42 (Node 451 Empty " ++
            "Empty) Empty)))) (Node 8 Empty (Node 43 (Node 500 Empty " ++
            "Empty) Empty))")
    , testCase "treeFromList [0..5]"
        (treeFromList [0..5] @?= treeToFive)
    , testCase "treeFromList [] == Empty"
        (treeFromList [] @?= (Empty :: Graph Int))
    ]

functorTests = testGroup "Functor"
    [ testCase "fmap (*2) sampleTree produces doubled graph"
        (fmap (*2) sampleTree @?= sampleTreeDoubled)
    , QC.testProperty "fmapping inverse functions produces the input"
        fmapInversions
    ]

insertElementTests = testGroup "Node Insertion"
    [ testCase "Empty insertion"
        (insertElement Empty [GraphLeft, GraphRight] 3 @?= finalNode 3)
    , testCase "Leaf left insertion"
        (insertElement (finalNode 3) [GraphLeft] 4 @?= Node 3 (finalNode 4) Empty)
    , testCase "Leaf right insertion"
        (insertElement (finalNode 3) [GraphRight] 4 @?= Node 3 Empty (finalNode 4))
    , testCase "Intermediate Node insertion"
        (insertElement (Node 3 (finalNode 4) Empty) [GraphLeft] 5 @?=
            Node 3 (Node 5 (finalNode 4) Empty) Empty)
    , QC.testProperty "tree != insertElement <path> <el> tree"
        testInsertElementEq
    ]

pathFollowTests = testGroup "Path & Follow"
    [ testCase "Nonexistent path is Nothing"
        (paths sampleTreeDoubled 23456789 @?= [])
    , testCase "path to 451 in sampleTree"
        (paths sampleTree 451 @?= [[GraphLeft, GraphRight, GraphRight, GraphRight, GraphLeft]])
    , testCase "following path to 451 in sampleTree gives Just 451"
        (follow sampleTree (head $ paths sampleTree 451) @?= Just 451)
    , QC.testProperty "and ((== Just el) $ fmap (follow root) (path root el))"
        testFollowPath
    ]

data GraphType = Tree | Graph deriving Eq

lcaTests = testGroup "LCA" (

    (map (\(expect, t, graphName, graph, a, b) -> testCase ("LCA of " ++
    (if t == Tree then "tree" else "graph") ++ " " ++ graphName ++
    " with els: " ++ show a ++ " and " ++ show b ++ ", is " ++ show expect)
        ((expect :: Maybe Int) @=? lca graph a b)) $

    (map (\(exp, tree, a, b) -> (exp, Tree, show tree, FullGraph [tree], a, b))
    -- (e, t, a, b)
    [ (Nothing, Empty, 1, 2)
    , (Just 1, Node 3 Empty (finalNode 1), 1, 1)
    , (Just 3, Node 3 Empty (finalNode 1), 3, 1)
    ]) ++

    (map (\(exp, a, b) -> (exp, Tree, "sampleTree", FullGraph [sampleTree], a, b))
    -- (e, a, b)
    [ (Nothing, 300, 200)
    , ( Just 5,   1, 451)
    ]) ++

    (map (\(a, b, e) -> (Just e, Tree, "bigTree", FullGraph [bigTree], a, b))
    --( a,  b,  e)
    [ ( 1,  2, 33)
    , ( 4,  5, 57)
    , ( 8,  9, 61)
    , (16, 17, 63)
    , (24, 25, 62)
    , (35, 50, 50)
    , (37, 37, 37)
    , (37, 39, 58)
    , (63, 63, 63)
    , (60,  9, 63)
    ]) ++

    (map (\(a, b, e) -> (e, Graph, "bigGraph", bigGraph, a, b))

    [ (16, 25, Just  25)
    , (58, 60, Just  62)
    , (28, 40, Just  55)
    , (12, 16, Just  58)
    , (57, 62, Just 100)
    , (49, 56, Just 100)
    , (38, 34, Just  61)
    ])) ++

    [ QC.testProperty
        "LCA of (abs <num1> + 64) <num2> in bigTree is Nothing" testNoLCA
    ])

tests :: TestTree
tests = testGroup "Graph"
    [ constructorTests
    , functorTests
    , pathFollowTests
    , insertElementTests
    , lcaTests
    ]

treeToFive =
    Node 0
        (Node 1
            (finalNode 3)
            (finalNode 4)
        )
        (Node 2
            (finalNode 5)
            Empty
        )

allGraphs :: (Graph a -> Bool) -> [Graph a] -> Bool
allGraphs f gs = all f gs

testInsertElementEq :: FullGraph Int -> GraphPath -> Int -> Bool
testInsertElementEq (FullGraph gs) p a =
    allGraphs (\g -> g /= insertElement g p a) gs

testNoLCA :: Int -> Bool
testNoLCA aa =
    let a = abs aa + bigTreeEnd + 1 in
        isNothing (lca (FullGraph [bigTree]) bigTreeStart a)

testfinalNode :: Int -> Bool
testfinalNode a = Node a Empty Empty == finalNode a

testFollowPath :: FullGraph Int -> [GraphDir] -> Int -> Bool
testFollowPath (FullGraph gs) p a =
    allGraphs lead gs
    where
        lead :: Graph Int -> Bool
        lead g = let newGraph = insertElement g p a in
            and (fmap ((== Just a) . follow newGraph) (paths newGraph a))

fmapInversions :: FullGraph Int -> Bool
fmapInversions (FullGraph gs) =
    allGraphs (\g -> g == fmap ((* (-1)) . (* (-1))) g) gs
