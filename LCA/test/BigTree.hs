module BigTree where

import Graph

bigTreeStart = 1
bigTreeEnd = 63
bigTreeList = [bigTreeEnd, (bigTreeEnd - 1) .. bigTreeStart] :: [Int]
bigTree = treeFromList bigTreeList
