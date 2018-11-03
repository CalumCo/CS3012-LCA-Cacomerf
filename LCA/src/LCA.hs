module LCA where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

data Graph a = Empty
              | Blank (Graph a) (Graph a)
              | Node a (Graph a) (Graph a)
                deriving (Eq, Ord, Show)

data GraphDir = GraphLeft
              | GraphRight
              deriving (Eq, Ord, Show)

newtype FullGraph a = FullGraph [Graph a] deriving (Eq, Ord, Show)

type GraphPath = [GraphDir]

lastNode :: a -> Graph a
lastNode a = Node a Empty Empty

instance Functor Graph where
fmap fun Empty = Empty
fmap fun (Blank l r) = Blank (fmap fun l) (fmap fun r)
fmap fun (Node x l r) = Node (fun x) (fmap fun l) (fmap fun r)

paths :: Eq a => Graph a -> a -> [GraphPath]
paths Empty _ = []
paths n el = reverse <$> pathsInt n el []

pathsInt :: Eq a => Graph a -> a -> GraphPath -> [GraphPath]
pathsInt (Blank l r) el p = amassPaths l r el p
pathsInt (Node a l r) el p =
    (if a == el then (p:) else id) $ amassPaths l r el p
pathsInt _ _ _ = []

amassPaths :: Eq a => Graph a -> Graph a -> a -> GraphPath -> [GraphPath]
amassPaths l r el p =
    let left  = pathsInt l el (GraphLeft  : p)
        right = pathsInt r el (GraphRight : p)
    in left ++ right

nodesAlongPath :: Graph a -> GraphPath -> [Graph a]
nodesAlongPath n [] = [n]
nodesAlongPath Empty _ = [Empty]
nodesAlongPath (Blank l r) (GraphLeft : xs) = Blank l r : nodesAlongPath l xs
nodesAlongPath (Blank l r) (GraphRight : xs) = Blank l r : nodesAlongPath r xs
nodesAlongPath (Node el l r) (GraphLeft : xs) = Node el l r : nodesAlongPath l xs
nodesAlongPath (Node el l r) (GraphRight : xs) = Node el l r : nodesAlongPath r xs

nodesToEls :: [Graph a] -> [a]
nodesToEls [] = []
nodesToEls (Node el _ _ : xs) = el : nodesToEls xs
nodesToEls (x : xs) = nodesToEls xs

elsAlongPath :: Graph a -> GraphPath -> [a]
elsAlongPath g p = nodesToEls $ nodesAlongPath g p

followNode :: Graph a -> GraphPath -> Maybe (Graph a)
followNode g p = tryGetLast $ nodesAlongPath g p
    where
        tryGetLast [] = Nothing
        tryGetLast lst | length lst < length p = Nothing
        tryGetLast lst = Just (last lst)

follow :: Graph a -> GraphPath -> Maybe a
follow graph path = case followNode graph path of
    Just (Node a _ _) -> Just a
    _ -> Nothing

-- if path is longer, inserts as a leaf on the edge
-- if path is shorter, node is inserted, previous graph moved to left
replaceNode :: (Graph a -> Graph a) -> Graph a -> GraphPath -> Graph a
replaceNode f Empty _ = f Empty
replaceNode f n [] = f n
replaceNode f (Blank l r) (GraphLeft : xs) = Blank (replaceNode f l xs) r
replaceNode f (Blank l r) (GraphRight : xs) = Blank l (replaceNode f r xs)
replaceNode f (Node a l r) (GraphLeft : xs) = Node a (replaceNode f l xs) r
replaceNode f (Node a l r) (GraphRight : xs) = Node a l (replaceNode f r xs)

insertElement :: Graph a -> GraphPath -> a -> Graph a
insertElement g p el = replaceNode f g p
    where
    f n = Node el n Empty

insertNode :: Graph a -> GraphPath -> Graph a -> Graph a
insertNode g p subGraph = replaceNode (Blank subGraph) g p

graphToList :: Graph a -> [Graph a]
graphToList (Node el l r) = Node el l r : graphToList l ++ graphToList r
graphToList (Blank l r) = graphToList l ++ graphToList r
graphToList Empty = [Empty]

treeFromList :: [a] -> Graph a
treeFromList lst = head $ formTree $ treePartition 1 lst
    where
        treePartition :: Int -> [a] -> [[a]]
        treePartition _ [] = []
        treePartition stage lst = take stage lst :
            treePartition (stage * 2) (drop stage lst)

        formTree :: [[a]] -> [Graph a]
        formTree = foldr treeLayer [Empty]

        treeLayer :: [a] -> [Graph a] -> [Graph a]
        treeLayer [] _ = []
        treeLayer (x : xs) [] = lastNode x : treeLayer xs []
        treeLayer (x : xs) [a] = Node x a Empty : treeLayer xs []
        treeLayer (x : xs) (y1 : y2 : ys) = Node x y1 y2 : treeLayer xs ys

lca :: Ord a => FullGraph a -> a -> a -> Maybe a
lca (FullGraph roots) el1 el2 =
    let
        ancestorsA = ancestors el1
        ancestorsB = ancestors el2
        sharedAncestors = Set.toList $ Set.intersection ancestorsA ancestorsB

        -- all posible paths from all roots to all sharedAncestors, we've
        -- concatenated the paths from different roots, because we only care
        -- about which ancestor has the longest minimum path length
        pathsToAncestors =
            map (\a -> concatMap (`paths` a) roots) sharedAncestors

        shortestPathLengths = map (minimum . map length) pathsToAncestors

        mostNestedAncestor = if null sharedAncestors then
            Nothing else
            Just (fst (maximumBy (\(_, a) (_, b) -> compare a b)
                (zip sharedAncestors shortestPathLengths)))
    in mostNestedAncestor
        where
            ancestors el = Set.fromList $
                roots >>= \root -> paths root el >>= elsAlongPath root
