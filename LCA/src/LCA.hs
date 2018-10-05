module LCA where

data BST a = Node a (BST a) (BST a) | Nil deriving Show
--Given a binary search tree and a value returns true if value is contained in said 

bstContains :: Ord a => BST a -> a -> Bool
bstContains Nil _ = False
bstContains (Node r left right) value =
  if value == r then True
  else if value < r then bstContains left value
  else bstContains right value

--Given a BST and two values the following function with return the Lowest common ancestor

bstLCA :: Ord a => BST a -> a -> a -> Maybe (BST a)

--If the BST is empty the following will handle it

bstLCA Nil _ _ = Nothing
bstLCA root@(Node r left right) v1 v2 =
  -- If both values are on the same side, descend into that side.
  if v1 < r && v2 < r then bstLCA left v1 v2
  else if v1 > r && v2 > r then bstLCA right v1 v2
  
  -- If the root value matches at least one node, search the tree for the other
  -- and return this node if we find it.
  else if v1 == r then
       if bstContains root v2 then Just root else Nothing
  else if v2 == r then
       if bstContains root v1 then Just root else Nothing
 
  -- Otherwise, the values must be on opposite sides of the tree, so search
  -- for both and return the current node if we find them.
  else if (bstContains root v1) && (bstContains root v2) then Just root
  else Nothing