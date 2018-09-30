module LCA where

data BST a = Node a (BST a) (BST a) | nil deriving show

--Given a binary search tree and a value returns true if value is contained in said bst
bstContains :: Ord a => BST a -> a -> Bool
bstContains Nil _ = False
bstContains (Node r left right) value =
  if value == r then True
  else if value < r then bstContains left value
  else bstContains right value
