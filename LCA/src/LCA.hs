module LCA where

import Data.List (unfoldr)
type Id = Int

data Tree
  = Bin {-# UNPACK #-} !Id Tree Tree
  | Tip {-# UNPACK #-} !Id
  deriving (Show, Read)

instance Eq Tree where
  Tip a     == Tip b     = a == b
  Bin a _ _ == Bin b _ _ = a == b
  _         == _         = False

data Path
  = Nil
  | Cons {-# UNPACK #-} !Int
         {-# UNPACK #-} !Int
         Tree
         Path
  deriving (Show, Read)

instance Eq Path where
  Nil == Nil = True
  Cons _ _ s _ == Cons _ _ t _ = s == t

size :: Path -> Int
size Nil = 0
size (Cons n _ _ _) = n

consT :: Int -> Tree -> Path -> Path
consT w t ts = Cons (w + size ts) w t ts

cons :: Id -> Path -> Path
cons k (Cons n w t (Cons _ w' t2 ts))
  | w == w' = Cons (n + 1) (2 * w + 1) (Bin k t t2) ts
cons k ts = Cons (size ts + 1) 1 (Tip k) ts

uncons :: Path -> Maybe (Id, Path)
uncons Nil = Nothing
uncons (Cons _ _ (Tip k) ts)     = Just (k, ts)
uncons (Cons _ w (Bin k l r) ts) = Just (k, consT w2 l (consT w2 r ts))
  where w2 = div w 2

keep :: Int -> Path -> Path
keep _ Nil = Nil
keep k xs@(Cons n w t ts)
  | k >= n = xs
  | otherwise = case compare k (n - w) of
     GT -> keepT (k - n + w) w t ts
     EQ -> ts
     LT -> keep k ts

keepT :: Int -> Int -> Tree -> Path -> Path
keepT n w (Bin _ l r) ts = case compare n w2 of
  LT -> keepT n w2 r ts
  EQ -> consT w2 r ts
  GT | n == w - 1 -> consT w2 l (consT w2 r ts)
     | otherwise -> keepT (n - w2) w2 l (consT w2 r ts)
 where w2 = div w 2
keepT _ _ _ ts = ts

lca' :: Path -> Path -> Path
lca' h@(Cons _ w x xs) (Cons _ _ y ys)
  | x == y = h
  | xs == ys = lcaT w x y ys
  | otherwise = lca' xs ys
lca' _ _ = Nil

lcaT :: Int -> Tree -> Tree -> Path -> Path
lcaT w (Bin _ la ra) (Bin _ lb rb) ts
  | la == lb = consT w2 la (consT w2 ra ts)
  | ra == rb = lcaT w2 la lb (consT w ra ts)
  | otherwise = lcaT w2 ra rb ts
  where w2 = div w 2
lcaT _ _ _ ts = ts

lca :: Path -> Path -> Path
lca xs ys = case compare nxs nys of
  LT -> lca' xs (keep nxs ys)
  EQ -> lca' xs ys
  GT -> lca' (keep nys xs) ys
 where
  nxs = size xs
  nys = size ys

fromList :: [Int] -> Path
fromList = foldr cons Nil

toList :: Path -> [Int]
toList = unfoldr uncons

xs = fromList [6,4,3,2,1]
ys = fromList [5,3,2,1]
