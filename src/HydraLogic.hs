module HydraLogic (change) where

import Data.Tree
import Data.Maybe
import Data.List

enumTree :: Int -> Tree a -> Tree Int
enumTree = f
  where
    f n (Node _ as) = Node n (g (n + 1) as)
    g _ []          = []
    g n (x:xs)      = f n x : g (n + length x) xs

change' :: Int -> Int -> Tree Int -> Tree Int
change' k a (Node x xs) = case toDel of
                            Nothing -> Node x (map (change' k a) xs)
                            Just _  -> newt
  where
    toDel = listToMaybe
          $ mapMaybe (\(Node y ys) -> fmap ((,) (Node y ys)) (find (\(Node z _) -> z == a) ys)) xs
    newt  = Node x (xs >>= f)
      where
        (Node y ys, Node z _)     = fromJust toDel
        nt                        = Node y (filter (\(Node r _) -> r /= z) ys)
        f (Node r rs) | r == y    = nt : replicate k nt
                      | otherwise = [Node r rs]

change :: Int -> Int -> Tree Int -> Tree Int
change k a (Node x xs) = case f xs of
                           Nothing -> enumTree 0 (change' k a (Node x xs))
                           Just rs -> Node x rs
  where
    f []                           = Nothing
    f (Node y ys : rs) | y == a    = Just rs
                       | otherwise = fmap (Node y ys :) (f rs)
