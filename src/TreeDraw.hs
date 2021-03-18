module TreeDraw (drawT) where

import Data.Tree (Tree(..))
import Control.Arrow ((***))
import Data.List (foldl')

type Coord = (Float, Float)

translateT :: Float -> Tree (a, Float) -> Tree (a, Float)
translateT k (Node (a,p) as) = Node (a, p + k) as

translateP :: Float -> [Coord] -> [Coord]
translateP k = map ((+k) *** (+k))

merge :: [Coord] -> [Coord] -> [Coord]
merge [] ys = ys
merge xs [] = xs
merge ((x,_):xs) ((_,y):ys) = (x, y) : merge xs ys

minD :: [Coord] -> [Coord] -> Float
minD ((_,p):ps) ((q,_):qs) = max (minD ps qs) (p - q + 1)
minD _          _          = 0

minDL :: [[Coord]] -> [Float]
minDL = f []
  where
    f _   []     = []
    f acc (x:xs) = let d = minD acc x
                    in d : f (merge acc (translateP d x)) xs

minDR :: [[Coord]] -> [Float]
minDR = reverse . map (0-) . minDL . map (map (\(p, q) -> (negate q, negate p))) . reverse

wrap :: [[Coord]] -> [Float]
wrap xs = zipWith (\x y -> (x+y)/2) (minDL xs) (minDR xs)

drawT :: Tree a -> Tree (a, Float)
drawT = fst . f
  where
 f (Node a as) = (ntree, nls)
      where
        (trees, ls) = unzip (map f as)
        ps          = wrap ls
        nls         = (0,0) : foldl' merge [] (zipWith translateP ps ls)
        ntree       = Node (a, 0) (zipWith translateT ps trees)
