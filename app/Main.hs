module Main where

import Data.List (foldl')
import Data.Maybe (isJust,fromJust,mapMaybe)
import Data.Tree (Tree(..))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import System.Random (StdGen, getStdGen, randomR)

import HydraLogic (change)
import TreeDraw (drawT)
import Trees

background   = black
maincolor    = aquamarine
leavecolor   = orange
initialscale = 70
offrange     = (-0.32,0.32) :: (Float,Float) -- Both should be smaller (in absolute value) than .4

getClosest :: Float -> StdGen -> Tree (a,Float) -> (Float,Float) -> (a,Float,StdGen)
getClosest poff g tree pos = f poff g 0 0 tree
  where
    d (x,y) (j,k) = sqrt ((x-j)^2 + (y-k)^2)
    f poff g depth acc (Node (a,x) []) = (a,d (x+acc+poff,depth) pos, snd $ randomR offrange g)
    f poff g depth acc (Node (a,x) as) = foldl h (a,100,g') as
      where
        (off,g')    = randomR offrange g
        h (b,d,g) n = let (c,d',g') = f off g (depth+1) (x+acc) n
                        in if d' < d then (c,d',g') else (b,d,g')

getDrawing :: Eq a => Float -> StdGen -> Float -> Float -> Float -> Tree (a,Float) -> ([Picture],StdGen)
getDrawing poff g scale x y (Node (l,acc) st)  =
  (lines ++ node : r, gen)
  where
    (r,gen)    = foldl' h ([],g') st
    h (ps,g) n = let (ps',g') = getDrawing off g scale (x+acc) (y+1) n
                   in (ps++ps',g')
    x'       = x+acc
    (off,g') = randomR offrange g
    pics     = if st == [] then [Color maincolor (ThickCircle 3 1),Color leavecolor (circleSolid 3)]
                           else [Color maincolor (circleSolid 4)]
    node     = Translate (scale*(x'+poff)) (scale*y) (Pictures pics)
    lines    = map (\(Node (_,acc') _) -> Color maincolor $ Line [(scale*(x'+poff),scale*y), (scale*(x'+acc'+off), scale*(y+1))]) st

draw (i,tree,s,x,y,_,gen) = Scale s s $ Translate x y $ Pictures (fst (getDrawing 0 gen initialscale 0 0 (drawT tree)))

hevent (EventKey (MouseButton WheelDown) _ _ _) (i,tree,s,x,y,desp,gen) = (i,tree,s*0.95,x,y,desp,gen)
hevent (EventKey (MouseButton WheelUp) _ _ _) (i,tree,s,x,y,desp,gen)   = (i,tree,s*1.05,x,y,desp,gen)
hevent (EventMotion (x1,y1)) (i,tree,s,x,y,desp,gen) =
  case desp of
     Just (x0,y0) -> (i,tree,s,x+(x1-x0)/s,y+(y1-y0)/s,Just (x1,y1),gen)
     Nothing      -> (i,tree,s,x,y,desp,gen)
hevent (EventKey (MouseButton LeftButton) Down _ (x0,y0)) (i,tree,s,x,y,desp,gen)
  | d < 0.2   = (i+1,change (i+1) a tree,s,x,y,Nothing,g')
  | otherwise = (i,tree,s,x,y,Just (x0,y0),gen)
  where
    (a,d,g') = getClosest 0 gen (drawT tree) (((x0/s) - x)/initialscale,((y0/s) - y)/initialscale)
hevent (EventKey (MouseButton LeftButton) Up _ _) (i,tree,s,x,y,_,gen) = (i,tree,s,x,y,Nothing,gen)
hevent _ s = s

update _ s = s

main = getStdGen >>= \gen -> play FullScreen background 0 (0,examplePaper,1,0,-300,Nothing,gen) draw hevent update
