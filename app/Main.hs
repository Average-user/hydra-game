module Main where

import Data.Maybe (isJust,fromJust,mapMaybe)
import Data.Tree (Tree(..))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List
import Data.Function

import HydraLogic (change)
import TreeDraw (drawT)
import Trees

background = black
maincolor = aquamarine
selected = orange
initialscale = 50

getClosest :: Tree (a,Float) -> (Float,Float) -> (a,Float)
getClosest tree pos = f 0 0 tree
  where
    d (x,y) (j,k) = sqrt ((x-j)^2 + (y-k)^2)
    f depth acc (Node (a,x) []) = (a,d (x+acc,depth) pos)
    f depth acc (Node (a,x) as) = minimumBy (compare `on` snd) (map (f (depth+1) (x+acc)) as)

getDrawing :: Eq a => Float -> Float -> Float -> Tree (a,Float) -> [Picture]
getDrawing scale x y (Node (l,acc) st)  =
  lines ++ node : concatMap (getDrawing scale (x+acc) (y+1)) st
  where
    x'  = x+acc
    node  = Translate (scale*x') (scale*y) (Color maincolor (circleSolid 4))
    lines = map (\(Node (_,acc') _) -> Color maincolor $ Line [(scale*x',scale*y), (scale*(x'+acc'), scale*(y+1))]) st

draw (i,tree,s,x,y,_) = Scale s s $ Translate x y $ Pictures (getDrawing initialscale 0 0 (drawT tree))
hevent (EventKey (MouseButton WheelDown) _ _ _) (i,tree,s,x,y,mon) = (i,tree,s*0.95,x,y,mon)
hevent (EventKey (MouseButton WheelUp) _ _ _) (i,tree,s,x,y,mon) = (i,tree,s*1.05,x,y,mon)
hevent (EventMotion (x0,y0)) (i,tree,s,x,y,mon) | mon       = (i,tree,s,x0,y0,mon)
                                                | otherwise = (i,tree,s,x,y,mon)
hevent (EventKey (MouseButton LeftButton) Down modif (x0,y0)) (i,tree,s,x,y,mon)
  | ctrl modif == Down = (i,tree,s,x,y,True)
  | otherwise          = (i,nt,s,x,y,False)
  where
    nt    = if d < 0.2 then change (i+1) a tree else tree
    (a,d) = getClosest (drawT tree) (((x0/s) - x)/initialscale,((y0/s) - y)/initialscale)
hevent (EventKey (MouseButton LeftButton) Up _ _) (i,tree,s,x,y,_) = (i,tree,s,x,y,False)
hevent _ s                                           = s

update _ s = s

main = play FullScreen background 0 (0,t,1,0,-300,False) draw hevent update
  where
    t = examplePaper
