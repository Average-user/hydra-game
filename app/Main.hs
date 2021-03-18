module Main where

import Data.CircularList (fromList,rotR,rotL,focus)
import Data.Maybe (isJust,fromJust,mapMaybe)
import qualified Data.Set as S
import Data.Tree (Tree(..))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import HydraLogic (change)
import TreeDraw (drawT)
import Trees

background = black
maincolor = aquamarine
selected = orange

getDrawing :: Eq a => a -> Float -> Float -> Float -> Tree (a,Float) -> [Picture]
getDrawing hl scale x y (Node (l,acc) st)  =
  lines ++ node : concatMap (getDrawing hl scale (x+acc) (y+1)) st
  where
    x'  = x+acc
    color = if hl == l then selected else maincolor
    node  = Translate (scale*x') (scale*y) (Color color (circleSolid 4))
    lines = map (\(Node (_,acc') _) -> Color maincolor $ Line [(scale*x',scale*y), (scale*(x'+acc'), scale*(y+1))]) st

draw (i,tree,ls,s,x,y,_) = Scale s s $ Translate x y $ Pictures (getDrawing (fromJust $ focus ls) 50 0 0 (drawT tree))

hevent (EventKey (SpecialKey KeyRight) Down _ _) (i,tree,ls,s,x,y,keys) = (i,tree,rotR ls,s,x,y,keys)
hevent (EventKey (SpecialKey KeyLeft) Down _ _)  (i,tree,ls,s,x,y,keys) = (i,tree,rotL ls,s,x,y,keys)
hevent (EventKey (SpecialKey KeyEnter) Down _ _) (i,tree,ls,s,x,y,keys) = (i+1, nt, fromList (leaves nt),s,x,y,keys)
  where
    nt = change (i+1) (fromJust $ focus ls) tree
hevent (EventKey (MouseButton WheelDown) _ _ _) (i,tree,ls,s,x,y,keys) = (i,tree,ls,s*0.95,x,y,keys)
hevent (EventKey (MouseButton WheelUp) _ _ _) (i,tree,ls,s,x,y,keys) = (i,tree,ls,s*1.05,x,y,keys)
hevent (EventKey k Down _ _) (i,tree,ls,s,x,y,keys)  = (i,tree,ls,s,x,y,S.insert k keys)
hevent (EventKey k Up _ _) (i,tree,ls,s,x,y,keys)    = (i,tree,ls,s,x,y,S.delete k keys)
hevent _ s                                           = s


update _  (i,tree,ls,s,x,y,keys) = foldr (flip f) (i,tree,ls,s,x,y,keys) keys
  where
    f (i,tree,ls,s,x,y,keys) (Char 'j') = (i,tree,ls,s,x-10,y,keys)
    f (i,tree,ls,s,x,y,keys) (Char 'l') = (i,tree,ls,s,x+10,y,keys)
    f (i,tree,ls,s,x,y,keys) (Char 'i') = (i,tree,ls,s,x,y+10,keys)
    f (i,tree,ls,s,x,y,keys) (Char 'k') = (i,tree,ls,s,x,y-10,keys)
    f s _ = s

main = play FullScreen background 30 (0,t,fromList (leaves t),1,0,-300,S.empty) draw hevent update
  where
    t = examplePaper
