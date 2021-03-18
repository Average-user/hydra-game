module Trees where

import Data.Tree

printT :: Show a => Tree a -> IO ()
printT = putStrLn . drawTree . fmap show

examplePaper :: Tree Int
examplePaper = Node 0 [ Node 6 [ Node 10 [ Node 11 []
                                         , Node 12 []
                                         , Node 13 []]
                               , Node 7 [ Node 8 [ Node 9 []]]]
                      , Node 1 [ Node 5 []
                               , Node 2 [ Node 3 []
                                        , Node 4 []]]]
linear 1 = Node 1 []
linear n = Node n [linear (n-1)]
