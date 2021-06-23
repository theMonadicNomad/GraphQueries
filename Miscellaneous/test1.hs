import Data.Map (Map)
import qualified Data.Map as Map


data PrePostRef = Prelabel Int | Postlabel Int
    deriving (Eq, Show)

{- test :: PrePostRef -> Int
test (pc i) = case pc of
    Prelabel -> i+1
    Postlabel -> i+10
 -}

test1 :: PrePostRef -> Int
test1 d = show d 
    


data Graph a = Graph [(a, [a])] 
  deriving Show

type GraphMap a = Map a [a]

graph2 :: Graph Char
graph2 = Graph
    [ (  'a',  [  'b',  'c']  ),
      (  'b', [   'd',  'e',  'f' ] ),
      (  'c', [  'h' ] ),
      (  'd', [  'k' ] ),
      (  'e', [  'g',   'h' ] ),
      (  'f', [  'g' ] ),
      (  'g', [  'i',  'j' ] ),
      (  'h', [  'k' ] ),
      (  'i', [  'k' ] ),
      (  'j', [  'k' ] ),
      (  'k', [] )
    ]
