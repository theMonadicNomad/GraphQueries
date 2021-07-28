{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Common where


import System.Random
import           Database.Daison
import           System.Environment
import           Data.Data
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad
import Control.Monad.IO.Class
import qualified System.IO as IO
import Test.QuickCheck 
import Test.QuickCheck.Monadic
import Data.Maybe (fromJust)
import System.Process (callProcess, callCommand)
import qualified Data.Bits as Bits
import Data.Int

type Nd = Key Labels
--  deriving (Eq, Ord, Read, Data)
type Ndc = Char
type Nds = [Char]
  --deriving (Eq, Ord, Read, Data)
type Hops = Set Nd
type Directs = Set Nd
--type Pre = Int
--type Post = Int
--type Graph =  [(Ndc, [Ndc])]
data Graph a = Graph [(a, [a])] 
  deriving Show

max_bound = maxBound :: Nd

type GraphMap a = Map a [a]
type Special = Bool
type Record = (Nd, Labels)
type Edges = [Nd]

data PrePostRef = PreLabel Nd | PostLabel Nd deriving (Eq,Show)


data Labels = Labels {
    tree_parent :: Nd,
    preL :: Nd,
    postL :: Nd,
    hops :: Hops,
    directs :: Directs,
    firstChild :: Nd,
    lastChild :: Nd,
    nextSibling :: Nd,
    lastSibling :: Nd
} deriving (Typeable, Data )

data X = X { nd :: Node,  
     edges :: Edges
} deriving (Typeable, Data, Show)



data Node = C Ndc | I Nd | S Nds deriving (Eq,Data, Ord, Show)

{- isChar :: Node -> Bool
isChar (C _) = True
isChar _ = False

isInt :: Node -> Bool
isInt (I _) = True
isInt _ = False -}


graph11 :: Graph Node
graph11 = Graph
    [ (C 'a', [ C 'b', C 'c'] ),
      (C 'b', [ C 'c'] ),
      (C 'c', []),
      (C 'd', [C 'e']),
      (C 'e', [C 'f'] ),
      (C 'f', [])
    ]
 

graph2 :: Graph Node
graph2 = Graph
    [ ( C 'a',  [ C 'b', C 'c']  ),
      ( C 'b', [  C 'd', C 'e', C 'f' ] ),
      ( C 'c', [ C 'h' ] ),
      ( C 'd', [ C 'k' ] ),
      ( C 'e', [ C 'g',  C 'h' ] ),
      ( C 'f', [ C 'g' ] ),
      ( C 'g', [ C 'i', C 'j' ] ),
      ( C 'h', [ C 'k' ] ),
      ( C 'i', [ C 'k' ] ),
      ( C 'j', [ C 'k' ] ),
      ( C 'k', [] )
    ]


graph3 :: Graph Node 
graph3 = Graph 
  [
    (I 1, [I 2]),
    (I 2, [I 3]),
    (I 3, [I 4]),    
    (I 4, [I 5]),
    (I 5, [I 6]),
    (I 6, [I 7]),
    (I 7, [I 8]),    
    (I 8, [I 9]),
    (I 9, [I 10]),
    (I 10, [I 11]),
    (I 11, [I 12]),    
    (I 12, [I 13]),
    (I 13, [I 14]),
    (I 14, [I 15]),
    (I 15, [I 16]),    
    (I 16, [I 17]),
    (I 17, [I 18]),
    (I 18, [I 19]),
    (I 19, [I 20]),    
    (I 20, [I 21]),
    (I 21, [I 22]),
    (I 22, [I 23]),
    (I 23, [I 24]),    
    (I 24, [I 25]),
    (I 25, [I 26]),
    (I 26, [I 27]),
    (I 27, [I 28]),    
    (I 28, [I 29]),
    (I 29, [I 30]),
    (I 30, [I 31]),
    (I 31, [I 32]),    
    (I 32, [I 33]),
    (I 33, [I 34]),
    (I 34, [I 35]),    
    (I 35, [I 36]),
    (I 36, [I 37]),     
    (I 37, [I 38]),
    (I 38, [I 39]),    
    (I 39, [I 40]),
    (I 40, [I 41]),        
    (I 41, [I 42]),        
    (I 42, [I 43]),        
    (I 43, [I 44]),        
    (I 44, [I 45]),        
    (I 45, [I 46]),        
    (I 46, [I 47]),        
    (I 47, [I 48]),        
    (I 48, [I 49]),        
    (I 49, [I 50]),        
    (I 50, [I 51]),
    (I 51, [I 52]),
    (I 52, [I 53]),
    (I 53, [I 54]),
    (I 54, [I 55]),
    (I 55, []) 
  ]



instance Show Labels where
  show (Labels a b c d e f g h i) = "TP: " ++  show a ++   " Pre: " ++ show b ++
   " Post:  " ++ show c   ++   " Hops: " ++ show d ++ " Directs:  " ++ show e ++
   "FC : " ++ show f ++ " LC :  " ++ show g ++ " NS: " ++ show h ++ " PS : " ++ show i
 
graph1Table :: Table (Labels)
graph1Table = table "graph1"
            `withIndex` graph_index

graph_index :: Index Labels Labels 
graph_index = index graph1Table "node_index" Prelude.id

nodeMapTable :: Table X
nodeMapTable = table "nodemap" `withIndex` nodemap_index


nodemap_index :: Index X Node
nodemap_index = index nodeMapTable "nodemap_index" nd

counters :: Table (String, Nd)
counters = table "counter" 
             `withIndex` counter_index

counter_index :: Index (String, Nd) String
counter_index = index counters "counter_index" fst


generateGraph :: Int64 -> Double ->Graph Node
generateGraph n p =  Graph $ map (\x -> (I x,restList x )) {- list@( -}[1..n]
    where 
        restList x= map I $ List.sort $ List.nub (take  (floor (p * fromIntegral (n-x))) $ randomRs (x+1,n) (mkStdGen 3) :: [Int64]  )


getNdIndex node = do
  nod <- select [ind | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
  case nod of
    [nd] -> return nd
    []   -> do 
{-       c_counter <- getCounter
      incrementCounter >> incrementCounter  -}
      pkey <- insert_ nodeMapTable (X node [])
--      store  graph1Table (Just pkey) (Labels (-1) (-3) (-2) Set.empty Set.empty (-100) (-100) (-100) (-100)  )
      return pkey
    _    -> error $ "ivalid getindex nd :" ++ show nod


addHop :: Nd -> Labels -> Nd -> Daison ()
addHop nd1 (Labels tp pr ps hp dir fc lc ns ls) nd2 = do
  store graph1Table (Just nd1) (Labels tp pr ps (Set.insert nd2 hp) dir fc lc ns ls)
  return ()

updateDirectInAncestors :: Nd -> Labels -> (Directs -> Directs) -> Daison ()
updateDirectInAncestors nd (Labels tp pr ps hp dir fc lc ns ls) f = do
  store graph1Table (Just nd) (Labels tp pr ps hp (f dir) fc lc ns ls)
  when (tp /= 0) $ do
    record <- query firstRow (from graph1Table (at tp))
    updateDirectInAncestors tp record f

