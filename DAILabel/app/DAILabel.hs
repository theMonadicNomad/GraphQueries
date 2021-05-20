
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveDataTypeable #-}

import           Database.Daison
import           System.Environment
import           Data.Data
import Data.Set (Set)
import qualified Data.Set as Set



newtype Nd = Nd Char
  deriving (Eq, Ord, Read, Data)

type Hops = Set Nd
type Directs = Set Nd
type Pre = Int
type Post = Int
type Graph =  [(Nd, [Nd])]


type Distance = Int

data City = City { name :: String,
 id :: Int,
 population :: Int,
 paths :: [(Key City, Distance)] 
} deriving (Typeable, Data )
  

data Labels = Labels {
    tree_parent :: Nd,
    pre :: Pre,
    post :: Post,
    hops :: Hops,
    directs :: Directs
} deriving (Typeable, Data)

graph11 :: Graph
graph11 = 
    [ (Nd 'a', [ Nd 'b', Nd 'c'] ),
      (Nd 'b', [ Nd 'c'] )
    ]

graph2 :: Graph
graph2 = 
    [ (Nd 'a', [ Nd 'b', Nd 'c'] ),
      (Nd 'b', [ Nd 'd', Nd 'e', Nd 'f' ] ),
      (Nd 'c', [ Nd 'h' ] ),
      (Nd 'd', [ Nd 'k' ] ),
      (Nd 'e', [ Nd 'g', Nd 'h' ] ),
      (Nd 'f', [ Nd 'g' ] ),
      (Nd 'g', [ Nd 'i', Nd 'j' ] ),
      (Nd 'h', [ Nd 'k' ] ),
      (Nd 'i', [ Nd 'k' ] ),
      (Nd 'j', [ Nd 'k' ] ),
      (Nd 'k', [] )
    ]



instance Show Nd where
  show (Nd a) = show a 

instance Show Labels where
  show (Labels a b c d e ) = "TP: " ++  show a ++ " Pre: " ++ show b ++ " Post:  " ++ show c  ++ " Hops: " ++ show d ++ " Directs:  " ++ show e


graph1 :: Table (Nd,Labels)
graph1 = table "graph1"
           `withIndex` graph_index

graph_index :: Index (Nd, Labels) Nd
graph_index = index graph1 "node_index" fst


counters :: Table (String, Int)
counters = table "counter" 
             `withIndex` counter_index

counter_index :: Index (String, Int) String
counter_index = index counters "counter_index" fst


main :: IO ()
main = do
  db <- openDB "test1.db"
  x  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1
    tryCreateTable counters
    insert counters (return ( "counter", 143 ))
{-     insert graph1 (return ( Nd 'a', Labels (Nd 'a') 1 10 Set.empty Set.empty ))
    insert graph1 (return ( Nd 'b', Labels (Nd 'a') 2 9 Set.empty Set.empty ))
 -}    --update people2 (\_ (name,age,x) -> (name,age,10))
      --             (from people2)
    insertT
    select [ x | x <- from graph1 everything ]
--    head $ select [ x | x <- from counters (at 1) ]
  print x
  closeDB db


{- getCounter :: Daison Int
getCounter =  do
  p <- select [ x | x <- from counters (at 1) ]
  let m = head p
  let n = snd m 
  return n
 -}


getCounter :: Daison Int
getCounter =  do
  p <- select [ x | x <- from counters (at 1) ]
  return (snd . head . p )






incrementCounter :: Daison Int
incrementCounter = undefined

resetCounter :: Daison Int
resetCounter = undefined




process :: Graph -> Daison ()
process graph = processNodes graph (Nd 'a') (Nd 'a')


processNodes :: Graph -> Nd -> Nd -> Daison()
processNodes graph nd parent = undefined


--updatePost :: Graph -> Nd -> Daison ()



insertT :: Daison (Key (Nd, Labels), Key (Nd, Labels))
insertT = do
  insert graph1 (return ( Nd 'a', Labels (Nd 'a') 1 10 Set.empty Set.empty ))
  insert graph1 (return ( Nd 'b', Labels (Nd 'a') 2 9 Set.empty Set.empty ))


