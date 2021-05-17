
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


instance Show Nd where
  show (Nd a) = show a 

instance Show Labels where
  show (Labels a b c d e ) = "TP: " ++  show a ++ " Pre: " ++ show b ++ " Post:  " ++ show c  ++ " Hops: " ++ show d ++ " Directs:  " ++ show e


graph1 :: Table (Nd,Labels)
graph1 = table "graph1"
           `withIndex` node_name

node_name :: Index (Nd, Labels) Nd
node_name = index graph1 "node_index" fst


main :: IO ()
main = do
  db <- openDB "test1.db"
  x  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1
    insert graph1 (return ( Nd 'a', Labels (Nd 'a') 1 10 Set.empty Set.empty ))
    insert graph1 (return ( Nd 'b', Labels (Nd 'a') 2 9 Set.empty Set.empty ))
    --update people2 (\_ (name,age,x) -> (name,age,10))
      --             (from people2)
    select [ x | x <- from graph1 everything ]
  print x
  closeDB db

