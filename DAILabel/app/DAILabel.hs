
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveDataTypeable #-}

import           Database.Daison
import           System.Environment
import           Data.Data
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad


newtype Nd = Nd Char
  deriving (Eq, Ord, Read, Data)

type Hops = Set Nd
type Directs = Set Nd
type Pre = Int
type Post = Int
type Graph =  [(Nd, [Nd])]
type GraphMap = Map Nd [Nd]


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


graph1Table :: Table (Nd,Labels)
graph1Table = table "graph1"
           `withIndex` graph_index

graph_index :: Index (Nd, Labels) Nd
graph_index = index graph1Table "node_index" fst


counters :: Table (String, Int)
counters = table "counter" 
             `withIndex` counter_index

counter_index :: Index (String, Int) String
counter_index = index counters "counter_index" fst


main :: IO ()
main = do
  db <- openDB "test1.db"
  x  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable counters
    insert counters (return ( "counter", 0 ))
    let graphmap1 = Map.fromList graph2
    process graphmap1
    select [ x | x <- from graph1Table everything ]
  print x
  closeDB db

process :: GraphMap -> Daison ()
process graphmap = do
  let firstnode = fst $ Map.elemAt 0 graphmap
  processNodes graphmap firstnode firstnode

processNodes :: GraphMap -> Nd -> Nd -> Daison()
processNodes graph nd parent = do
  x <- insertNodeinDB nd parent
  unless x $ do
    let adjacent = Map.lookup nd graph
    case adjacent of
      Nothing -> return ()
      Just []      -> return ()
      Just rest    -> mapM_ (\x -> processNodes graph x nd ) rest
    updatePost nd

insertNodeinDB :: Nd -> Nd -> Daison Bool
insertNodeinDB node parent = do
  record <- select [(ind, nd, labels) | (ind, (nd, labels)) <- from graph1Table everything , nd == node  ] 
  case record of
    [] -> do
      c_counter <- getCounter
      incrementCounter 
      insert graph1Table (return ( node, Labels parent c_counter c_counter Set.empty Set.empty ))    
      return False
    _   -> do
      parent_record <- select [(ind2, nd2, labels2) | (ind2, (nd2, labels2)) <- from graph1Table everything , nd2 == parent  ] 
      case parent_record of 
        [] -> error "error "
        [(indp, ndp, labelp)] -> case labelp of 
          (Labels ptrp ppr pps php pdir) -> do
            update_ graph1Table (return (indp,(ndp, Labels ptrp ppr pps (Set.insert node php) pdir) ))
            updateDirects ndp ptrp
      return True
    first : rest -> error "duplicate records in the database table, please verify"
    
updatePost :: Nd -> Daison ()
updatePost node = do 
  record <- select [(ind, nd, label) | (ind, (nd, label)) <- from graph1Table everything , nd == node  ] 
  case record of 
    [(ind, nd, label)] -> case label of
      Labels trp pr ps hp dir ->  do 
        c_counter <- getCounter
        incrementCounter
        update_ graph1Table (return (ind,(nd, Labels trp pr c_counter hp dir) ))
      _   -> error "error from updatepost"
    _ -> error "error " 

updateDirects :: Nd -> Nd -> Daison()
updateDirects parent gp = do
  record <- select [(ind1, nd1, label1) | (ind1, (nd1, label1)) <- from graph1Table everything , nd1 == gp  ] 
  case record of 
    [(ind, nd, label)] -> case label of
      Labels trp pr ps hp dir ->  do
        update_ graph1Table (return (ind,(nd, Labels trp pr ps hp (Set.insert parent dir)) ))
      _ -> error "updatedirects error"
  ggp <- getParent gp
  if(ggp == gp) then return ()
  else updateDirects parent ggp

getParent :: Nd -> Daison Nd
getParent node = do
  record <- select [(ind, nd, labels) | (ind, (nd, labels)) <- from graph1Table everything , nd == node  ] 
  case record of
    [] -> error "invalid parent node"
    [(ind1, nd1, label1)] -> case label1 of 
      (Labels trp _ _ _ _) -> return trp
    _           -> error "multiple parents error "

getCounter :: Daison Int
getCounter = select [ x | x <- from counters (at 1) ] >>= \p -> return . snd . head $ p  

incrementCounter :: Daison ()
incrementCounter = getCounter >>= \c_counter -> update_ counters (return (1, ("counter", c_counter+1) ))

resetCounter :: Daison ()
resetCounter = update_ counters (return (1, ("counter", 0) ))

