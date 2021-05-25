
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveDataTypeable #-}

import           Database.Daison
import           System.Environment
import           Data.Data
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad


newtype Nd = Nd Char
  deriving (Eq, Ord, Read, Data)

type Hops = Set Nd
type Directs = Set Nd
type Pre = Int
type Post = Int
type Graph =  [(Nd, [Nd])]
type GraphMap = Map Nd [Nd]
type Special = Bool


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

data Edges = Edges {
    edges :: [(Nd,Bool)]
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

instance Show Edges where
  show (Edges a) = show a 



graph1Table :: Table (Nd,Labels)
graph1Table = table "graph1"
           `withIndex` graph_index

graph_index :: Index (Nd, Labels) Nd
graph_index = index graph1Table "node_index" fst


edge1Table :: Table (Nd,  Edges)
edge1Table = table "edge1" `withIndex` edge_index

edge_index :: Index (Nd,  Edges) Nd
edge_index = index edge1Table "edge_index" fst--(\(a,b,c) -> a)



counters :: Table (String, Int)
counters = table "counter" 
             `withIndex` counter_index

counter_index :: Index (String, Int) String
counter_index = index counters "counter_index" fst

databaseTest = "test1.db"

main :: IO ()
main = do
  db <- openDB databaseTest
  (a,b)  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable counters
    tryCreateTable edge1Table
{-     c_counter <- getCounter
    if (c_counter >0) then return ()
    else
      do  -}     
    insert counters (return ( "counter", 0 ))
    let graphmap1 = Map.fromList graph2
    process graphmap1
    a <- select [ x | x <- from graph1Table everything ]
    b <- select [ x | x <- from edge1Table everything ]
    return (a,b)
{-     p <- isTheOnlyNonTreeEdge (Nd 'c') (Nd 'h')
    return [p]
 -}{-     cou <- getCounter
    return [cou] -}
  mapM_ (\y -> putStrLn (show y) ) a
  mapM_ (\y -> putStrLn (show y) ) b
  closeDB db
  makeDynamicOperation databaseTest ReadWriteMode

makeDynamicOperation :: String -> AccessMode -> IO()
makeDynamicOperation test_db readwritemode = do
    putStrLn ("Enter your choice for (I) for Edge Insertion or (D) for Edge Deletion : ")
    choice <- getChar
    putStrLn (" Enter the first node of the edge that you want to update : ")
    --firstChar <- getChar
    putStrLn (" Enter the second node of the edge that you want to update : ")
    --secondChar <- getChar
    let firstChar = 'c'
    let secondChar = 'h'
    db <- openDB test_db  
    (a,b) <- runDaison db readwritemode $ do 
      case choice of
        'I' -> handleInsert (Nd firstChar) (Nd secondChar)
        'D' -> handleDelete (Nd firstChar) (Nd secondChar)
      x <- select [ x | x <- from graph1Table everything ] 
      y <- select [ x | x <- from edge1Table everything ]
      return (x,y)
    mapM_ (\y -> putStrLn (show y) ) a
    mapM_ (\y -> putStrLn (show y) ) b
    closeDB db
    makeDynamicOperation test_db readwritemode



handleInsert :: Nd -> Nd -> Daison ()
handleInsert nd1 nd2 = undefined

handleDelete :: Nd -> Nd -> Daison ()
handleDelete nd1 nd2 = do
  istreeEdge <- isTreeEdge nd1 nd2
  deleteEdge nd1 nd2 istreeEdge
  case istreeEdge of
    True -> do
      deleteDirectsandAncestors nd1 nd2
      removeTreeParent nd2
      relabel nd2 
    False -> do
      flag <- isTheOnlyNonTreeEdge nd1 nd2
      if flag then
        do
          record <- select [(ind, nd, labels) | (ind, (nd, labels)) <- from graph1Table everything , nd == nd1  ] 
          case record of
            [(ind, nd , Labels tp pr ps hp dir)] -> do
              deleteDirectsandAncestors tp nd1
              if Set.null dir then return ()
              else 
                addDirectsandAncestors tp dir
            _ -> error "invalid "
      else return ()
      deleteHopsFrom nd1 nd2
      return ()

removeTreeParent :: Nd -> Daison ()
removeTreeParent nd1 = do 
  record <- select [(ind, nd, labels) | (ind, (nd, labels)) <- from graph1Table everything , nd == nd1  ] 
  case record of
    [(ind, nd , Labels tp pr ps hp dir)] -> do
      update_ graph1Table (return (ind,(nd, Labels nd pr ps hp dir) ))

relabel :: Nd -> Daison ()
relabel nd1 = undefined


deleteDirectsandAncestors :: Nd -> Nd -> Daison()
deleteDirectsandAncestors nd1 nd2 = do
  record <- select [(ind, nd, labels) | (ind, (nd, labels)) <- from graph1Table everything , nd == nd1  ] 
  case record of
    [(ind, nd , Labels tp pr ps hp dir)] -> do
      update_ graph1Table (return (ind,(nd, Labels tp pr ps hp (Set.delete nd2 dir)) ))
      if tp == nd1 then return ()
      else deleteDirectsandAncestors tp nd2
    _ -> error "invalid"

deleteHopsFrom :: Nd -> Nd -> Daison ()
deleteHopsFrom nd1 nd2 = do
  record <- select [(ind, nd, labels) | (ind, (nd, labels)) <- from graph1Table everything , nd == nd1  ] 
  case record of
    [(ind, nd , Labels tp pr ps hp dir)] -> 
      update_ graph1Table (return (ind,(nd, Labels tp pr ps (Set.delete nd2 hp) dir) ))
    _ -> error "invalid"  

addDirectsandAncestors :: Nd -> Directs -> Daison ()
addDirectsandAncestors nd1 setdir = do
  record <- select [(ind, nd, labels) | (ind, (nd, labels)) <- from graph1Table everything , nd == nd1  ] 
  case record of
    [(ind, nd , Labels tp pr ps hp dir)] -> do
      update_ graph1Table (return (ind,(nd, Labels tp pr ps hp (Set.union setdir dir)) ))
      if tp == nd1 then return ()
      else addDirectsandAncestors tp setdir
    _ -> error "invalid"

deleteEdge :: Nd -> Nd -> Bool -> Daison()
deleteEdge nd1 nd2 boolval = do
  record <- select [(ind, nd, edgess) | (ind, (nd, Edges edgess)) <- from edge1Table everything , nd == nd1  ] 
  case record of
    [] -> error $ "database error " ++ show nd1 ++ " :  " ++ show nd2
    [(ind, nd, edges )] -> update_ edge1Table (return (ind, (nd, Edges (List.delete (nd2, boolval) edges )   )))  
  return ()


isIsolated :: Nd -> Daison Bool
isIsolated node = do
  record <- select [(ind, nd, labels) | (ind, (nd, labels)) <- from graph1Table everything , nd == node  ] 
  case record of
    [] -> return True
    _  -> return False

isTreeEdge :: Nd -> Nd -> Daison Bool
isTreeEdge nd1 nd2 = do
  record <- select [edgess | (ind, (nd, Edges edgess)) <- from edge1Table everything , nd == nd1  ] 
  case record of
    [] -> return False
    [[(nd, True)]] -> return (nd2 == nd)
    [lis] -> return $ elem (nd2, True) lis



isTheOnlyNonTreeEdge :: Nd -> Nd -> Daison Bool
isTheOnlyNonTreeEdge nd1 nd2 = do
  record <- select [edgess | (ind, (nd, Edges edgess)) <- from edge1Table everything , nd == nd1  ] 
  case record of
    [] -> error "database error"
--    [[(nd, True)]] -> return (nd2 == nd)
    [lis] -> return $ length (filter (\(x,y) -> y == False) lis ) == 0




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
      updateEdge1Table parent node True
      return False
    _   -> do
      parent_record <- select [(ind2, nd2, labels2) | (ind2, (nd2, labels2)) <- from graph1Table everything , nd2 == parent  ] 
      case parent_record of 
        [] -> error "error "
        [(indp, ndp, labelp)] -> case labelp of 
          (Labels ptrp ppr pps php pdir) -> do
            update_ graph1Table (return (indp,(ndp, Labels ptrp ppr pps (Set.insert node php) pdir) ))
            updateEdge1Table parent node False
            updateDirects ndp ptrp
      return True
{-     first : rest -> error "duplicate records in the database table, please verify"
 -}
updateEdge1Table :: Nd -> Nd -> Bool -> Daison ()
updateEdge1Table nd1 nd2 isTreeEdge = do
  record <- select [(ind, nd, edgess) | (ind, (nd, Edges edgess)) <- from edge1Table everything , nd == nd1  ] 
  case record of
    [] -> insert edge1Table (return ( nd1, Edges [(nd2, isTreeEdge)] ))   >> return ()
    [(ind, nd, edges )] -> update_ edge1Table (return (ind, (nd, Edges ((nd2, isTreeEdge): edges)   )))  
  return ()


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


