
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
import Control.Monad.IO.Class

type Nd = Key Labels
--  deriving (Eq, Ord, Read, Data)
type Ndc = Char
  --deriving (Eq, Ord, Read, Data)
type Hops = Set Nd
type Directs = Set Nd
type Pre = Int
type Post = Int
type Graph =  [(Ndc, [Ndc])]
type GraphMap = Map Ndc [Ndc]
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
} deriving (Typeable, Data )

data X = X { ndc :: Ndc, 
     keyLabels :: Key Labels
} deriving (Typeable, Data, Show)


graph11 :: Graph
graph11 = 
    [ ('a', [ 'b',  'c'] ),
      ('b', [ 'c'] )
    ]

graph2 :: Graph
graph2 = 
    [ ( 'a', [  'b',  'c'] ),
      ( 'b', [  'd',  'e',  'f' ] ),
      ( 'c', [  'h' ] ),
      ( 'd', [  'k' ] ),
      ( 'e', [  'g',  'h' ] ),
      ( 'f', [  'g' ] ),
      ( 'g', [  'i',  'j' ] ),
      ( 'h', [  'k' ] ),
      ( 'i', [  'k' ] ),
      ( 'j', [  'k' ] ),
      ( 'k', [] )
    ]

instance Show Labels where
  show (Labels a b c d e ) = "TP: " ++  show a ++ " Pre: " ++ show b ++ " Post:  " ++ show c  ++ " Hops: " ++ show d ++ " Directs:  " ++ show e

graph1Table :: Table (Labels)
graph1Table = table "graph1"
            `withIndex` graph_index

graph_index :: Index Labels Labels 
graph_index = index graph1Table "node_index" Prelude.id

nodeMapTable :: Table X
nodeMapTable = table "nodemap" `withIndex` nodemap_index

nodemap_index :: Index X Ndc
nodemap_index = index nodeMapTable "nodemap_index" ndc 

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
    tryCreateTable nodeMapTable
{-     c_counter <- getCounter
    if (c_counter >0) then return ()
    else
      do  -}     
    insert counters (return ( "counter", 0 ))
    let graphmap1 = Map.fromList graph2
    process graphmap1
    a <- select [ x | x <- from graph1Table everything ]
    b <- select [ x | x <- from nodeMapTable everything ]
    return (a,b)
  mapM_ (\y -> putStrLn (show y) ) a
  mapM_ (\y -> putStrLn (show y) ) b
  closeDB db
  --makeDynamicOperation databaseTest ReadWriteMode

{- makeDynamicOperation :: String -> AccessMode -> IO()
makeDynamicOperation test_db readwritemode = do
    putStrLn ("Enter your choice for (I) for Edge Insertion or (D) for Edge Deletion : ")
    choice <- getChar
    putStrLn (" Enter the first node of the edge that you want to update : ")
    firstChar <- getChar
    putStrLn (" Enter the second node of the edge that you want to update : ")
    secondChar <- getChar
    --let firstChar = 'c'
    --let secondChar = 'h'
    db <- openDB test_db  
    (a,b) <- runDaison db readwritemode $ do 
      case choice of
        'I' -> handleInsert (Nd firstChar) (Nd secondChar)
        'D' -> handleDelete (Nd firstChar) (Nd secondChar)
      x <- select [ x | x <- from graph1Table everything ] 
      y <- select [ x | x <- from nodeMapTable everything ]
      return (x,y)
    mapM_ (\y -> putStrLn (show y) ) a
    mapM_ (\y -> putStrLn (show y) ) b
    closeDB db
    makeDynamicOperation test_db readwritemode
 -}



isTreeEdge :: Nd -> Nd -> Daison Bool
isTreeEdge nd1 nd2 = undefined {- do
  record <- select [edgess | (ind, (nd, Edges edgess)) <- from edge1Table everything , nd == nd1  ] 
  case record of
    [] -> return False
    [[(nd, True)]] -> return (nd2 == nd)
    [lis] -> return $ elem (nd2, True) lis
 -}


isTheOnlyNonTreeEdge :: Nd -> Nd -> Daison Bool
isTheOnlyNonTreeEdge nd1 nd2 = undefined {- do
  record <- select [edgess | (ind, (nd, Edges edgess)) <- from edge1Table everything , nd == nd1  ] 
  case record of
    [] -> error "database error"
--    [[(nd, True)]] -> return (nd2 == nd)
    [lis] -> return $ length (filter (\(x,y) -> y == False) lis ) == 0
 -}


--getNdIndex :: Ndc -> Daison Nd 
getNdIndex node = do
  nod <- select [nodeindex | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
  case nod of
    [nd] -> return nd
    _    -> error $ "ivalid getindex nd :" ++ show nod


process :: GraphMap -> Daison ()
process graphmap = do
  let firstnode = fst $ Map.elemAt 0 graphmap
  processNodes graphmap firstnode firstnode

processNodes :: GraphMap -> Ndc -> Ndc -> Daison()
processNodes graph nd parent = do
  x <- insertNodeinDB nd parent
  unless x $ do
    let adjacent = Map.lookup nd graph
    case adjacent of
      Nothing -> return ()
      Just []      -> return ()
      Just rest    -> mapM_ (\x -> processNodes graph x nd ) rest
    getNdIndex nd >>= \nd1 -> updatePost nd1

insertNodeinDB :: Ndc -> Ndc -> Daison Bool
insertNodeinDB node parent = do
  map <- select [nodeindex | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
  par <- if (node == parent) then return [0] else select [nodeindex | (ind, ( X nd  nodeindex )) <- from nodeMapTable everything , nd == parent  ]
  case map of
    [] -> do
      c_counter <- getCounter
      incrementCounter
      pkey <- insert_ graph1Table ((Labels (head par) c_counter c_counter Set.empty Set.empty ))    
      insert_ nodeMapTable (X node pkey)
      return False
    _   ->  do
      parent_record <- select [(head par, labels2) | (labels2) <- from graph1Table (at (head par))  ] 
      case parent_record of 
        [] -> error "error "
        [(indp, labelp)] -> case labelp of 
          (Labels ptrp ppr pps php pdir) -> do
            update_ graph1Table (return (indp,(Labels ptrp ppr pps (Set.insert (head map) php) pdir) ))
            --when (ptrp /= 0)
            updateDirects (head par) ptrp 
      return True 
{-     first : rest -> error "duplicate records in the database table, please verify"
 -}

updatePost :: Nd -> Daison ()
updatePost nd = do 
  record <- select [(nd,label1) | (label1) <- from graph1Table (at nd)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir ->  do 
        c_counter <- getCounter
        incrementCounter
        update_ graph1Table (return (nd, Labels trp pr c_counter hp dir ))
      _   -> error "error from updatepost"
    _ -> error "error " 

updateDirects :: Nd -> Nd -> Daison()
updateDirects parent gp = do
  record <- select [(gp,label1) | (label1) <- from graph1Table (at gp)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir ->  do
        update_ graph1Table (return (nd, Labels trp pr ps hp (Set.insert parent dir) ))
      _ -> error "updatedirects error"
    _   -> liftIO $ print record
  if gp == 0 then return () 
  else 
    do
      ggp <- getParent gp
      if(ggp == gp) then return ()
      else updateDirects parent ggp

getParent :: Nd -> Daison Nd
getParent node = do
  record <- select [(node, labels) | (labels) <- from graph1Table (at node) ] 
  case record of
    [] -> error $ "invalid parent node :" ++ show node
    [(ind1,  label1)] -> case label1 of 
      (Labels trp _ _ _ _) -> return trp
    _           -> error "multiple parents error "
 
getCounter :: Daison Int
getCounter = select [ x | x <- from counters (at 1) ] >>= \p -> return . snd . head $ p  

incrementCounter :: Daison ()
incrementCounter = getCounter >>= \c_counter -> update_ counters (return (1, ("counter", c_counter+1) ))

resetCounter :: Daison ()
resetCounter = update_ counters (return (1, ("counter", 0) ))


