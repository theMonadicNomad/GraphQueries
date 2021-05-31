
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
import qualified System.IO as IO


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
type TreeEdges =  [Nd]
type Record = (Nd, Labels)


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
    directs :: Directs,
    treeedges ::TreeEdges
} deriving (Typeable, Data )

data X = X { ndc :: Ndc, 
     keyLabels :: Key Labels--,
--     edges 
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
  show (Labels a b c d e f) = "TP: " ++  show a ++ " Pre: " ++ show b ++ " Post:  " ++ show c  ++ " Hops: " ++ show d ++ " Directs:  " ++ show e ++ " Tree Edges : " ++ show f 

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
  makeDynamicOperation databaseTest ReadWriteMode

makeDynamicOperation :: String -> AccessMode -> IO()
makeDynamicOperation test_db readwritemode = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    putStrLn ("Enter your choice for (I) for Edge Insertion or (D) for Edge Deletion : ")
    choice <- getChar
    putStrLn (" Enter the first node of the edge that you want to update : ")
    firstChar <- getChar
    putStrLn (" Enter the second node of the edge that you want to update : ")
    secondChar <- getChar
--    let firstChar = 'k'
--    let secondChar = 'l'
    db <- openDB test_db  
    (a,b) <- runDaison db readwritemode $ do 
      nd1 <- getNdIndex firstChar
      nd2 <- getNdIndex secondChar
      case choice of
        'I' -> handleInsert nd1 nd2
        'D' -> handleDelete nd1 nd2 
      x <- select [ x | x <- from graph1Table everything ] 
      y <- select [ x | x <- from nodeMapTable everything ]
      return (x,y)
    mapM_ (\y -> putStrLn (show y) ) a
    mapM_ (\y -> putStrLn (show y) ) b
    closeDB db
    makeDynamicOperation test_db readwritemode


handleInsert :: Nd -> Nd -> Daison ()
handleInsert nd1 nd2 = do
  record1 <- select [labels | (labels) <- from graph1Table (at nd1)  ] 
  record2 <- select [labels | (labels) <- from graph1Table (at nd2)  ]
  let isolated1  = isIsolated record1
  let isolated2 = isIsolated record2
  let special1 = isSpecialNode record1
  let special2 = isSpecialNode record2
  let hasparent2 = hasParent record2
  if (not isolated1) 
    then do
      if (not isolated2)
        then do 
          if (not hasparent2)
            then do 
              if (not special2)
                then getDirects record2 >>= \dir2 -> addDirectsandAncestors nd1 dir2 -- and the informatio of (u,v)
                                                                                -- stored in the intervals 
              else
                addDirectinAncestors nd1 nd2
              setTreeParent nd2 nd1
              resetCounter 
              liftIO $ print $ ( " from Case1 Insert - v hasparent2 : ") ++ show hasparent2 ++ " special2 : " ++ show special2
              relabel nd1 [] >> return ()
          else
            do
              if special1
                then addHop nd1 nd2 -- And all the nodes in v's directs can be generated by v
              else
                getParent nd1 >>= \parent1 -> when (parent1 /=0) (addDirectinAncestors parent1 nd1 >> addHop nd1 nd2)
              liftIO $ print $ ( " from Case1 Insert, hasparent2 : ") ++ show hasparent2 ++ " special1 : " ++ show special1
      else
        do
          liftIO $ print ( " case2 insert from ")
          addEdge nd1 nd2 >> resetCounter >>  relabel 1 [] >>  return ()   --relabel from which point ?
        
  else
    do
      if (not isolated2)
        then do
          if hasparent2
            then do
              c_counter <- getCounter
              incrementCounter
              incrementCounter
--              record <- select [(labels) | (labels) <- from graph1Table (at nd1)  ]
              case record1 of --how the new node is inserted
                [(Labels tp pr ps hp dir te)] -> update_ graph1Table (return (nd1, Labels tp c_counter (c_counter+1) (Set.insert nd2 hp) dir te) )   
              liftIO $ print ( " from Case3 -- HasParent2 Insert")
          else
            do               
              if (not special2)
                then getDirects record2 >>= \dir2 -> addDirectsandAncestors nd1 dir2 -- and the informatio of (u,v)
                                                                                -- stored in the intervals 
              else
                addDirectinAncestors nd1 nd2
              setTreeParent nd2 nd1
              resetCounter 
              liftIO $ print (( " from Case3 Insert - v has no parent, special2 :  ") ++ show special2)
              relabel nd1 [] >> return ()   --is it valid  case1 
      else 
        do 
--          addEdge nd1 nd2 
--          record1 <- select [(labels) | (labels) <- from graph1Table (at nd1)  ]
--          record2 <- select [(labels) | (labels) <- from graph1Table (at nd2)  ]
          c_counter <- getCounter
          incrementCounter >> incrementCounter >> incrementCounter >> incrementCounter
          case record1 of
            [(Labels tp pr ps hp dir te)] -> update_ graph1Table (return (nd1, Labels 0 c_counter (c_counter+3) hp dir (nd2:te)) ) 
          case record2 of
            [(Labels tp pr ps hp dir te)] -> update_ graph1Table (return (nd2, Labels nd1 (c_counter+1) (c_counter+2) hp dir te) ) 
          liftIO $ print ( " from Case4 Insert")
 
  return ()
  where
    hasParent record = case record of
        [(Labels tp pr ps hp dir te)] ->  tp > 0
--        _  -> return False
    isIsolated record = case record of
        [(Labels tp _ _ _ _ _ )] -> tp == -1
        _  ->  False
    getDirects record = case record of
        [(Labels tp pr ps hp dir te)] -> return dir
    setTreeParent nd1 nd2 = do
      record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
      case record of
        [(nd, Labels tp pr ps hp dir te)] ->  update_ graph1Table (return (nd, Labels nd2 pr ps hp dir te) )   
    isSpecialNode record = case record of 
      [(Labels tp pr ps hp dir _)] ->  (Set.size hp) > 0
      _       -> error "error from special node record : "



addEdge :: Nd -> Nd -> Daison()
addEdge nd1 nd2 = do 
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir te)] -> do
      update_ graph1Table (return (nd, Labels tp pr ps hp dir (nd2:te)) )

handleDelete :: Nd -> Nd -> Daison ()
handleDelete nd1 nd2 = do
  istreeEdge <- isTreeEdge nd1 nd2
  case istreeEdge of
    True -> do
      deleteDirectsandAncestors nd1 nd2
      deleteTreeEdge nd1 nd2
      removeTreeParent nd2
--      relabel nd2 
    False -> do
      flag <- isTheOnlyNonTreeEdge nd1 nd2
      if flag then
        do
          record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
          case record of
            [(nd , Labels tp pr ps hp dir te)] -> do
              deleteDirectsandAncestors tp nd1
              when ( not $ Set.null dir ) $ addDirectsandAncestors tp dir
            _ -> error $ "invalid from handle delete : " ++ show nd1 ++ show nd2
      else return ()
      deleteHopsFrom nd1 nd2
      return ()

addHop :: Nd -> Nd -> Daison ()
addHop nd1 nd2 = do 
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir te)] -> do
      update_ graph1Table (return (nd, Labels tp pr ps (Set.insert nd2 hp) dir te) )


deleteTreeEdge :: Nd -> Nd -> Daison ()
deleteTreeEdge nd1 nd2 = do 
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir te)] -> do
      update_ graph1Table (return (nd, Labels tp pr ps hp dir (List.delete nd2 te)) )


removeTreeParent :: Nd -> Daison ()
removeTreeParent nd1 = do 
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir te)] -> do
      update_ graph1Table (return (nd, Labels 0 pr ps hp dir te) )
    

relabel :: Nd -> [Nd] -> Daison [Nd]
relabel nd visited =  do 
--  liftIO $ print $ (" relabel, nd : ") ++ show nd ++ "  visited : " ++ show visited
  x <- updatePre nd visited
  if x then return visited
       else do
         edges <- getEdges nd 
--         liftIO $ print $ " nd :" ++ show nd ++ " edges : " ++ show edges
         nv <- case edges of
           [] -> return visited
           rest -> foldM (\acc y -> relabel y acc) visited  rest
         updatePost nd 
         return (nd : nv)


getEdges :: Nd -> Daison [Nd]
getEdges nd = do 
  record <- select [(nd, labels) | (labels) <- from graph1Table (at nd)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir te)] -> if (List.null te) then return $ Set.toList hp
                                          else 
                                            return $ te ++ (Set.toList hp)

deleteDirectsandAncestors :: Nd -> Nd -> Daison()
deleteDirectsandAncestors nd1 nd2 = do
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir te)] -> do
      update_ graph1Table (return (nd, Labels tp pr ps hp (Set.delete nd2 dir) te) )
      when (tp/=0) $ deleteDirectsandAncestors tp nd2
    _ -> error $ "invalid from deletedirectss and ancestors " ++ show nd1 ++ show nd2


deleteHopsFrom :: Nd -> Nd -> Daison ()
deleteHopsFrom nd1 nd2 = do
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir te)] -> do
      update_ graph1Table (return (nd, Labels tp pr ps (Set.delete nd2 hp) dir te) )
    _ -> error "invalid"  


addDirectinAncestors :: Nd -> Nd -> Daison ()
addDirectinAncestors anc d = do
  record <- select [(anc, labels) | (labels) <- from graph1Table (at anc)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir te)] -> do
      update_ graph1Table (return (nd, Labels tp pr ps hp (Set.insert d dir) te) )
      when (tp/=0) $ addDirectinAncestors tp d
    _ -> error "invalid"




addDirectsandAncestors :: Nd -> Directs -> Daison ()
addDirectsandAncestors nd1 setdir = do
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir te)] -> do
      update_ graph1Table (return (nd, Labels tp pr ps hp (Set.union setdir dir) te) )
      when (tp/=0) $ addDirectsandAncestors tp setdir
    _ -> error "invalid"

isTreeEdge :: Nd -> Nd -> Daison Bool
isTreeEdge nd1 nd2 = do
  record <- select [label1 | (label1) <- from graph1Table (at nd1)  ] 
  case record of 
    [(Labels tp pr ps hp dir te)] -> return $ List.elem nd2 te 
    _ -> error $ " from istreeedge : " ++ show nd1 ++ show nd2
 

isTheOnlyNonTreeEdge :: Nd -> Nd -> Daison Bool
isTheOnlyNonTreeEdge nd1 nd2 = do
  record <- select [(nd1,label1) | (label1) <- from graph1Table (at nd1)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir _ ->  do 
        if (Set.member nd2 hp) && (Set.size hp ==1) then 
          return True
        else if (Set.member nd2 hp) && (Set.size hp > 1) then
          return False
        else 
          error $ "error from istheonlynontreeedge : nd1 , nd2 : " ++ show nd1 ++ show nd2
    _ -> error " couldnt find record from isonlynontreeedge "

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

getNdIndex node = do
  nod <- select [nodeindex | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
  case nod of
    [nd] -> return nd
    []   -> do 
      c_counter <- getCounter
      incrementCounter >> incrementCounter
      pkey <- insert_ graph1Table ((Labels (-1) c_counter (c_counter+1) Set.empty Set.empty []  ))    
      insert_ nodeMapTable (X node pkey)
      return pkey
    _    -> error $ "ivalid getindex nd :" ++ show nod

insertNodeinDB :: Ndc -> Ndc -> Daison Bool
insertNodeinDB node parent = do
  map <- select [nodeindex | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
  par <- if (node == parent) then return [0] else select [nodeindex | (ind, ( X nd  nodeindex )) <- from nodeMapTable everything , nd == parent  ]
  case map of
    [] -> do
      c_counter <- getCounter
      incrementCounter
      pkey <- insert_ graph1Table ((Labels (head par) c_counter c_counter Set.empty Set.empty []  ))    
      insert_ nodeMapTable (X node pkey)
      when (head par /= 0) $ do
        parent_record <- select [(head par, labels2) | (labels2) <- from graph1Table (at (head par))  ] 
        case parent_record of
          [(indp ,(Labels ptrp ppr pps php pdir pte))] -> update_ graph1Table (return (indp,(Labels ptrp ppr pps php pdir (pkey:pte)) ))
      return False
    _   ->  do
      parent_record <- select [(head par, labels2) | (labels2) <- from graph1Table (at (head par))  ] 
      case parent_record of 
          [] -> error "error "
          [(indp, labelp)] -> case labelp of 
            (Labels ptrp ppr pps php pdir pte) -> do
              update_ graph1Table (return (indp,(Labels ptrp ppr pps (Set.insert (head map) php ) pdir pte) ))
              when (ptrp /= 0) $ updateDirects (head par) ptrp 
      return True 
{-     first : rest -> error "duplicate records in the database table, please verify"
 -}


updatePre :: Nd -> [Nd] -> Daison Bool
updatePre nd visited = do 
  record <- select [(nd,label1) | (label1) <- from graph1Table (at nd)  ] 
  case record of 
    [(nd, Labels trp pr ps hp dir te )] -> if pr == ps || elem nd visited then return True
                                           else   
                                             do
                                               c_counter <- getCounter
                                               incrementCounter
                                               update_ graph1Table (return (nd, Labels trp c_counter c_counter hp dir te ))
                                               return False
    _ -> error "error " 

updatePost :: Nd -> Daison ()
updatePost nd = do 
  record <- select [(nd,label1) | (label1) <- from graph1Table (at nd)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir te ->  do 
        c_counter <- getCounter
        incrementCounter
        update_ graph1Table (return (nd, Labels trp pr c_counter hp dir te ))
      _   -> error "error from updatepost"
    _ -> error "error " 

updateDirects :: Nd -> Nd -> Daison()
updateDirects parent gp = do
  record <- select [(gp,label1) | (label1) <- from graph1Table (at gp)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir te ->  do
        update_ graph1Table (return (nd, Labels trp pr ps hp (Set.insert parent dir) te ))
      _ -> error "updatedirects error"
    _   -> liftIO $ print record
  when (gp /= 0) $ do
    ggp <- getParent gp
    when (ggp /= 0) $ updateDirects parent ggp

getParent :: Nd -> Daison Nd
getParent node = do
  record <- select [(node, labels) | (labels) <- from graph1Table (at node) ] 
  case record of
    [] -> error $ "invalid parent node :" ++ show node
    [(ind1,  label1)] -> case label1 of 
      (Labels trp _ _ _ _ _) -> return trp
    _           -> error "multiple parents error "
 
getCounter :: Daison Int
getCounter = select [ x | x <- from counters (at 1) ] >>= \p -> return . snd . head $ p  

incrementCounter :: Daison ()
incrementCounter = getCounter >>= \c_counter -> -- liftIO $ print show (c_counter+1) >>
   update_ counters (return (1, ("counter", c_counter+1) )) 
  


resetCounter :: Daison ()
resetCounter = update_ counters (return (1, ("counter", 0) ))


