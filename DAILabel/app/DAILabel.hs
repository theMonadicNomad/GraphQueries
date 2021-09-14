{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module DAILabel where

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
import System.Directory
import qualified Data.Bits as Bits
import Data.Int
import Data.Time

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
    directs :: Directs
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
    [ ( C 'a',  [ C 'b',  C 'c' ]  ),
      ( C 'b', [  C 'd', C 'e', C 'f' ] ),
      ( C 'c', [ C 'h' ] ),
      ( C 'd', [ C 'k' ] ),
      ( C 'e', [ C 'g',  C 'h' ] ),
      ( C 'f', [ C 'g' ] ),
      ( C 'g', [ C 'i', C 'j' ] ),
      ( C 'h', [ C 'k' ] ),
      ( C 'i', [ C 'k' ] ),
      ( C 'j', [ C 'k' ] ),
      ( C 'k', [] ),
      (C 'm', [ C 'a' ])
    ]
graph10 :: Graph Node 
graph10 = Graph 
  [
    (I 1, [I 4]),
    (I 2, []),
    (I 3, []),    
    (I 4, [I 5]),
    (I 5, [I 6]),
    (I 6, [I 7])
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
  show (Labels a b c d e) = "TP: " ++  show a ++   " Pre: " ++ show b ++
   " Post:  " ++ show c   ++   " Hops: " ++ show d ++ " Directs:  " ++ show e 

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

databaseTest = "dailabel.db"

gap = 1


instance Arbitrary (Graph Nd) where
  arbitrary = do 
    x <- arbitrary :: Gen Nd
    ((List.nub . filter (>= 0)) <$> listOf1 arbitrary) >>= helper

instance Arbitrary (Graph Ndc) where
  arbitrary = do 
    x <- arbitrary :: Gen Ndc
    ((List.nub . filter isAlphabet) <$> listOf1 arbitrary) >>= helper
    

helper :: (Eq a, Arbitrary a) => [a] -> Gen (Graph a)
helper ls = do
  let tls = List.tails ls
      l = length tls - 1
      t2ls = take l tls
      gs = map (fromJust . List.uncons) t2ls
  Graph <$> mapM (\(y,ks) -> do
                            js <- elements . List.subsequences $ ks
                            return (y, js)
                 ) gs


isAlphabet :: Ndc -> Bool
isAlphabet ch = if ch `elem` ['a'..'z'] || ch `elem` ['A'..'Z']
		then True 
		else False


main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  putStrLn ("Enter the number of nodes : ")
  inp_1 <- getLine
  putStrLn (" Enter the density : ")
  inp_2 <- getLine
  let n = (read inp_1 :: Int64)
      d = (read inp_2 :: Double)
      Graph g1 = generateGraph n d
  print $ show g1 
  db <- openDB databaseTest
  (a,b)  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable counters
    tryCreateTable nodeMapTable
    insert counters (return ( "counter", 0 ))
    let Graph g = graph2
        graphmap1 =  Map.fromList g
    dynamicProcess graphmap1 
    a <- select [ x | x <- from graph1Table everything ]
    b <- select [ x | x <- from nodeMapTable everything ]
    return (a,b)
  putStrLn "FROM MAIN"
  mapM_ (\y -> putStrLn (show y) ) a
  mapM_ (\y -> putStrLn (show y) ) b
  closeDB db
  makeDynamicOperation databaseTest ReadWriteMode


main1 :: Int64 -> Double -> IO ()
main1 n d = do
  removeFile databaseTest
  IO.hSetBuffering IO.stdin IO.NoBuffering
  let Graph g1 = generateGraph n d
  print $ show g1 
  db <- openDB databaseTest
  (a,b,c)  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable counters
    tryCreateTable nodeMapTable
    insert counters (return ( "counter", 0 ))
    let Graph g = graph2
        graphmap1 | n == 0 = Map.fromList g
                  | otherwise = Map.fromList g1
    start <- liftIO $  getCurrentTime
    dynamicProcess graphmap1  
    end <- liftIO $  getCurrentTime
    let timePassed = diffUTCTime end start  

    a <- select [ x | x <- from graph1Table everything ]
    b <- select [ x | x <- from nodeMapTable everything ]
    return (a,b,timePassed)
  putStrLn "FROM dailabel"
  mapM_ (\y -> putStrLn (show y) ) a
  mapM_ (\y -> putStrLn (show y) ) b
  print $ "Time for  DaiLabel  for n : " ++ show n ++ " d " ++ show d ++ " : "++ show c
  closeDB db
  makeDynamicOperation databaseTest ReadWriteMode



generateGraph :: Int64 -> Double ->Graph Node
generateGraph n p =  Graph $ map (\x -> (I x,restList x )) {- list@( -}[1..n]
    where 
        restList x= map I $ List.sort $ List.nub (take  (floor (p * fromIntegral (n-x))) $ randomRs (x+1,n) (mkStdGen 3) :: [Int64]  )


getNdIndex node = do
  nod <- select [ind | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
  case nod of
    [nd] -> return nd
    []   -> do 
      pkey <- insert_ nodeMapTable (X node [])
      return pkey
    _    -> error $ "ivalid getindex nd :" ++ show nod



makeDynamicOperation :: String -> AccessMode -> IO()
makeDynamicOperation test_db readwritemode = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    putStrLn ("Enter your choice for (s) for Search (i) for Edge Insertion or (d) for Edge Deletion : ")
    choice <- getChar
    putStrLn (" Enter the first node : ")
    firstChar <- getChar
    putStrLn (" Enter the second node : ")
    secondChar <- getChar
    db <- openDB test_db  
    (a,b) <- runDaison db readwritemode $ do 
      nd1 <- getNdIndex (C firstChar)
      nd2 <- getNdIndex (C secondChar)
      case choice of
        'i' -> handleInsert nd1 nd2
        'd' -> handleDelete nd1 nd2 
        's' -> do 
          flag <- dfsearch nd1 nd2
          liftIO $ print  $ " search result : " ++ show flag
      x <- select [ x | x <- from graph1Table everything ] 
      y <- select [ x | x <- from nodeMapTable everything ]
      return (x,y)
    putStrLn "from make dynamic" 
    mapM_ (\y -> putStrLn (show y) ) a
--    mapM_ (\y -> putStrLn (show y) ) b
    closeDB db
    makeDynamicOperation test_db readwritemode

dynamicProcess :: GraphMap Node -> Daison ()
dynamicProcess graphmap = do
  let firstnode = fst $ Map.elemAt 0 graphmap
  unprocessedGraph <-processNodes graphmap graphmap firstnode firstnode
  foldM_ (\acc x -> processNodes acc acc x x ) unprocessedGraph (Map.keys unprocessedGraph)
  return () 

processNodes :: GraphMap Node -> GraphMap Node -> Node -> Node -> Daison(GraphMap Node)
processNodes graph graphMap node parent_node = do
  nd <- getNdIndex node
  parent <- getNdIndex parent_node
  par <-  query firstRow (from nodeMapTable (at parent))
  case par of
    (X pnd edges) ->
      if List.elem nd edges
         then return graphMap
         else do
            when (parent /= nd) $ handleInsert  parent nd 
            let adjacent = Map.lookup node graph
                gm = Map.delete node graphMap
            case adjacent of
                Nothing      -> return gm
                Just []   -> do 
                  when (parent == nd ) $ do
                    (pre, post) <- insertIsolatedNode
                    store graph1Table (Just nd) (Labels 0   pre post Set.empty Set.empty )
                    return ()
                  return gm
                Just rest    ->do
                  foldM (\acc x -> processNodes graph acc x node) gm rest
  where
    insertIsolatedNode = getCounter >>= \x ->incrementCounter >>incrementCounter >> return (x*gap ,(x+1) *gap)


handleInsert :: Nd -> Nd -> Daison ()
handleInsert nd1 nd2 = do
  res1 <- select (from graph1Table (at nd1))
  res2 <- select (from graph1Table (at nd2))  
  par <-  query firstRow (from nodeMapTable (at nd1))
  allnodes <- getAllNodes
  case par of
    (X pnd edges) ->  store nodeMapTable (Just nd1) (X pnd (edges ++ [nd2]))

  case (res1,res2) of

    ([record1@(Labels tp1 _ _ hp1 _  )],[record2@(Labels tp2 _ _ hp2 dir2  )])   -- Case 1
          | tp2 == 0 -> do            
              if Set.null hp1
                then updateDirectInAncestors nd1 record1 (Set.union dir2)                        -- Case 1.1.1
                else updateDirectInAncestors nd1 record1 (Set.insert nd2)                        -- Case 1.1.2
              update_ graph1Table (return (nd2, record2{tree_parent=nd1}))
              resetCounter
              reLabelAll allnodes [] 'n'
              x <- getCounter
              return ()
          | otherwise -> do  
              if (not (Set.null hp1) || tp1 == 0 || tp1 == nd1)
                then addHop nd1 record1 nd2                                                      -- Case 1.2.1
                else do record <- query firstRow (from graph1Table (at tp1))                     -- Case 1.2.2
                        updateDirectInAncestors tp1 record (Set.insert nd1)
                        addHop nd1 record1 nd2
              return ()

    ([record1@(Labels tp1 pr1 ps1 _ _  )],[]                                          ) ->  -- Case 2
          do 
             (pre,post) <- insertIsolatedNode
             store graph1Table (Just nd2) (Labels nd1 pre post Set.empty Set.empty)
             resetCounter
             reLabelAll (allnodes) [] 'n'
             return ()

    ([]                                       ,[record2@(Labels tp2 _ _ hp2 dir2 )])     -- Case 3
          | tp2 > 0   -> do              
              (pre,post)  <- insertIsolatedNode 
              store graph1Table (Just nd1) (Labels 0 pre post (Set.singleton nd2) Set.empty)
              return ()
          | otherwise -> do  

              (pre,post)  <- insertIsolatedNode              
              let record1 = Labels 0 pre post Set.empty Set.empty 
              store graph1Table (Just nd2) record2{tree_parent=nd1}
--              x <- getCounter
              if Set.null hp2
                then updateDirectInAncestors nd1 record1 (Set.union dir2)
                else updateDirectInAncestors nd1 record1 (Set.insert nd2)
              resetCounter
              --reLabelAll (allnodes) [] 'n'
              relabel nd1 [] 'n'
              return ()
    ([]                                       ,[]                                          ) ->  -- Case 4
          do 
             (pre1,pre2,post1,post2) <- insert2IsolatedNodes {- TODO: insert four labels after (PreLabel root) -}
             store graph1Table (Just nd1) (Labels 0   pre1 post1 Set.empty Set.empty )
             store graph1Table (Just nd2) (Labels nd1 pre2 post2 Set.empty Set.empty )
             return ()
  return()
  where 
    average x y = x + div (y-x) 2
    insertIsolatedNode = getCounter >>= \x ->incrementCounter >>incrementCounter >> return (x*gap ,(x+1) *gap)
    insert2IsolatedNodes = getCounter >>= \x ->incrementCounter >> incrementCounter >>
      incrementCounter >> incrementCounter >> return ((x*gap),(x+1)*gap,(x+2)*gap,(x+3)*gap)



getAllNodes :: Daison [Nd]
getAllNodes = do 
  nodes <- select [ind | (ind, ( X nd nodeindex )) <- from nodeMapTable everything  ]
  return nodes 




handleDelete :: Nd -> Nd -> Daison ()
handleDelete nd1 nd2 = do
  istreeEdge <- isTreeEdge nd1 nd2
  deleteEdge nd1 nd2
  case istreeEdge of
    True -> do
      deleteDirectsandAncestors nd1 nd2
      removeTreeParent nd2
      relabel nd2 [] 's'
      return()
    False -> do
      flag <- isTheOnlyNonTreeEdge nd1 nd2
      when flag $
        do
          record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
          case record of
            [(nd , Labels tp pr ps hp dir )] -> do
              deleteDirectsandAncestors tp nd1
              when ( not $ Set.null dir && tp/= 0 ) $ do
                record <- query firstRow (from graph1Table (at tp))
                updateDirectInAncestors tp record (Set.union dir)
            _ -> error $ "invalid from handle delete : " ++ show nd1 ++ show nd2
      deleteHopsFrom nd1 nd2
      return ()

addHop :: Nd -> Labels -> Nd -> Daison ()
addHop nd1 (Labels tp pr ps hp dir ) nd2 = do
  store graph1Table (Just nd1) (Labels tp pr ps (Set.insert nd2 hp) dir )
  return ()

deleteEdge :: Nd -> Nd -> Daison ()
deleteEdge nd1 nd2 = do 
  record <- select [(n, edgs) | (X n edgs) <- from nodeMapTable (at nd1)  ] 
  case record of
        [(n , edgs)] -> update_ nodeMapTable (return (nd1, X n (List.delete nd2 edgs)) )
        _            -> error $ show nd1 ++ " -  " ++ show nd2 ++ " is not an edge"


removeTreeParent :: Nd -> Daison ()
removeTreeParent nd1 = do 
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir )] -> do
      update_ graph1Table (return (nd, Labels 0 pr ps hp dir) )
    


deleteDirectsandAncestors :: Nd -> Nd -> Daison()
deleteDirectsandAncestors nd1 nd2 = do
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir )] -> do
      update_ graph1Table (return (nd, Labels tp pr ps hp (Set.delete nd2 dir)  ) )
      when (tp/=0 && tp /= nd2) $ deleteDirectsandAncestors tp nd2
    _ -> error $ "invalid from deletedirectss and ancestors " ++ show nd1 ++ show nd2


deleteHopsFrom :: Nd -> Nd -> Daison ()
deleteHopsFrom nd1 nd2 = do
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir )] -> do
      update_ graph1Table (return (nd, Labels tp pr ps (Set.delete nd2 hp) dir ) )
    _ -> error "invalid"  


updateDirectInAncestors :: Nd -> Labels -> (Directs -> Directs) -> Daison ()
updateDirectInAncestors nd (Labels tp pr ps hp dir ) f = do
  store graph1Table (Just nd) (Labels tp pr ps hp (f dir) )
  when (tp /= 0 && tp /= nd) $ do
    record <- query firstRow (from graph1Table (at tp))
    updateDirectInAncestors tp record f


isTreeEdge :: Nd -> Nd -> Daison Bool
isTreeEdge nd1 nd2 = do
  record <- select [label1 | (label1) <- from graph1Table (at nd1)  ] 
  case record of 
    [(Labels tp pr ps hp dir )] -> case (List.elem nd2 hp) of
      True -> return False
      False -> do 
        nodemap_record <- select [edgs | ( X n edgs) <- from nodeMapTable (at nd1)  ]
        let nd_edges = head nodemap_record
        if (List.elem nd2 nd_edges) then 
          return True
        else 
          return False
    _ -> error $ " from istreeedge : " ++ show nd1 ++ show nd2
 

isTheOnlyNonTreeEdge :: Nd -> Nd -> Daison Bool
isTheOnlyNonTreeEdge nd1 nd2 = do
  record <- select [(nd1,label1) | (label1) <- from graph1Table (at nd1)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir  ->  do 
        if (Set.member nd2 hp) && (Set.size hp ==1) then 
          return True
        else if (Set.member nd2 hp) && (Set.size hp > 1) then
          return False
        else 
          error $ "error from istheonlynontreeedge : nd1 , nd2 : " ++ show nd1 ++ show nd2
    _ -> error " couldnt find record from isonlynontreeedge "


getParent :: Nd -> Daison Nd
getParent node = do
  record <- select [(node, labels) | (labels) <- from graph1Table (at node) ] 
  case record of
    [] -> error $ "invalid parent node :" ++ show node
    [(ind1,  label1)] -> case label1 of 
      (Labels trp _ _ _ _  ) -> return trp
    _           -> error "multiple parents error "

getCounter :: Daison Nd
getCounter = select [ x | x <- from counters (at 1) ] >>= \p -> return . snd . head $ p  


getMax :: Daison Nd
getMax = select [ x | x <- from counters (at 1) ] >>= \p -> return . snd . head $ p  


updateMax :: Nd -> Daison()
updateMax newmax =   update_ counters (return (1, ("l_max", newmax) )) 



incrementCounter :: Daison ()
incrementCounter = do
  c_counter <- getCounter  
  update_ counters (return (1, ("counter", c_counter+1) )) 
  


resetCounter :: Daison ()
resetCounter = update_ counters (return (1, ("counter", 0) ))



queryM :: Nd -> Nd -> Daison Bool
queryM nd1 nd2 = do
  label1 <- select [labels | (labels) <- from graph1Table (at nd1)  ] 
  label2 <- select [labels | (labels) <- from graph1Table (at nd2)  ]
  case label1 of 
    [(Labels trp1 pre1 post1 hp1 dir1 )] -> do
      case label2 of
        [(Labels trp2 pre2 post2 hp2 dir2)] -> if  (pre1 < post2 && post2 <= post1) then return True
                                             else return False
        _ -> error "error "                
    _ -> error "error again "


search :: Nd -> Nd -> Daison Bool
search nd1 nd2 = do
  label1 <- select [labels | (labels) <- from graph1Table (at nd1)  ] 
  label2 <- select [labels | (labels) <- from graph1Table (at nd2)  ]
  case label1 of 
    [(Labels trp1 pre1 post1 hp1 dir1 )] -> do
      flag <- queryM nd1 nd2
      if flag then return True
      else do
        x <- or <$> (mapM (\x -> queryM x nd2) (Set.toList hp1)) 
        if not x 
          then or <$> (mapM (\x -> queryM x nd2) (Set.toList dir1)) 
        else return x

dfsearch :: Nd -> Nd -> Daison Bool
dfsearch nd1 nd2 = do 
  record <- select [edgs | (X n edgs) <- from nodeMapTable (at nd1)  ] 
  case (head record) of
    lis@(first : rest) -> case (List.elem nd2 lis) of
      True -> return True
      False -> or <$> (mapM (\x -> dfsearch x nd2) rest)


reLabelAll :: [Nd] -> [Nd] -> Char-> Daison ()
reLabelAll  all visited t = do 
  let difference = all List.\\ visited
  when (length difference > 0 ) $ do 
    updatedVisited <-  relabel (head difference) visited t --(S "root")
    reLabelAll all updatedVisited t
    return ()
  return()


relabel :: Nd -> [Nd] -> Char-> Daison [Nd]
relabel nd visited t =  do 
  x <- updatePre nd visited
  if x then return visited
       else do
         edges <- if t == 's' then getTreeEdges nd else getEdges nd
         --getTree Edges is used to relabel the spanning tree when an edge is deleted in case 1
         --getEdges is used to relabel the whole spanning tree
         nv <- case edges of
           [] -> return visited
           rest -> foldM (\acc y -> relabel y acc t) visited  rest
         updatePost nd 
         return (nd : nv)
 
updatePre :: Nd -> [Nd] -> Daison Bool
updatePre nd visited = do 
  record <- select [(nd,label1) | (label1) <- from graph1Table (at nd)  ] 
  case record of 
    [(nd, Labels trp pr ps hp dir)] -> 
      if pr == ps || elem nd visited then return True
        else do
          c_counter <- getCounter
          incrementCounter
          update_ graph1Table (return (nd, Labels trp (c_counter*gap) (c_counter*gap) hp dir  ))
          return False
    _ -> error "error " 

updatePost :: Nd -> Daison ()
updatePost nd = do 
  record <- select [(nd,label1) | (label1) <- from graph1Table (at nd)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir  ->  do 
        c_counter <- getCounter
        incrementCounter
        update_ graph1Table (return (nd, Labels trp pr (c_counter*gap) hp dir ))
      _   -> error "error from updatepost"
    _ -> error "error " 



getTreeEdges :: Nd -> Daison [Nd]
getTreeEdges nd = do 
  record <- select [ treeEdges edgs | ( X n edgs) <- from nodeMapTable (at nd)  ] 
  record1 <- head record
  return record1
  where
    treeEdges es = filterM (\nd2 -> isTreeEdge nd nd2 ) es



getEdges :: Nd -> Daison [Nd]
getEdges nd = do 
  record <- select [ edgs| ( X n edgs) <- from nodeMapTable (at nd)  ] 
  return . head $ record

updateDirects :: Nd -> Nd -> Daison()
updateDirects parent gp = do
  record <- select [(gp,label1) | (label1) <- from graph1Table (at gp)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir ->  do
        update_ graph1Table (return (nd, Labels trp pr ps hp (Set.insert parent dir) ))
      _ -> error "updatedirects error"
    _   -> liftIO $ print record
  when (gp > 0) $ do
    ggp <- getParent gp
    when (ggp > 0) $ updateDirects parent ggp
