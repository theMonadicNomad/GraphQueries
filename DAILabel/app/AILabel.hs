
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module AILabel where 

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
import Data.Int
import System.Random

type Nd = Key Labels
--  deriving (Eq, Ord, Read, Data)
type Ndc = Char
type Nds = [Char]
  --deriving (Eq, Ord, Read, Data)
type Hops = Set Nd
type Directs = Set Nd
type Pre = Int
type Post = Int
--type Graph =  [(Ndc, [Ndc])]
data Graph a = Graph [(a, [a])] 
  deriving Show


type GraphMap a = Map a [a]
type Special = Bool
type Record = (Nd, Labels)
type Edges = [Nd]


instance Arbitrary (Graph Int) where
  arbitrary = do 
    x <- arbitrary :: Gen Int
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


type Distance = Int

data City = City { name :: String,
 id :: Int,
 population :: Int,
 paths :: [(Key City, Distance)] 
} deriving (Typeable, Data )
  

data Labels = Labels {
{-     tree_parent :: Nd, -}
    pre :: Pre,
    post :: Post,
    hops :: Hops,
    directs :: Directs
} deriving (Typeable, Data )

--type Y = X Special2

data X = X { nd :: Node,  
     edges :: Edges
} deriving (Typeable, Data, Show)



{- data X Ndc = X { ndc :: Ndc, 
     edges :: Edges
} deriving (Typeable, Data, Show)
 -}

data Node = C Ndc | I Nd | S Nds deriving (Eq,Data, Ord, Show)


isChar :: Node -> Bool
isChar (C _) = True
isChar _ = False

isInt :: Node -> Bool
isInt (I _) = True
isInt _ = False

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


instance Show Labels where
  show (Labels {- a -} b c d e) = {- "TP: " ++  show a ++ -} " Pre: " ++ show b ++ " Post:  " ++ show c  ++ " Hops: " ++ show d ++ " Directs:  " ++ show e 



parentTable :: Table Nd
parentTable = table "parentGraph1" `withIndex` parent_index


parent_index :: Index Nd Nd 
parent_index = index parentTable "parent_index" Prelude.id



graph1Table :: Table (Labels)
graph1Table = table "graph1"
            `withIndex` graph_index

graph_index :: Index Labels Labels 
graph_index = index graph1Table "node_index" Prelude.id

nodeMapTable :: Table X
nodeMapTable = table "nodemap" `withIndex` nodemap_index

--nodemap_index :: Index Y Special2
nodemap_index :: Index X Node
nodemap_index = index nodeMapTable "nodemap_index" nd

counters :: Table (String, Int)
counters = table "counter" 
             `withIndex` counter_index

counter_index :: Index (String, Int) String
counter_index = index counters "counter_index" fst

databaseTest = "ailabel.db"


generateGraph :: Int64 -> Double ->Graph Node
generateGraph n p =  Graph $ map (\x -> (I x,restList x )) {- list@( -}[1..n]
    where 
        restList x= map I $ List.sort $ List.nub (take  (floor (p * fromIntegral (n-x))) $ randomRs (x+1,n) (mkStdGen 3) :: [Int64]  )


{- main1 :: IO ()
main1 = do
  quickCheck prop_graphCSearch
 -}

main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
{-   putStrLn ("Enter the number of nodes : ")
  inp_1 <- getLine
  putStrLn (" Enter the density : ")
  inp_2 <- getLine
  let n = (read inp_1 :: Int64)
  let d = (read inp_2 :: Double)
  let Graph g1 = generateGraph n d
  print $ show g1
 -}
  db <- openDB databaseTest
  (a,b)  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable counters
    tryCreateTable nodeMapTable
    tryCreateTable parentTable
{-     c_counter <- getCounter
    if (c_counter >0) then return ()
    else
      do  -}     
    insert counters (return ( "counter", 0 ))
    let Graph g = graph2
    let graphmap1 =  Map.fromList g
    process graphmap1
    a <- select [ x | x <- from graph1Table everything ]
    b <- select [ x | x <- from parentTable everything ]
    return (a,b)
  mapM_ (\y -> putStrLn (show y) ) a
  mapM_ (\y -> putStrLn (show y) ) b
  closeDB db

main1 :: Int64 -> Double -> IO ()
main1 n d = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
{-   putStrLn ("Enter the number of nodes : ")
  inp_1 <- getLine
  putStrLn (" Enter the density : ")
  inp_2 <- getLine
  let n = (read inp_1 :: Int64)
  let d = (read inp_2 :: Double)
  let Graph g1 = generateGraph n d
  print $ show g1
 -}
  db <- openDB databaseTest
  (a,b)  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable counters
    tryCreateTable nodeMapTable
    tryCreateTable parentTable
{-     c_counter <- getCounter
    if (c_counter >0) then return ()
    else
      do  -}     
    insert counters (return ( "counter", 0 ))
    let Graph g1 = generateGraph n d
    let Graph g = graph2
    let graphmap1 =  Map.fromList g1
    process graphmap1
    liftIO $ print $ " From AILabel " 
    a <- select [ x | x <- from graph1Table everything ]
    b <- select [ x | x <- from parentTable everything ]
    return (a,b)
  mapM_ (\y -> putStrLn (show y) ) a
  mapM_ (\y -> putStrLn (show y) ) b
  closeDB db




    
process :: GraphMap Node-> Daison ()
process graphmap = do
  let firstnode = fst $ Map.elemAt 0 graphmap
  processNodes graphmap firstnode firstnode
  processRemainingNodes graphmap

processRemainingNodes :: GraphMap Node -> Daison ()
processRemainingNodes graphmap = do 
  nodes <- select [nd | (ind, ( X nd nodeindex )) <- from nodeMapTable everything  ]
  let graphlist = Map.toList graphmap
  let nodelist = map (\(x,y) -> x ) graphlist
  let difference = nodelist List.\\ nodes
  liftIO $ print $ " nodes :  " ++ show nodes
  liftIO $ print $ " difference : " ++ show difference
  liftIO $ print $ " nodelist : " ++ show nodelist
  when (length difference > 0 ) $ do 
    processNodes graphmap (head difference) (head difference)
    processRemainingNodes graphmap
  return()

processNodes :: GraphMap Node -> Node -> Node -> Daison()
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
  nod <- select [ind | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
  case nod of
    [nd] -> return nd
    []   -> do 
      --c_counter <- getCounter
      --incrementCounter >> incrementCounter
      pkey <- insert_ nodeMapTable (X node [])
      store  graph1Table (Just pkey) (Labels {- (-1) -} (-3) (-2) Set.empty Set.empty  )
      return pkey
    _    -> error $ "ivalid getindex nd :" ++ show nod

insertNodeinDB :: Node -> Node -> Daison Bool
insertNodeinDB node parent = do
  map <- select [ind | (ind, ( X nd edgess )) <- from nodeMapTable everything , nd == node  ]
  par <- if (node == parent) then return [(0,(S "root",[]))] else select [(ind,(nd, edgess)) | (ind, ( X nd edgess )) <- from nodeMapTable everything , nd == parent  ]
--  liftIO $ print $ " map : " ++ show map ++ " par" ++ show par 
  let parent_ndmp = fst . head $ par
  let edges_par = snd . snd . head $ par 
  let parent_ndc = fst . snd . head $ par
  case map of
    [] -> do
      c_counter <- getCounter
      incrementCounter
      pkey <-  insert_ nodeMapTable (X node [])
      store graph1Table (Just pkey) (Labels {- parent_ndmp -} c_counter c_counter Set.empty Set.empty )    
      store parentTable (Just pkey) parent_ndmp
      when (parent_ndmp > 0) $ do
--        parent_record <- select [(parent_ndmp, labels2) | (labels2) <- from graph1Table (at parent_ndmp)  ] 
--        case parent_record of
--          [(indp ,(Labels ptrp ppr pps php pdir pte))] -> update_ graph1Table (return (indp,(Labels ptrp ppr pps php pdir (pkey:pte)) ))
--        parent_ndmprecord <- select [(ind,edgess) | (ind, ( X nd edgess )) <- from nodeMapTable everything , nd == parent  ]
        update_ nodeMapTable  (return (parent_ndmp, (X parent_ndc (pkey:edges_par) ) ))

      return False
    [nod]   ->  do
      parent_record <- select [(parent_ndmp, labels2) | (labels2) <- from graph1Table (at parent_ndmp)  ] 
      case parent_record of 
          [] -> error "error "
          [(indp, labelp)] -> case labelp of 
            (Labels {- ptrp -} ppr pps php pdir) -> do
              update_ graph1Table (return (indp,(Labels {- ptrp -} ppr pps (Set.insert (head map) php ) pdir) ))
              ptrp1 <- getParent1 parent_ndmp
              when (ptrp1 > 0) $ updateDirects parent_ndmp ptrp1 
      update_ nodeMapTable  (return (parent_ndmp, (X parent_ndc (nod:edges_par) ) ))

      return True 
{-     first : rest -> error "duplicate records in the database table, please verify"
 -}

updatePost :: Nd -> Daison ()
updatePost nd = do 
  record <- select [(nd,label1) | (label1) <- from graph1Table (at nd)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels {- trp -} pr ps hp dir  ->  do 
        c_counter <- getCounter
        incrementCounter
        update_ graph1Table (return (nd, Labels {- trp -} pr c_counter hp dir ))
      _   -> error "error from updatepost"
    _ -> error "error " 

updateDirects :: Nd -> Nd -> Daison()
updateDirects parent gp = do
  record <- select [(gp,label1) | (label1) <- from graph1Table (at gp)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels {- trp -} pr ps hp dir ->  do
        update_ graph1Table (return (nd, Labels {- trp -} pr ps hp (Set.insert parent dir) ))
      _ -> error "updatedirects error"
    _   -> liftIO $ print record
  when (gp /= 0) $ do
    ggp <- getParent1 gp
    when (ggp /= 0) $ updateDirects parent ggp

{- getParent :: Nd -> Daison Nd
getParent node = do
  record <- select [(node, labels) | (labels) <- from graph1Table (at node) ] 
  case record of
    [] -> error $ "invalid parent node :" ++ show node
    [(ind1,  label1)] -> case label1 of 
      (Labels trp _ _ _ _ ) -> return trp
    _           -> error "multiple parents error "
 -}
getParent1 :: Nd -> Daison Nd
getParent1 node = do
  record <- select [parent | parent <- from parentTable (at node) ] 
  case record of
    [] -> error $ "invalid parent node :" ++ show node
    [par] ->  return par
    _           -> error "multiple parents error "

 
getCounter :: Daison Int
getCounter = select [ x | x <- from counters (at 1) ] >>= \p -> return . snd . head $ p  

incrementCounter :: Daison ()
incrementCounter = do
  c_counter <- getCounter  
  liftIO $ print $ " counter current value : " ++ show (c_counter+1) 
  update_ counters (return (1, ("counter", c_counter+1) )) 
  


resetCounter :: Daison ()
resetCounter = update_ counters (return (1, ("counter", 0) ))


queryM :: Nd -> Nd -> Daison Bool
queryM nd1 nd2 = do
  label1 <- select [labels | (labels) <- from graph1Table (at nd1)  ] 
  label2 <- select [labels | (labels) <- from graph1Table (at nd2)  ]
  case label1 of 
    [(Labels {- trp1 -} pre1 post1 hp1 dir1)] -> do
      case label2 of
        [(Labels {- trp2 -} pre2 post2 hp2 dir2)] -> if  (pre1 < post2 && post2 <= post1) then return True
                                             else return False
        _ -> error "error "                
    _ -> error "error again "


search :: Nd -> Nd -> Daison Bool
search nd1 nd2 = do
  label1 <- select [labels | (labels) <- from graph1Table (at nd1)  ] 
  label2 <- select [labels | (labels) <- from graph1Table (at nd2)  ]
  case label1 of 
    [(Labels {- trp1 -} pre1 post1 hp1 dir1)] -> do
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



{- prop_graphISearch :: Graph Int-> Property
prop_graphISearch graph =undefined 



prop_graphCSearch :: Graph Ndc -> Property
prop_graphCSearch graph = monadicIO $ do 
  (a ,b) <- run $! ( helper_quickcheck graph)
  assert (a == b) 

helper_quickcheck :: Graph Node  -> IO (Bool, Bool)
helper_quickcheck graph  =  do
    callCommand ("ls")
   -- callCommand ("rm " ++ databaseTest)
    callCommand("ls")
    db <- openDB databaseTest 
    print  $ " from liftio outside daison : ++ " ++ show databaseTest
     
    (x,y) <-  runDaison db ReadWriteMode $ do 
      tryCreateTable graph1Table
      tryCreateTable counters
      tryCreateTable nodeMapTable
      insert counters (return ( "counter", 0 ))
      let Graph g = graph
      let graphmap1 =  Map.fromList g

      
      liftIO  $ print  $ " from liftio graph " ++ show graphmap1
      flagR <- 
        if (Map.size graphmap1 > 2)
           then do
              liftIO $ print  $ " from liftio graph " ++ show graphmap1
              process graphmap1
              nd1 <- getNdIndex (fst $ Map.elemAt 0 graphmap1)
              nd2 <- getNdIndex (fst $ Map.elemAt 1 graphmap1)
              flag1 <- dfsearch nd1 nd2
              flag2 <- search nd1 nd2 
            
              return (flag1, flag2)
           else  
              return (True, True)
      dropTable graph1Table
      dropTable counters
      dropTable nodeMapTable
      return flagR

    closeDB db

    callCommand ("rm " ++ databaseTest)
    --writeFile databaseTest ""
    return (x,y) -}