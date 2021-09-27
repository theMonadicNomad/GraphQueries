
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
import Data.Maybe (fromJust, fromMaybe)
import System.Process (callProcess, callCommand)
import System.Directory
import Data.Int
import System.Random
import Debug.Trace
import Data.Time

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

graph12 :: Graph Node
graph12 = Graph
    [ (I 1, [ I 2, I 3] ),
      (I 2, [ ] ),
      (I 3, [I 5]),
      (I 4, [I 5]),
      (I 5, [I 6] ),
      (I 6, [ I 7] ),
      (I 7, [ ] ),
      (I 8, [] )
    ]
 





instance Show Labels where
  show (Labels {- a -} b c d e) = {- "TP: " ++  show a ++ -} " Pre: " ++ show b ++ " Post:  " ++ show c  ++ " Hops: " ++ show d ++ " Directs:  " ++ show e 


graph1Table :: Table (Labels)
graph1Table = table "graph1"

nodeMapTable :: Table X
nodeMapTable = table "nodemap" `withIndex` nodemap_index

--nodemap_index :: Index Y Special2
nodemap_index :: Index X Node
nodemap_index = index nodeMapTable "nodemap_index" nd


databaseTest = "ailabel.db"


generateGraph :: Int64 -> Double ->Graph Node
generateGraph n p =  Graph $ map (\x -> (I x,restList x )) {- list@( -}[1..n]
    where 
        restList x= map I $ List.sort $ List.nub (take  (floor (p * fromIntegral (n-x))) $ randomRs (x+1,n) (mkStdGen 3) :: [Int64]  )


generateTreeGraph :: Int64 -> Int64 -> Int64-> IO (Graph Node)
generateTreeGraph  total n p  = do
  if total == 0
    then return $ Graph []
    else do
      let rt = 1 
      (lrt , ch) <- genChild (total,  rt + 1 , n) rt
      let rest = (\x -> (I x, [])) <$> ([lrt+1 .. total])
      final <- mapM (addMoreEdges total p) (ch ++ rest)
      return $ Graph final

addMoreEdges :: Int64 -> Int64 -> (Node, [Node]) -> IO (Node, [Node])
addMoreEdges total n (I x, []) = if x == total 
    then return (I x , [])
    else do
      gen <- newStdGen
      let (nv,gen') = randomR (0,n) gen
          ls = take (fromEnum nv) (randomRs (x+1,total) gen') 
      return (I x, I <$> (List.sort $ List.nub ls))
addMoreEdges total n (I x, ls) = do
  gen <- newStdGen
  let I ns = last ls
      (nv,gen') = randomR (0,n) gen
      ls' = if (ns==total) then [] else take (fromEnum nv) (randomRs (ns+1,total) gen') 
  return (I x, ls ++ (I <$> (List.sort $ List.nub ls')))

genChild :: (Int64, Int64, Int64) -> Int64 -> IO (Int64, [(Node, [Node])])
genChild (total, curr, n) rt = do
  children <- generateChildren n curr total
  let curr' = if null children && curr - 1 == rt
                then curr + 1
                else curr + (fromIntegral (length children ))
  (rt',ch) <-
    if total <= curr'
      then return (rt, [])
      else do
        genChild (total, curr', n) (rt+1) 
  return (rt', (I rt,children) : ch)

generateChildren :: Int64 -> Int64 -> Int64-> IO [Node]
generateChildren n c total =   do
  gen <- newStdGen
  let values = fst $ (randomR (1,n) gen )
  -- putStrLn $ show (values)
  return (I <$> [c .. (mod (c+ values-1) total) ] )




main1 :: Int64 -> Int64 ->Int64-> IO ()
main1 n d p = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  Graph g1 <- generateTreeGraph n d p
  let Graph g = graph2
  let graphmap1 | n == 0 = Map.fromList g
                | otherwise = Map.fromList g1
  print $ show graphmap1
  removeFile databaseTest

  db <- openDB databaseTest
  (x,y,z) <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable nodeMapTable
    start <- liftIO $ getCurrentTime
    process graphmap1
    end <- liftIO $ getCurrentTime
    let timePassed = diffUTCTime end start  
    liftIO $ print timePassed
    x<-select (from graph1Table everything)
    y<-select (from nodeMapTable everything)
    makeDynamic n
    return (x,y,timePassed)

  putStrLn "-------------------"
  mapM_ print x
  mapM_ print y
  print $ "Time for AILabel  for n : " ++ show n ++ " d " ++ show d ++ " : "++ show z

  closeDB db
  makeDynamicOperation databaseTest ReadWriteMode


    
process :: GraphMap Node-> Daison ()
process graph = do
  foldM_ (\index x -> do (index',id,is_tree) <- processNodes graph index x
                         return index')
         0
         (Map.toList graph)

processNodes :: GraphMap Node -> Int -> (Node,[Node]) -> Daison (Int,Nd,Maybe [Nd])
processNodes graph index (nd,edges) = do
  res <- select (from nodemap_index (at nd))
  case res of
    []   -> do (index1,tree_edges,dirs0,hops) <-
                  foldM (\(i,tree_edges,dirs,hops) child -> do
                              let edges = fromMaybe [] (Map.lookup child graph)
                              (i,id,is_tree) <- processNodes graph i (child,edges)
                              case is_tree of
                                Nothing    -> return (i,tree_edges,dirs,id:hops)
                                Just dirs' -> return (i,id:tree_edges,dirs'++dirs,hops))
                        (index+1,[],[],[])
                        edges
               id <- store nodeMapTable Nothing (X nd (tree_edges++hops))
               let dirs | null hops = dirs0
                        | otherwise = id:dirs0
               store graph1Table (Just id) (Labels index index1 (Set.fromList hops) (Set.fromList dirs))
               return (index1+1,id,Just dirs)
    [id] -> return (index,id,Nothing)


getNdIndex node = do
  nod <- select [ind | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
  case nod of
    [nd] -> return nd
    []   -> error $ "no such node in the database" ++ show nod
    _    -> error $ "ivalid getindex nd :" ++ show nod


makeDynamic  :: Int64 -> Daison ()
makeDynamic p = do
  liftIO $ print $  "Enter number of searches : "
  firstChar <- liftIO $ getLine
  let n = read firstChar :: Int64
  foldM_ (\_ m -> do 
                      (x,y) <- liftIO $ getRandomNodes p
                      resx <- select (from nodemap_index (at x))
                      resy <- select (from nodemap_index (at y))
                      processSearching (head resx) (head resy)
                      return ()                    
                   )
                   ()
                   [1..n]
  makeDynamic p

getRandomNodes :: Int64 -> IO(Node,Node)
getRandomNodes n = do 
      gen <- newStdGen
      let a:b:_ = take 2 (randomRs (1,n) gen) 
      if (a<b) then return (I a,I b) else return (I b,I a)

processSearching :: Nd ->Nd-> Daison()
processSearching nd1  nd2 = do 
    liftIO $ print  $ " nd1:  " ++ show nd1 ++ " nd2: " ++ show nd2

    start1 <- liftIO $ getCurrentTime
    flag1 <- search nd1 nd2
    end1 <- liftIO $ getCurrentTime
    let timePassed1 = diffUTCTime end1 start1  
    --liftIO $ print timePassed
    liftIO $ print  $ " Result : " ++ show flag1 ++ " Time Taken for  AILabel Search : "++show timePassed1

    start <- liftIO $ getCurrentTime
    (flag, count) <- df_search nd1 nd2 0
    end <- liftIO $ getCurrentTime
    let timePassed = diffUTCTime end start  
    --liftIO $ print timePassed
    liftIO $ print  $ " Result : " ++ show flag ++ " Nodes: " ++ show count ++ " Time Taken for Depth First Search : "++show timePassed

    start2 <- liftIO $ getCurrentTime
    (flag2) <- dfsearch nd1 nd2 
    end2 <- liftIO $ getCurrentTime
    let timePassed2 = diffUTCTime end2 start2  
    --liftIO $ print timePassed
    liftIO $ print  $ " Result : " ++ show flag2  ++ " Time Taken for Depth First Search : "++show timePassed2
    return ()





makeDynamicOperation :: String -> AccessMode -> IO()
makeDynamicOperation test_db readwritemode = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
{-     putStrLn ("Enter your choice for (s) for Search (i) for Edge Insertion or (d) for Edge Deletion : ")
    choice <- getChar -}
    putStrLn (" Enter the first node to search : ")
    firstChar <- getLine
    putStrLn (" Enter the second node : ")
    secondChar <- getLine
    db <- openDB test_db  
    runDaison db readwritemode $ do 
      nd1 <- getNdIndex (I (read firstChar :: Int64))
      nd2 <- getNdIndex (I (read secondChar :: Int64))
      start1 <- liftIO $ getCurrentTime
      flag1 <- search nd1 nd2
      end1 <- liftIO $ getCurrentTime
      let timePassed1 = diffUTCTime end1 start1  
      --liftIO $ print timePassed
      liftIO $ print  $ " Result : " ++ show flag1 ++ " Time Taken for  AILabel Search : "++show timePassed1

      start <- liftIO $ getCurrentTime
      (flag, count) <- df_search nd1 nd2 0
      end <- liftIO $ getCurrentTime
      let timePassed = diffUTCTime end start  
      --liftIO $ print timePassed
      liftIO $ print  $ " Result : " ++ show flag ++ " Nodes: " ++ show count ++ " Time Taken for Depth First Search : "++show timePassed

      start2 <- liftIO $ getCurrentTime
      (flag2) <- dfsearch nd1 nd2 
      end2 <- liftIO $ getCurrentTime
      let timePassed2 = diffUTCTime end2 start2  
      --liftIO $ print timePassed
      liftIO $ print  $ " Result : " ++ show flag2  ++ " Time Taken for Depth First Search : "++show timePassed2


{-       x <- select [ x | x <- from graph1Table everything ] 
      y <- select [ x | x <- from nodeMapTable everything ] -}
      return ()
    putStrLn "make dynamic" 
--    mapM_ (\y -> putStrLn (show y) ) a
--    mapM_ (\y -> putStrLn (show y) ) b
    closeDB db
    makeDynamicOperation test_db readwritemode





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
        x <- or <$> (mapM (\x -> search x nd2) (Set.toList hp1)) 
        if not x 
          then or <$> (mapM (\x -> queryM x nd2) (Set.toList dir1)) 
        else return x

dfsearch :: Nd -> Nd -> Daison Bool
dfsearch nd1 nd2 = do 
  record <- select [edgs | (X n edgs) <- from nodeMapTable (at nd1)  ] 
{-   liftIO  $ print  $ " from liftio graph " ++ show record ++ " for node : " ++ show nd1 -}
  case (head record) of
    lis@(first : rest) -> case (List.elem nd2 lis) of
      True -> return True
      False -> or <$> (mapM (\x -> dfsearch x nd2) lis)
    [] -> return False


df_search :: Nd -> Nd ->Int64-> Daison (Bool, Int64)
df_search nd1 nd2 count = do 
  record <- select [edgs | (X n edgs) <- from nodeMapTable (at nd1)  ] 
{-   liftIO $ print $ " countMaam : " ++ show (count) ++ " X : " ++ show nd1 -}
{-   liftIO  $ print  $ " from liftio graph " ++ show record ++ " for node : " ++ show nd1 -}
  case (head record) of
    lis@(first : rest) -> case (List.elem nd2 lis) of
      True -> return (True, count)
      False -> do (b',i') <-foldM (\(b, i) x -> do 
                                     (b',i') <- df_search x nd2 (i+1)
                                    
                                     return (b'||b , i'))
                                  (False, (count))
                                  lis
{-                   liftIO $ print $ " count : " ++ show i' ++ " X : " ++ show nd1  -}
                  return (b',i')
    [] ->  return (False,count)





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
