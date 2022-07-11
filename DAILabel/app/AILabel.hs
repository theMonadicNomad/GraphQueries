
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

{- generates tree graphs by taking the input of total number of nodes(n), maximum number of total tree edges(n)
  and maximum number of non-tree edges -}
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




main1 :: Int64 -> Int64 ->Int64-> Int64-> IO ()
main1 n d p benchmarking_flag = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  Graph g1 <- generateTreeGraph n d p
  let Graph g = graph2
  let graphmap1 | n == 0 = Map.fromList g
                | otherwise = Map.fromList g1
  when (n<50) $ print $ show graphmap1
--  removeFile databaseTest
  let b = if p == 0 
            then (length g1-1)
            else foldl (\x y -> x + length ( snd y)) 0 g1
 -- liftIO $ print  $ " Total Edges : " ++ show b ++ " Tree edges : " ++ show (length g1 - 1) ++ " Non Tree Edges :" ++ show (b -(length g1 -1))
  liftIO $ print  $ "Edges: " ++ show b ++ " Tree: " ++ show (length g1 - 1) ++ " NonTree:" ++ show (b -(length g1 -1)) ++ " NTE%: " ++ show (  fromIntegral (b -(length g1 -1)) *100/ fromIntegral b)

  --let c = (b -(length g1 -1)) / b
--  liftIO $ print $ " NonTree Edges Percentage: " ++ show (  fromIntegral (b -(length g1 -1)) *100/ fromIntegral b)
--{-  
  print $ " Do you want to use the previous database ? (y/n): "
  previous_data <- getChar
  if (previous_data == 'y') then do
    db1 <- openDB databaseTest
    runDaison db1 ReadWriteMode $ do
      performRandomSearch n
      return ()
    closeDB db1 --}
    else do
      removeFile databaseTest
      db <- openDB databaseTest
      (x,y,z) <- runDaison db ReadWriteMode $ do
          tryCreateTable graph1Table
          tryCreateTable nodeMapTable
          start <- liftIO $ getCurrentTime
          staticProcess graphmap1
          end <- liftIO $ getCurrentTime
          let timePassed = diffUTCTime end start  
          edgesPercent
--    liftIO $ print $ " AILabel Index creation time:" ++  show timePassed 
          x<-select (from graph1Table everything)
          y<-select (from nodeMapTable everything)
          when (benchmarking_flag == 0) $ do performRandomSearch n
          return (x,y,  timePassed)

--  putStrLn "-------------------"
      print $ "TimeAILabel n : " ++ show n ++ " d: " ++ show d ++ " p: " ++ show p ++" : " ++ show z

      if (benchmarking_flag == 0) then 
        do
          print $ " Do you want to display all the contents of graph and nodemap table (y/n): "
          process_char <- getChar
          when (process_char == 'y') $ do
            mapM_ print x
            mapM_ print y
          closeDB db
          performManualSearch databaseTest ReadWriteMode
        else closeDB db

staticProcess :: GraphMap Node-> Daison ()
staticProcess graph = do
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


performRandomSearch  :: Int64 -> Daison ()
performRandomSearch p = do
  liftIO $ print $  "Enter number of searches : "
  firstChar <- liftIO $ getLine
  let n = read firstChar :: Int64
  foldM_ (\_ m -> do 
                      (x,y) <- liftIO $ getRandomNodes p
                      when(x/=y) $ do 
                        resx <- select (from nodemap_index (at x))
                        resy <- select (from nodemap_index (at y))
                        processSearching (head resx) (head resy)
                        return ()                    
                   )
                   ()
                   [1..n]
  liftIO $ print $ "Enter 'q' to quit: "
  qChar <- liftIO $ getChar
  if(qChar == 'q') then return ()
    else performRandomSearch p

getRandomNodes :: Int64 -> IO(Node,Node)
getRandomNodes n = do 
      gen <- newStdGen
      let a:b:_ = take 2 (randomRs (1,n) gen) 
      return (I a, I b)
--      if (a<b) then return (I a,I b) else return (I b,I a)

processSearching :: Nd ->Nd-> Daison()
processSearching nd1  nd2 = do 
{-     liftIO $ print  $ " nd1:  " ++ show nd1 ++ " nd2: " ++ show nd2 -}
    --liftIO $ print  $ "begin"

--    liftIO $ putStr  $ "-OAI-S "
    start11 <- liftIO $ getCurrentTime
    (flag11, _) <- search1 nd1 nd2 1 (Set.empty)
    end11 <- liftIO $ getCurrentTime
--    liftIO $ putStr  $ "-E"

    let timePassed11 = diffUTCTime end11 start11  

--    liftIO $ print  $ "before flag11"
    when (flag11) $ do 
--      liftIO $ print  $ "after flag11"

--      liftIO $ print  $ " Result : " ++ show flag11 ++ " Time Taken for  AILabel1 Optimized Search1 : "++show timePassed11
      liftIO $ (IO.hSetBuffering IO.stdout IO.NoBuffering)
{-  
      liftIO $ putStr  $ "-AI-S"

      start1 <- liftIO $ getCurrentTime
      flag1 <- search nd1 nd2 1
      end1 <- liftIO $ getCurrentTime
      let timePassed1 = diffUTCTime end1 start1   
      liftIO $ putStr  $ "-E"
--      liftIO $ print  $ " Result : " ++ show flag1 ++ " Time Taken for  AILabel Search : "++show (timePassed1 * 1000)
      start <- liftIO $ getCurrentTime
      (flag, count) <- df_search nd1 nd2 0
      end <- liftIO $ getCurrentTime
      let timePassed = diffUTCTime end start  
      liftIO $ print  $ " Result : " ++ show flag ++ " Nodes: " ++ show count ++ " Time Taken for Depth First Search : "++show timePassed
 -}
      liftIO $ putStr  $ "-DFS-S"

      start2 <- liftIO $ getCurrentTime
      (flag2, count2) <- df_search1 [nd1] nd2 0 
      end2 <- liftIO $ getCurrentTime
      liftIO $ putStr  $ "-E"

      let timePassed2 = diffUTCTime end2 start2
--      liftIO $ print  $ " Result : " ++ show flag2  ++ " Nodes: " ++ show count2 ++" Time Taken for Depth First Search : "++show timePassed2
      
{-      liftIO $ print  $ " Nodes: " ++ show count2 ++ " OAI: " ++ show (timePassed11 * 1000)++{-  " AI: "++ show (timePassed1 * 1000) ++ -} " DS: "++show (timePassed2 * 1000) 
      when (flag11 /= flag2 {- || flag11 /= flag1 -}) $ liftIO $ print $ "failed for " ++ show (nd1,nd2)
       if(flag11 && flag1 && flag && flag2 ) then return ()
        else do  -}
{-     when (flag1/=flag2) $ do
      liftIO $ print  $ " Result : " ++ show flag1 ++ " Time Taken for  AILabel Search : "++show timePassed1
 -}
      liftIO $ putStr  $ "-BFS-S"

      start3 <- liftIO $ getCurrentTime
      (flag3, count3) <- bf_search nd1 nd2
      end3 <- liftIO $ getCurrentTime
      let timePassed3 = diffUTCTime end3 start3  
      liftIO $ putStr  $ "-E"

--      liftIO $ print  $ " Result : " ++ show flag2  ++ " Nodes: " ++ show count2 ++" Time Taken for Depth First Search : "++show timePassed2
      
      liftIO $ print  $ "DFNodes :" ++ show count2 ++ " BFNodes: " ++  show count3 ++ {-" AI: " ++ show (timePassed1 * 1000)++ -} " OAI: "++ show (timePassed11 * 1000) ++  " DFS: "++show (timePassed2 * 1000) ++ " BFS: "++show (timePassed3 * 1000) 

      when (flag11 /= flag3 {- || flag11 /= flag1 -}) $ liftIO $ print $ "failed for " ++ show (nd1,nd2) ++ " " ++ show flag11 ++ " " ++ {- show flag2 ++ -}" " ++  show flag3

    return ()


performManualSearch :: String -> AccessMode -> IO()
performManualSearch test_db readwritemode = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
{-     putStrLn ("Enter your choice for (s) for Search (i) for Edge Insertion or (d) for Edge Deletion : ")
    choice <- getChar -}
    putStrLn (" Enter the graph type (I) for Integer, (C) for Character : ")
    gType <- getChar
    putStrLn (" Enter the first node to search : ")
    firstChar <- getLine
    putStrLn (" Enter the second node : ")
    secondChar <- getLine
    db <- openDB test_db  
    runDaison db readwritemode $ do 
      (nd1,nd2) <- if (gType == 'I') then 
        do 
          nd1 <- getNdIndex (I (read firstChar :: Int64))
          nd2 <- getNdIndex (I (read secondChar :: Int64))
          return (nd1,nd2)
        else do
          nd1 <- getNdIndex (C (head firstChar))
          nd2 <- getNdIndex (C (head secondChar :: Char))
          return (nd1,nd2)
      start1 <- liftIO $ getCurrentTime
      flag1 <- search1 nd1 nd2 1 (Set.empty)
      end1 <- liftIO $ getCurrentTime
      let timePassed1 = diffUTCTime end1 start1  
      --liftIO $ print timePassed
      liftIO $ print  $ " Result : " ++ show flag1 ++ " Time Taken for  AILabel Search : "++show timePassed1

{-       start <- liftIO $ getCurrentTime
      (flag, count) <- df_search nd1 nd2 0
      end <- liftIO $ getCurrentTime
      let timePassed = diffUTCTime end start  
      --liftIO $ print timePassed
      liftIO $ print  $ " Result : " ++ show flag ++ " Nodes: " ++ show count ++ " Time Taken for Depth First Search : "++show timePassed
 -}
      start2 <- liftIO $ getCurrentTime
      (flag2) <- df_search1 [nd1] nd2  0
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
    performManualSearch test_db readwritemode

edgesPercent :: Daison()
edgesPercent = do 
  a <- select [ (ind, length edges) | (ind, ( X nd edges )) <- from nodeMapTable everything ]
  let b = foldl (\x y -> x + snd y) 0 a
  liftIO $ print  $ " Total Edges : " ++ show b ++ " Tree edges : " ++ show (length a - 1) ++ " Non Tree Edges :" ++ show (b -(length a -1))
{-   b <- foldM (\acc x -> do


              )
            0
            a -}
  return ()
{-   foldM_ (\(index, visited) x -> do 
    (index', visited') <- countEdge (Map.fromList a) index x visited
    return (index', visited')) (0, []) a 
 -}


step1 :: Nd -> Nd ->Labels-> Daison Bool
step1 nd1 nd2 label2 = do
  label1 <- query firstRow (from graph1Table (at nd1)) 
--  liftIO  $ print  $ " from QUERY " ++ show nd1 ++ " :  " ++ show nd2
  case label1 of 
    (Labels pre1 post1 hp1 dir1) -> do
      case label2 of
        (Labels pre2 post2 hp2 dir2) -> if  (pre1 < post2 && post2 <= post1) then return True
                                             else return False
        _ -> error "error "                
    _ -> error "error again "

--Main search function for AILabel
search :: Nd -> Nd ->Int -> Daison Bool
search nd1 nd2 step = do
  label1 <- query firstRow (from graph1Table (at nd1)) 
--test
--  liftIO  $ print  $ " from SEARCH " ++ show nd1 ++ " :  " ++ show nd2
  case label1 of 
    (Labels  pre1 post1 hp1 dir1) -> do
      if (step ==1) then do
        label2 <- query firstRow (from graph1Table (at nd2)) 
        flag <- step1 nd1 nd2 label2
        if flag then return True
          else do
            x <-  (foldM (\acc x -> if acc
              then return acc
              else search x nd2 1) False (Set.toList hp1)) 
            if x then return True
            else do
              (foldM (\acc x -> if acc 
                then return acc
                else search  x nd2 2) False ( filter (/=nd1) (Set.toList dir1)) )
        else do
          x <- (foldM (\acc x -> if acc
            then return acc
            else search x nd2 1) False (Set.toList hp1)) 
          if x then return True
            else do
              (foldM (\acc x -> if acc
                then return acc
                else search  x nd2 2) False ( filter (/=nd1)(Set.toList dir1)) )

--optimized AILabel search function.
search1 :: Nd -> Nd ->Int -> (Set (Nd,Nd)) -> Daison (Bool, Set (Nd, Nd))
search1 nd1 nd2 step queryset = do
  let newset = (Set.insert (nd1,nd2) queryset)
  label1 <- query firstRow (from graph1Table (at nd1)) 
--  liftIO  $ print  $ " from SEARCH " ++ show nd1 ++ " :  " ++ show nd2
  case label1 of 
    (Labels pre1 post1 hp1 dir1) -> do
      if (step ==1) then do
        label2 <- query firstRow (from graph1Table (at nd2)) 
        flag <- step1 nd1 nd2 label2
        if flag then return (True, newset)
          else do
            (x,s'') <- foldM (\(b,s ) x -> do 
                            if b || (Set.member (x,nd2) s) then return (b,s)
                              else do 
                                (b',s') <- search1 x nd2 1 s                                   
                                return (b'||b ,s'))
                        (False, newset)
                        (Set.toList hp1) 
            if x then return (True,s'')
            else do
                foldM (\(b,s) x -> do
                          if b || (Set.member (x,nd2) s ) then return (b,s)
                            else do 
                              (b',s') <- search1 x nd2 2 s                                  
                              return (b'||b, s' ))
                      (False, s'')
                      ( filter (/=nd1)(Set.toList dir1)) 
        else do
          (x,s'') <- foldM (\(b,s) x -> do 
                                     if b || (Set.member (x, nd2) s) then return (b,s)
                                        else do 
                                          (b',s') <- search1 x nd2 1 s                                 
                                          return (b'||b,s' ))
                                  (False, newset)
                                  (Set.toList hp1) 
          if x then return (True,s'')
            else do
              foldM (\(b,s) x -> do 
                                    if b || (Set.member (x, nd2) s ) then return (b,s)
                                      else do 
                                        (b',s') <- search1 x nd2 2 s                               
                                        return (b'||b,s' ))
                                  (False,s'')
                                  ( filter (/=nd1)(Set.toList dir1)) 

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
  (X n edges) <- query firstRow (from nodeMapTable (at nd1)) 
--  liftIO $ print $ " countMaam : " ++ show (count) ++ " nd1 : " ++ show nd1 ++ " edges :" ++show edges ++" nd2 : " ++ show nd2
{-   liftIO  $ print  $ " from liftio graph " ++ show record ++ " for node : " ++ show nd1 -}
  case edges of
    (first : rest) -> case (List.elem nd2 edges) of
      True -> return (True, count)
      False -> do (b',i') <-foldM (\(b, i) x -> do 
                                     (b',i') <- df_search x nd2 (i+1)
                                     return (b'||b , i'))
                                  (False, (count))
                                  edges
                  return (b',i')
    [] ->  return (False,count)


     -- 1
   -- 2 3 4 5 6 7

   -- 8  

   -- bf_search 1 8
bf_search :: Nd -> Nd -> Daison (Bool, Int64)
bf_search curr search = if curr == search
  then return (True, 0)
  else  do 
    edgesOfCurr <- getEdges curr
    if elem search edgesOfCurr
         then return (True, toEnum (fromJust (List.elemIndex search edgesOfCurr)) + 1)
         else bf_search1 edgesOfCurr search (toEnum (length edgesOfCurr + 1))


-- Edges -> search node -> Value
bf_search1 :: [Nd] -> Nd-> Int64 -> Daison (Bool, Int64)
bf_search1 [] _ count = return (False,count)
bf_search1 bnodes@(nd1:rs) nd2 count  = do 
  (ad, cnt, list) <- foldM (\(alreadyDone, counter, list) v -> 
    if alreadyDone
      then return (alreadyDone, counter, list)
      else do
        edges <- getEdges v
        if elem nd2 edges
          then return (True, counter+ toEnum (fromJust (List.elemIndex nd2 edges) )+ 1, [])
          else return (alreadyDone,counter + toEnum ( length edges), list ++ edges)
      ) (False, count, []) bnodes
  if ad
    then return (True, cnt)
    else bf_search1 list nd2 cnt



getEdges node = do 
  (X n edges) <- query firstRow (from nodeMapTable (at node)) 
  return edges


df_search1 :: [Nd] -> Nd-> Int64 -> Daison (Bool, Int64)
df_search1 (nd1:rs) nd2 count  = do 
  (X n edges) <- query firstRow (from nodeMapTable (at nd1)) 
--  liftIO $ print $ " nd1 : " ++ show nd1 ++ " edges :" ++show edges ++" nd2 : " ++ show nd2
  case edges of
    (first : rest) -> if first == nd2-- case (List.elem nd2 edges) of
      then return (True, count)
      else do
        (b, count') <- df_search1 [first] nd2 (count +1)
        if b
          then return (True, count') 
          else do
            (c, count'') <- 
              if null rest 
                then return (b, count') 
                else df_search1 rest nd2 (count'+1)
            if c
              then return (True, count'') 
              else do 
                if null rs 
                  then return (c, count'') 
                  else df_search1 rs nd2 (count''+1)
    [] -> if null rs
           then return (False, count) 
           else df_search1 rs nd2 (count+1)
       --return (False, count)



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
