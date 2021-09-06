
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


main1 :: Int64 -> Double -> IO ()
main1 n d = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  let Graph g1 = generateGraph n d
  let Graph g = graph2
  let graphmap1 | n == 0 = Map.fromList g
                  | otherwise = Map.fromList g1
  print $ show graphmap1
  removeFile databaseTest

  db <- openDB databaseTest
  (x,y) <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable nodeMapTable

    process graphmap1

    x<-select (from graph1Table everything)
    y<-select (from nodeMapTable everything)
    return (x,y)

  putStrLn "-------------------"
  mapM_ print x
  mapM_ print y

  closeDB db


    
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
