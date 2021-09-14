{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module DAILabelModified where
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
import Data.Maybe

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
    directs :: Directs,
    firstChild :: Nd,
    lastChild :: Nd,
    nextSibling :: Nd,
    lastSibling :: Nd
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


graph4 :: Graph Node 
graph4 = Graph 
  [
    (I 1, [I 2, I 3, I 4, I 5, I 6]),
    (I 2, [I 3,I 4, I 5, I 6]),
    (I 3, [I 4, I 5, I 6]),    
    (I 4, [ I 5, I 6]),
    (I 5, [I 6]),
    (I 6, [])
  ]

graph5 :: Graph Node
graph5 = Graph
    [ ( C 'a',  [ C 'b', C 'e']  ),
      ( C 'b', [  C 'd', C 'c' ] ),
      ( C 'c', [  ] ),
      ( C 'd', [ ] ),
      ( C 'e', [ C 'f',  C 'g' ] ),
      ( C 'f', [  ] ),
      ( C 'g', [  ] ),
      ( C 'h', [ C 'i', C 'l' ] ),
      ( C 'i', [ C 'j' , C 'k' ] ),
      ( C 'j', [ ] ),
      ( C 'k', [] ),
      ( C 'l', [ C 'm', C 'n' ] ),
      ( C 'm', [  ] ),
      ( C 'n', [  ] ),
      ( C 'o', [ C 'p' , C 'q' ] ),
      ( C 'p', [  ] ),
      ( C 'q', [  ] )


      
    ]



graph6 :: Graph Node
graph6 = Graph
    [ ( C 'a',  [ C 'b', C 'c']  ),
      ( C 'b', [] ),
      ( C 'c', [] ),
      ( C 'd', [C 'e', C 'f' ] ),
      ( C 'e', [] ),
      ( C 'f', [] )
    ]
graph10 :: Graph Node 
graph10 = Graph 
  [
    (I 1, [I 4]),
    (I 2, [I 3]),
    (I 3, []),    
    (I 4, [I 5]),
    (I 5, [I 6]),
    (I 6, [I 7])
  ]

instance Show Labels where
  show (Labels a b c d e f g h i) = "TP: " ++  show a ++   " Pre: " ++ show b ++
   " Post:  " ++  show c   ++   " Hops: " ++ show d ++ " Directs:  " ++ show e ++
   "FC : " ++ show f ++ " LC :  " ++ show g ++ " NS: " ++ show h ++ " PS : " ++ show i
 
graph1Table :: Table (Labels)
graph1Table = table "graph1"
            `withIndex` graph_index

graph_index :: Index Labels Labels 
graph_index = index graph1Table "node_index" Prelude.id

nodeMapTable :: Table X
nodeMapTable = table "nodemap" `withIndex` nodemap_index


nodemap_index :: Index X Node
nodemap_index = index nodeMapTable "nodemap_index" nd


databaseTest = "mdailabel.db"




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

generateGraph1 :: Int64 -> Double -> IO (Graph Node)
generateGraph1 n p = do
  gen <- getStdGen
  (_, g) <- foldM (\(genV, vs) x -> do
    (gup, vsup) <- restList genV x
    return (gup, vs ++ [(I x, vsup)])) (gen, []) {- list@( -}[1..n]
  return $ Graph g
    where
        restList genV x = do
          (vs, ugen) <- ranValues genV (fromEnum $ (n-x)) 0.0 1.0
          let vs' =
                zipWith (\v i -> if v <= p
                              then Just i
                              else Nothing
                  )
                  vs
                  [(x+1)..(n)]
          return (ugen, I <$> catMaybes vs')
--          map I $ sort $ nub (take  (floor (p * fromIntegral (n-x))) $ randomRs (x+1,n) (mkStdGen 3) :: [Int64]  )

ranValues gen n a b = do
  let values = take n (randomRs (a,b) gen )
  putStrLn $ show (values)
  let (_,newGen) = random gen :: (Double, StdGen)
  return (values, newGen)



main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  putStrLn ("Enter the number of nodes : ")
  inp_1 <- getLine
  putStrLn (" Enter the density : ")
  inp_2 <- getLine
  let n = (read inp_1 :: Int64)
  let d = (read inp_2 :: Double)
  let Graph g1 = generateGraph n d
  print $ show g1
  db <- openDB databaseTest
  (a,b)  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable nodeMapTable
    let Graph g = graph5
    let graphmap1 =  Map.fromList g
    dynamicProcess graphmap1 
    a <- select [ x | x <- from graph1Table everything ]
    b <- select [ x | x <- from nodeMapTable everything ]
    return (a,b)
  putStrLn "FROM MAIN"
  mapM_ (\y -> putStrLn (show y) ) a
  mapM_ (\y -> putStrLn (show y) ) b
  closeDB db
  makeDynamicOperation databaseTest ReadWriteMode

main1  :: Int64 -> Double -> IO ()
main1 n d= do
  removeFile databaseTest
  IO.hSetBuffering IO.stdin IO.NoBuffering
  Graph g1 <- generateGraph1 n d
  print $ show g1
  db <- openDB databaseTest
  (a,b,c)  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable nodeMapTable
    let Graph g = graph2
    let graphmap1 | n == 0 = Map.fromList g
                  | otherwise = Map.fromList g1
    start <- liftIO $ getCurrentTime 
    dynamicProcess graphmap1 
    end <- liftIO $ getCurrentTime
    let timePassed = diffUTCTime end start  
    a <- select [ x | x <- from graph1Table everything ]
    b <- select [ x | x <- from nodeMapTable everything ]
    return (a,b,timePassed)
  putStrLn "FROM dailabel modified"
  mapM_ (\y -> putStrLn (show y) ) a
  mapM_ (\y -> putStrLn (show y) ) b
  print $ "Time for  DaiLabel modified for n : " ++ show n ++ " d " ++ show d ++ " : "++ show c
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
    putStrLn ("Enter your choice for (s) for Search (i) for Edge Insertion or (d) for Edge Deletion or (r) for Reset : ")
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
        'r' ->  do
          dropTable graph1Table
          dropTable nodeMapTable
          liftIO main
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
  store nodeMapTable (Just 0) (X (S "root" ) [])
  store graph1Table (Just 0) (Labels (-1) 0 max_bound Set.empty Set.empty (-100) (-100) (-100) (-100) )    
  unprocessedGraph <-processNodes graphmap graphmap firstnode (S "root")
  foldM_ (\acc x -> processNodes acc acc x (S "root") ) unprocessedGraph (Map.keys unprocessedGraph)
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
                Just []   -> return gm
                Just rest    ->do
                  foldM (\acc x -> processNodes graph acc x node) gm rest

updateLabel :: Nd ->Labels->  Daison ()
updateLabel nd res = do
  liftIO $ print $ " update label nd :" ++ show nd
  prev <- prevOf (PreLabel nd) res >>= \(x,xlabels) -> getIntervalLabel x xlabels
  case res of
    Labels tp pr ps hp dir fc lc ns ls -> do
      post <-  fetchLabels (PostLabel tp) >>= \tplabel -> getIntervalLabel (PostLabel tp) tplabel 
      let pr = average prev post
      let ps = average pr post
      update_ graph1Table (return (nd, res{preL = pr, postL = ps}))
      (next, nextRecord) <- nextNode nd res{preL = pr, postL = ps}
      when (next > 0) $ updateLabel next nextRecord
  return()
 
handleInsert :: Nd -> Nd -> Daison ()
handleInsert nd1 nd2 = do
  res1 <- select (from graph1Table (at nd1))
  res2 <- select (from graph1Table (at nd2))
  case (res1,res2) of

    ([record1@(Labels tp1 pr1 ps1 hp1 _ fc1 lc1 _ _ )],[record2@(Labels tp2 pr2 ps2 hp2 dir2 fc2 _ ns2 ls2 )])   -- Case 1
          | tp2 == 0 -> do                                                                       -- Case 1.1
              if Set.null hp1
                then updateDirectInAncestors nd1 record1 (Set.union dir2)                        -- Case 1.1.1
                else updateDirectInAncestors nd1 record1 (Set.insert nd2)                        -- Case 1.1.2
              (updatedRecord1 , updatedRecord2) <- verifyAndInsertChild nd1 record1 nd2 record2
              update_ graph1Table (return (nd1, updatedRecord1))
              update_ graph1Table (return (nd2, updatedRecord2))
              when(ls2>0) $ do 
                  record4 <- query firstRow (from graph1Table (at ls2))
                  update_ graph1Table (return (ls2, record4{nextSibling = ns2}))
                  return()
              when (fc2 >0) $ do
                fc2Record <- query firstRow (from graph1Table (at fc2))
                updateLabel fc2 fc2Record
              {-      - updated the pre and post labels of nd2 and its children.
                          All the labels moved right after PreLabel nd1.
                       - updated prevSibling for the firstChild of nd1
                       - updated firstChild for nd1 -}
          | otherwise -> do  
 -- Case 1.2
              if (not (Set.null hp1) || tp1 <= 0 || tp1 == nd1)
                then addHop nd1 record1 nd2                                                      -- Case 1.2.1
                else do 
                        record <- query firstRow (from graph1Table (at tp1))                     -- Case 1.2.2  
                        updateDirectInAncestors tp1 record (Set.insert nd1)
                        addHop nd1 record1 nd2 
              return ()

    ([record1@(Labels tp1 pr1 ps1 _ _ fc1 lc1 _ _ )],[]                                          ) ->  -- Case 2
          do (updatedRecord1, updatedRecord2) <- verifyAndInsertChild nd1 record1 nd2 (Labels nd1 0 0 Set.empty Set.empty (-1) (-1) (-1) (-1)  )
             store graph1Table (Just nd1) updatedRecord1
             store graph1Table (Just nd2) updatedRecord2
             return ()

    ([]                                       ,[record2@(Labels tp2 pre2 pos2 hp2 dir2 _ _ ns2 ps2 )])     -- Case 3
          | tp2 > 0   -> do                                                                      -- Case 3.1
      {- inserted two labels after (PreLabel root), root == 0 -}
              rootRecord <- query firstRow (from graph1Table (at 0))  
              (urootRecord, uRecord1) <- verifyAndInsertChild 0 rootRecord nd1 (Labels 0 0 0 Set.empty Set.empty (-1) (-1) (-1) (-1))
              store graph1Table (Just 0) urootRecord
              store graph1Table (Just nd1) uRecord1{ hops= (Set.singleton nd2)}
              return ()
          | otherwise -> do                                                                      -- Case 3.2
              (pre,post)  <- do
                a <- prevOf (PreLabel nd2) record2 >>= \(x,xlabels) -> getIntervalLabel x xlabels
                b <-nextOf (PostLabel nd2) record2 >>= \(x,xlabels) -> getIntervalLabel x xlabels
                if (pre2-a) <= 2  then do  
                    prevOf (PreLabel nd2) record2 >>= \(x,y)  -> reLabelMain x y
                    newRes2 <- query firstRow (from graph1Table (at nd2))
                    na <- prevOf (PreLabel nd2) newRes2 >>= \(x,xlabels) -> getIntervalLabel x xlabels
                    nb <-nextOf (PostLabel nd2) newRes2>>= \(x,xlabels) -> getIntervalLabel x xlabels
                    case newRes2 of 
                      newRecord2@(Labels _ nprend2 nposnd2 _ _ _  _ _ _) -> do 
                        store graph1Table (Just nd2) newRecord2{tree_parent=nd1, nextSibling= (-1), lastSibling = (-1)}
                        let npre = average nprend2 na 
                            npost = average nposnd2 nb 
                        return(npre,npost)          
                else if (b-pos2 <= 2) then do 
                    reLabelMain (PostLabel nd2) record2
                  --need to recalculate the prev and next values because the labels might have updated.  
                    newRes2 <- query firstRow (from graph1Table (at nd2))
                    na <- prevOf (PreLabel nd2) newRes2 >>= \(x,xlabels) -> getIntervalLabel x xlabels
                    nb <-nextOf (PostLabel nd2) newRes2 >>= \(x,xlabels) -> getIntervalLabel x xlabels
                 --need to refetch the record as the labels are updated now
                    case newRes2 of 
                      newRecord2@(Labels _ nprend2 nposnd2 _ _ _  _ _ _) -> do 
                        store graph1Table (Just nd2) newRecord2{tree_parent=nd1, nextSibling= (-1), lastSibling = (-1)}
                        let npre = average nprend2 na 
                            npost = average nposnd2 nb 
                        return(npre,npost)
                  else do 
                    let pre = average pre2 a 
                    let post = average pos2 b 
                    store graph1Table (Just nd2) record2{tree_parent=nd1, nextSibling= (-1), lastSibling = (-1)}
                    return (pre,post)
                --updated siblings and children of root
              when (ns2 >0 ) $ do --updating siblings 
                  res3 <- query firstRow (from graph1Table (at ns2))
                  case res3 of 
                    record3@(Labels _ _ _ _ _ _  _ _ ps) -> store graph1Table (Just ns2) record3{lastSibling = nd1}
                  return ()
              when (ps2 >0 ) $ do
                  res4 <- query firstRow (from graph1Table (at ps2))
                  case res4 of 
                    record4@(Labels _ _ _ _ _ _  _ ns _) -> store graph1Table (Just ps2) record4{nextSibling = nd1}
                  return ()              
              let record1 = Labels 0 pre post Set.empty Set.empty nd2 nd2 ns2 ps2
              if Set.null hp2
                then updateDirectInAncestors nd1 record1 (Set.union dir2)
                else updateDirectInAncestors nd1 record1 (Set.insert nd2)

    ([]                                       ,[]                                          ) ->  -- Case 4
          do (pre1,pre2,post1,post2,prevSib1) <- insert2IsolatedNodes nd1 nd2 
             store graph1Table (Just nd1) (Labels 0   pre1 post1 Set.empty Set.empty nd2  nd2  (-1) prevSib1)
             store graph1Table (Just nd2) (Labels nd1 pre2 post2 Set.empty Set.empty (-1) (-1) (-1) (-1))
             return ()
  par <-  query firstRow (from nodeMapTable (at nd1))
  case par of
    (X pnd edges) ->  store nodeMapTable (Just nd1) (X pnd (nd2:edges))
  return()
--  store nodeMapTable (Just parent) (X pnd (List.nub (nd:edges)))
  where     
    insert2IsolatedNodes nd1 nd2 = do 
      res <- query firstRow (from graph1Table (at 0))
      case res of 
        record@(Labels _ _ ps _ _ _  rlc _ _) -> if rlc < 0 then 
          do 
            let pre1 = average 0 ps
            let post1 = average pre1 ps
            let pre2 = average pre1 post1
            let post2 = average pre2 post1
            store graph1Table (Just 0) record{firstChild = nd1, lastChild =nd1}
            return (pre1,pre2,post1,post2, (-1))
          else do
            res1 <- query firstRow (from graph1Table (at rlc))
            case res1 of 
              record1@(Labels  _ pr1 ps1 _ _ _ _ _ _ )-> do 
                (pre1,post1) <- if ((ps - ps1) < 12 ) --12 is the minimum number to ensure the order of numbers
                  then do
                    reLabelMain (PostLabel rlc) record1
                    updatedRes <- query firstRow (from graph1Table (at rlc))
                    case updatedRes of
                      updatedRecord@(Labels _ upr ups _ _ _ _ _ _ ) -> do
                        let pre1 = average ups ps
                        let post1=  average pre1 ps
                        store graph1Table (Just rlc) updatedRecord{ nextSibling = nd1} --to update the rlc record with updated pre and post values
                        return (pre1,post1)
                  else do 
                    let pre1 = average ps1 ps 
                    let post1 = average pre1 ps
                    store graph1Table (Just rlc) record1{ nextSibling = nd1} 
                    return (pre1, post1)
                let pre2 = average pre1 post1 
                let post2 = average pre2 post1 
                store graph1Table (Just 0) record{ lastChild =nd1}
                return (pre1,pre2,post1,post2,rlc )


average x y = x + div (y-x) 2

verifyAndInsertChild nd1 record1@(Labels tp1 pr1 ps1 _ _ fc1 lc1 _ _) nd2 record2@(Labels tp2 _ _ _ _ _ _ _ _ ) = do 
      if(lc1 < 0) then do 
        if (ps1-pr1 <=2) then do 
          reLabelMain (PreLabel nd1) record1
          updatedRes1 <- query firstRow (from graph1Table (at nd1))
          case updatedRes1 of
            updatedRecord1@(Labels _ upr ups _ _ _ _ _ _ ) -> do
              let pre = average upr ups
                  post=  average pre ups
              return (updatedRecord1{firstChild = nd2, lastChild = nd2}, record2{tree_parent=nd1, preL = pre, postL = post}  )
          else do 
            let pre = average pr1 ps1 
                post = average pre ps1
            return (record1{firstChild = nd2, lastChild = nd2}, record2{tree_parent=nd1, preL = pre, postL = post}  )  
        else do 
          lc1Record <- query firstRow (from graph1Table (at lc1))
          case lc1Record of
            (Labels _ _ lps _ _ _ _ _ _) -> do 
              if(ps1-lps <=2) then do
                reLabelMain (PostLabel lc1) lc1Record
                updatedRes1 <- query firstRow (from graph1Table (at nd1))
                case updatedRes1 of
                  updatedRecord1@(Labels _ upr ups _ _ _ _ _ _ ) -> do
                    ulc1Record <- query firstRow (from graph1Table (at lc1))
                    case ulc1Record of
                      (Labels _ _ ulps _ _ _ _ _ _ ) -> do
                        let pre = average ulps ups
                            post=  average pre ups
                        update_ graph1Table (return (lc1, ulc1Record{nextSibling = nd2}))
                        return (updatedRecord1{ lastChild = nd2}, record2{tree_parent=nd1, preL = pre, postL = post,lastSibling = lc1 }  )
              else do
                let pre = average lps ps1
                    post= average pre ps1
                update_ graph1Table (return (lc1, lc1Record{nextSibling = nd2}))
                return (record1{ lastChild = nd2}, record2{tree_parent=nd1, preL = pre, postL = post,lastSibling = lc1 }  )


handleDelete :: Nd -> Nd -> Daison ()
handleDelete nd1 nd2 = do
  res1 <- select (from graph1Table (at nd1))
  res2 <- select (from graph1Table (at nd2))
  if( null res1|| null res2 ) then 
    liftIO $ print $ " node doesn't exist, try again "
    else do 
      istreeEdge <- isTreeEdge nd1 (head res1) nd2
      deleteEdge nd1 nd2
      case istreeEdge of
        True -> do
          deleteDirectsandAncestors nd1 (head res1) nd2
          case res1 of 
            [record1@(Labels tp pr ps hp dir fc lc ns ls)] -> do
              removeTreeParent nd1 record1{directs = (Set.delete nd2 dir)} nd2 (head res2)
              return()
        False -> do
          flag <- isTheOnlyNonTreeEdge nd1 (head res1) nd2
          when flag $ do
            case res1 of
              [(Labels tp pr ps hp dir fc lc ns ls)] -> do
                record <- query firstRow (from graph1Table (at tp))
                deleteDirectsandAncestors tp record nd1
                when ( not $ Set.null dir ) $ do
                  updateDirectInAncestors tp record (Set.union dir)
              _ -> error $ "invalid from handle delete : " ++ show nd1 ++ show nd2
          deleteHopsFrom nd1 (head res1) nd2 --verify 
          return ()

isTreeEdge :: Nd -> Labels-> Nd -> Daison Bool
isTreeEdge nd1 record nd2 = do
  case record of 
    (Labels tp pr ps hp dir fc lc ns ls) -> case (List.elem nd2 hp) of
      True -> return False
      False -> do 
        nodemap_record <- select [edgs | ( X n edgs) <- from nodeMapTable (at nd1)  ]
        let nd_edges = head nodemap_record
        if (List.elem nd2 nd_edges) then 
          return True
        else 
          return False
    _ -> error $ " from istreeedge : " ++ show nd1 ++ show nd2
 

isTheOnlyNonTreeEdge :: Nd -> Labels-> Nd -> Daison Bool
isTheOnlyNonTreeEdge nd1 record1 nd2 = do
    case record1 of
      Labels trp pr ps hp dir fc lc ns ls ->  do 
        if (Set.member nd2 hp) && (Set.size hp ==1) then 
          return True
        else if (Set.member nd2 hp) && (Set.size hp > 1) then
          return False
        else 
          error $ "error from istheonlynontreeedge : nd1 , nd2 : " ++ show nd1 ++ show nd2
      _ -> error " couldnt find record from isonlynontreeedge "

deleteDirectsandAncestors :: Nd ->  Labels-> Nd -> Daison()
deleteDirectsandAncestors nd1 record1 nd2 = do
  case record1 of
    Labels tp pr ps hp dir fc lc ns ls -> do
      update_ graph1Table (return (nd1, Labels tp pr ps hp (Set.delete nd2 dir) fc lc ns ls ) )
      when (tp/=0) $ do
        record <- query firstRow (from graph1Table (at tp))
        deleteDirectsandAncestors tp record nd2
    _ -> error $ "invalid from deletedirectss and ancestors " ++ show nd1 ++ show nd2

deleteHopsFrom :: Nd -> Labels -> Nd -> Daison ()
deleteHopsFrom nd1 record1 nd2 = do
  case record1 of
    Labels tp pr ps hp dir fc lc ns ls -> do
      update_ graph1Table (return (nd1, Labels tp pr ps (Set.delete nd2 hp) dir fc lc ns ls) )
    _ -> error "invalid"  


addHop :: Nd -> Labels -> Nd -> Daison ()
addHop nd1 (Labels tp pr ps hp dir fc lc ns ls) nd2 = when (nd1 > 0) $ do
  store graph1Table (Just nd1) (Labels tp pr ps (Set.insert nd2 hp) dir fc lc ns ls)
  return ()

deleteEdge :: Nd -> Nd -> Daison ()
deleteEdge nd1 nd2 = do 
  record <- select [(n, edgs) | (X n edgs) <- from nodeMapTable (at nd1)  ] 
  case record of
        [(n , edgs)] -> update_ nodeMapTable (return (nd1, X n (List.delete nd2 edgs)) )
        _            -> error $ show nd1 ++ " -  " ++ show nd2 ++ " is not an edge"


removeTreeParent :: Nd -> Labels-> Nd -> Labels-> Daison ()
removeTreeParent nd1 record1@(Labels tp1 pr1 ps1 hp1 dir1 fc1 lc1 ns1 ls1) nd2 record2@(Labels tp2 pr2 ps2 hp2 dir2 fc2 lc2 ns2 ls2) = do 
  when (ls2 >0) $ do
    ls2Record <- query firstRow (from graph1Table (at ls2))
    store graph1Table (Just ls2) ls2Record{ nextSibling = ns2}
    return ()
  when (ns2 >0) $ do
    ns2Record <- query firstRow (from graph1Table (at ns2))
    store graph1Table (Just ns2) ns2Record{ lastSibling = ls2}
    return ()
  store graph1Table (Just nd1) record1{ firstChild = if fc1 == nd2 then ns2 else fc1,
                                        lastChild = if lc1 == nd2 then ns2 else lc1
                                      }
  rootRecord <- query firstRow (from graph1Table (at 0))
  (updatedRootRecord, updatedRecord2) <- verifyAndInsertChild 0 rootRecord nd2 record2{tree_parent = 0, nextSibling = -1}
  store graph1Table (Just 0) updatedRootRecord
  store graph1Table (Just nd2) updatedRecord2
  when (fc2 >0) $ do
    fc2Record <- query firstRow (from graph1Table (at fc2))
    updateLabel fc2 fc2Record
  return()
    

reLabelMain :: PrePostRef -> Labels -> Daison ()
reLabelMain v record  =  do 
  let d = 3
  let count = 2
  (newd, newcount, begin,beginRecord, newv, newvRecord,  end, endRecord) <- mainLoop d count v record v record v record 
  reLabelRange begin beginRecord newv newvRecord end newd newcount  
  return ()

mainLoop :: Int64 -> Int64-> PrePostRef->Labels-> PrePostRef-> Labels-> PrePostRef -> Labels-> Daison (Nd, Int64, PrePostRef, Labels, PrePostRef,Labels,  PrePostRef, Labels)
mainLoop d count begin beginRecord v vRecord end endRecord = do
  if (d <  max_bound) then 
    do 
      (begin1, begin1Record, v1, v1Record, count1, d1)  <- goPrev (begin) beginRecord v vRecord count d
      (end1, end1Record, v2, v2Record, count2, d2)  <- goNext (end) endRecord v1 v1Record count1  d1
      if (count2 * count2 < fromIntegral (d2 +1)  ) then 
        return (d2, count2, begin1, begin1Record, v2, v2Record, end1, end1Record)
      else
        do 
          let d3 = d2 Bits..|. (Bits.shiftL d2 1)
          mainLoop d3 count2  begin1 begin1Record v2 v2Record end1 end1Record
  else 
    return (d, count, begin, beginRecord, v, vRecord, end, endRecord)

goPrev :: PrePostRef -> Labels-> PrePostRef -> Labels->Int64 ->  Int64 -> Daison (PrePostRef, Labels, PrePostRef, Labels,Int64,  Int64)
goPrev begin beginRecord v vRecord count  d = do 
  vlabel <- getIntervalLabel v vRecord
  (newbegin, newbeginRecord) <- prevOf begin beginRecord
  newbeginLabel <- getIntervalLabel newbegin newbeginRecord
  if (newbeginLabel > (vlabel Bits..&. (Bits.complement d))) then do
    goPrev newbegin newbeginRecord v vRecord (count +1) d
  else return (begin, beginRecord,v,vRecord, count, d)


goNext :: PrePostRef -> Labels->PrePostRef-> Labels -> Int64 ->  Int64 -> Daison (PrePostRef, Labels, PrePostRef, Labels,Int64,  Int64)
goNext end endRecord v vRecord count d = do 
  vlabel <- getIntervalLabel v vRecord
  (newend, newendRecord) <- nextOf end endRecord
  newendLabel <- getIntervalLabel newend newendRecord
  if (newendLabel < (vlabel Bits..|. d)) then 
    goNext newend newendRecord v vRecord (count +1)  d
  else return (end, endRecord,v, vRecord, count, d)

reLabelRange :: PrePostRef -> Labels-> PrePostRef ->Labels-> PrePostRef -> Int64 ->Int64 -> Daison()
reLabelRange begin beginRecord v vRecord end d count = do 
  let n = div d count
  vlabel <- getIntervalLabel v vRecord
  let newv = vlabel  Bits..&. (Bits.complement d )
  reLabelNodes begin beginRecord newv end n 1
  return ()

reLabelNodes begin beginRecord v end n i = do 
    let newlabel = v + i * n
    case begin of 
      (PreLabel nd) -> do 
        store graph1Table (Just nd) beginRecord{ preL = newlabel}
        return ()
      (PostLabel nd) -> do 
        store graph1Table (Just nd) beginRecord{ postL = newlabel}
        return ()
    if begin == end then return()
      else do
        (newbegin,newbeginRecord) <- nextOf (begin) beginRecord
        reLabelNodes newbegin newbeginRecord v end n (i +1)


fetchLabels (PreLabel nd) = query firstRow (from graph1Table (at nd))
fetchLabels (PostLabel nd) = query firstRow (from graph1Table (at nd))

getIntervalLabel :: PrePostRef -> Labels-> Daison Nd
getIntervalLabel ppr (Labels trp pr ps hp dir fc lc ns ls ) = 
  case ppr of
    (PreLabel nd) -> return pr
    (PostLabel nd) -> return ps


{- nextNode :: Nd -> Daison Nd
nextNode nd = do
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of 
   [(Labels trp pr ps hp dir fc lc ns ls)] -> if fc >0 then return fc
      else if ns > 0 then return ns
      else nextPre trp

 -}

prevOf :: PrePostRef -> Labels-> Daison (PrePostRef, Labels) 
prevOf ppr record@(Labels trp pr ps hp dir fc lc ns ls ) = do 
  case ppr of
    (PreLabel nd) -> if ls >0 then do 
      lsLabels <- fetchLabels (PostLabel ls)  
      return  ((PostLabel ls)  ,lsLabels)
      else do
        trpLabels <- fetchLabels (PreLabel trp)
        return ((PreLabel trp), trpLabels)
    (PostLabel nd) ->  if lc >0 then do
      lcLabels <- fetchLabels (PostLabel lc)
      return ((PostLabel lc), lcLabels)  
      else return ((PreLabel nd), record)

nextOf :: PrePostRef ->Labels -> Daison (PrePostRef, Labels) 
nextOf ppr record@(Labels trp pr ps hp dir fc lc ns ls ) = do 
  case ppr of
    (PreLabel nd) -> if fc >0 then do
      fcLabels <- fetchLabels (PreLabel fc)
      return ((PreLabel fc) , fcLabels)
      else return ((PostLabel nd), record)
    (PostLabel nd) ->  if ns >0 then do
      nsLabels <- fetchLabels (PreLabel ns)
      return ((PreLabel ns)  ,  nsLabels )
      else do
        trpLabels <- fetchLabels (PostLabel trp)
        return ((PostLabel trp) , trpLabels)

-- for updateLabels we need to find the next 'pre' nodes because 'post' nodes are already updated.

nextNode :: Nd -> Labels-> Daison (Nd,Labels)
nextNode nd record = do 
  (a, arecord) <- nextOf (PreLabel nd) record
  case a of 
    (PreLabel n) -> return (n, arecord) 
    (PostLabel n) -> nextPre (PostLabel n) arecord

nextPre :: PrePostRef -> Labels -> Daison (Nd, Labels)
nextPre (PreLabel n) record = return (n, record) 
nextPre (PostLabel n) record = if n <=0 then return (0, record) 
  else do
    (b,bRecord) <- nextOf (PostLabel n) record
    nextPre b bRecord


updateDirectInAncestors :: Nd -> Labels -> (Directs -> Directs) -> Daison ()
updateDirectInAncestors nd (Labels tp pr ps hp dir fc lc ns ls) f = do
  store graph1Table (Just nd) (Labels tp pr ps hp (f dir) fc lc ns ls)
  when (tp /= 0) $ do
    record <- query firstRow (from graph1Table (at tp))
    updateDirectInAncestors tp record f

queryM :: Nd -> Labels-> Nd -> Labels -> Daison Bool
queryM nd1 label1 nd2 label2 = do
  case label1 of 
    (Labels trp1 pre1 post1 hp1 dir1 fc1 lc1 ns1 ls1) -> do
      case label2 of
        (Labels trp2 pre2 post2 hp2 dir2 fc2 lc2 ns2 ls2) -> if  (pre1 < post2 && post2 <= post1) then return True
                                             else return False
        _ -> error "error "                
    _ -> error "error again "

search :: Nd -> Labels -> Nd -> Labels -> Daison Bool
search nd1 label1 nd2 label2 = do
{-   label1 <-  query firstRow (from graph1Table (at nd1))
  label2 <-  query firstRow (from graph1Table (at nd2)) -}
  case label1 of 
    (Labels trp1 pre1 post1 hp1 dir1 fc1 lc1 ns1 ls1) -> do
      flag <- queryM nd1 label1 nd2 label2
      if flag then return True
      else do
        x <- or <$> (mapM (\x ->fetchLabels (PreLabel x) >>= \xlabel-> search x xlabel nd2 label2) (Set.toList hp1)) 
        if not x 
          then or <$> (mapM (\x -> fetchLabels (PreLabel x) >>= \xlabel -> queryM x xlabel nd2 label2) (Set.toList dir1)) 
        else return x

dfsearch :: Nd -> Nd -> Daison Bool
dfsearch nd1 nd2 = do 
  record <- select [edgs | (X n edgs) <- from nodeMapTable (at nd1)  ] 
  case (head record) of
    lis@(first : rest) -> case (List.elem nd2 lis) of
      True -> return True
      False -> or <$> (mapM (\x -> dfsearch x nd2) rest)
 
getEdges :: Nd -> Daison [Nd]
getEdges nd = do 
  record <- select [ edgs| ( X n edgs) <- from nodeMapTable (at nd)  ] 
  return . head $ record



