{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

--module DynDAILabel where

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
import qualified Data.Bits as Bits
import Data.Int

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

max = maxBound :: Nd

type GraphMap a = Map a [a]
type Special = Bool
type Record = (Nd, Labels)
type Edges = [Nd]

data PrePostRef = PreLabel Nd | PostLabel Nd deriving Eq


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



instance Show Labels where
  show (Labels a b c d e f g h i) = "TP: " ++  show a ++  {- " Pre: " ++ show b ++
   " Post:  " ++ show c  ++ -}   " Hops: " ++ show d ++ " Directs:  " ++ show e ++
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

counters :: Table (String, Nd)
counters = table "counter" 
             `withIndex` counter_index

counter_index :: Index (String, Nd) String
counter_index = index counters "counter_index" fst

databaseTest = "test1.db"




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
  db <- openDB databaseTest
  (a,b)  <- runDaison db ReadWriteMode $ do
    tryCreateTable graph1Table
    tryCreateTable counters
    tryCreateTable nodeMapTable
    insert counters (return ( "l_max", Main.max ))
    let Graph g = graph2
    let graphmap1 =  Map.fromList g
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
    putStrLn ("Enter your choice for (s) for Search (i) for Edge Insertion or (d) for Edge Deletion : ")
    choice <- getChar
    putStrLn (" Enter the first node : ")
    firstChar <- getChar
    putStrLn (" Enter the second node : ")
    secondChar <- getChar
--    let firstChar = 'k'
--    let secondChar = 'l'
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
    mapM_ (\y -> putStrLn (show y) ) a
    mapM_ (\y -> putStrLn (show y) ) b
    closeDB db
    makeDynamicOperation test_db readwritemode

process :: GraphMap Node -> Daison ()
process graphmap = do
  let firstnode = fst $ Map.elemAt 0 graphmap
  store nodeMapTable (Just 0) (X (S "root" ) [])
  store graph1Table (Just 0) (Labels 0 0 Main.max Set.empty Set.empty (-100) (-100) (-100) (-100) )    
  let values = snd $ Map.elemAt 0 graphmap
  processNodes graphmap firstnode (S "root" ) 


processNodes :: GraphMap Node -> Node -> Node -> Daison()
processNodes graph node parent_node = do
  nd <- getNdIndex node
  parent <- getNdIndex parent_node
  par <-  query firstRow (from nodeMapTable (at parent))
  case par of
    (X pnd edges) -> case (List.elem nd edges ) of
      True -> return ()
      False -> handleInsert  parent nd   
  let adjacent = Map.lookup node graph
  case adjacent of
      Nothing      -> return () 
      Just []   -> return ()
      Just rest    ->do
        mapM_ (\x -> processNodes graph x node ) rest

updateLabel :: Labels ->Nd ->Labels -> Nd -> Daison()
updateLabel record@(Labels ptrp ppr pps _ _ pfc flc pns pls ) nd1 record1@(Labels _ pr1 ps1 _ _ fc1  rlc1 ns1 _) nd2 
  | nd2 < 0 = do  
    let pre1 = average ppr pps
    let post1 = average pre1 pps
    store graph1Table (Just nd1) record1{preL = pre1, postL =post1}
--    liftIO $ print $ " nd :" ++ show nd1 ++ " from if : " ++ show record1

    if (ns1>0) then updateLabel record nd1 record1{preL = pre1, postL =post1} ns1 else return()
    if (fc1 > 0 ) then 
      do 
        res3 <- query firstRow (from graph1Table (at fc1)) 
        updateLabel record1{preL = pre1, postL =post1} fc1 res3 (-1) 
      else return()

    return ()
  | otherwise = do
    res2 <- query firstRow (from graph1Table (at nd2))
    case res2 of 
      record2@(Labels _ pr2 ps2 _ _ fc2  rlc2 ns2 _) -> do 
            let pre2 = average pps ps1
            let post2 = average pre2 pps
            store graph1Table (Just nd2) record2{preL = pre2, postL =post2}
--            liftIO $ print $ " nd :" ++ show nd2 ++ " edges : " ++ show record2
            if (ns2 > 0) then updateLabel record nd2 record2{preL = pre2, postL =post2} ns2 else return()
            if (fc2 > 0) then 
              do 
                res3 <- query firstRow (from graph1Table (at fc2)) 
                updateLabel record2{preL = pre2, postL =post2} fc2 res3 (-1) 
              else return()

    return()
  where 
    average x y = (div x 2) + (div y 2)

handleInsert :: Nd -> Nd -> Daison ()
handleInsert nd1 nd2 = do
  res1 <- select (from graph1Table (at nd1))
  res2 <- select (from graph1Table (at nd2))
  
  case (res1,res2) of

    ([record1@(Labels tp1 _ _ hp1 _ fc1 lc1 _ _ )],[record2@(Labels tp2 _ _ hp2 dir2 _ _ ns2 ls2 )])   -- Case 1
          | tp2 == 0 -> do                                                                       -- Case 1.1
              if Set.null hp1
                then updateDirectInAncestors nd1 record1 (Set.union dir2)                        -- Case 1.1.1
                else updateDirectInAncestors nd1 record1 (Set.insert nd2)                        -- Case 1.1.2
              update_ graph1Table (return (nd2, record2{tree_parent=nd1{- , nextSibling=fc1 -}}))
              --liftIO $ print $ " nd :" ++ show nd1 ++ " edges : " ++ show record1
              --liftIO $ print $ " nd :" ++ show nd2 ++ " edges : " ++ show record2
                                   

              if (lc1 <0) then 
                update_ graph1Table (return (nd1, record1{firstChild = nd2, lastChild= nd2}))>>
                updateLabel record1 nd2 record2 (-1)
                else 
                  do update_ graph1Table (return (nd1, record1{lastChild= nd2}))
                     record3 <- query firstRow (from graph1Table (at lc1))
                     update_ graph1Table (return (lc1, record3{nextSibling = nd2}))
                     update_ graph1Table (return (nd2, record2{lastSibling = lc1}))
                     updateLabel record1 lc1 record3 nd2
              if(ls2>0) then 
                do 
                  record4 <- query firstRow (from graph1Table (at ls2))
                  update_ graph1Table (return (ls2, record4{nextSibling = ns2}))
                  --liftIO $ print $ " nd :" ++ show ls2 ++ " edges baabi: " ++ show record4 
                  return()
                else return ()

              {- TODO: - update the pre and post labels of nd2 and its children.
                          All the labels must be moved right after PreLabel nd1.
                       - update prevSibling for the firstChild of nd1
                       - update firstChild for nd1 -}
          | otherwise -> do                                                                      -- Case 1.2
              if not (Set.null hp1)
                then addHop nd1 record1 nd2                                                      -- Case 1.2.1
                else do record <- query firstRow (from graph1Table (at tp1))                     -- Case 1.2.2
                        updateDirectInAncestors tp1 record (Set.insert nd1)
                        addHop nd1 record1 nd2
              liftIO $ print $ " nd1 :" ++ show nd1 ++ " " ++ show record1
              liftIO $ print $ " nd2 :" ++ show nd2 ++ " " ++ show record2 
              return ()

    ([record1@(Labels tp1 pr1 ps1 _ _ fc1 lc1 _ _ )],[]                                          ) ->  -- Case 2
          do (pre,post)  <- do
                a <- prevOf (PostLabel nd1) >>= \x -> getLabel x 
                let pre = average a ps1
                let post = average pre ps1
                return (pre,post) {- TODO: insert two labels after (PreLabel nd1) -}
             (new_fc1, new_lc1, ns2, ls2) <- if lc1 < 0 
               then return (nd2,nd2,(-1),(-1)) 
               else do res3 <- query firstRow (from graph1Table (at lc1))
                       case res3 of 
                        record3@(Labels _ _ _ _ _ _  _ ns _) -> store graph1Table (Just lc1) record3{nextSibling = nd2}

                       return (fc1, nd2,(-1), lc1 )

             store graph1Table (Just nd1) record1{firstChild = new_fc1,lastChild = new_lc1}

             store graph1Table (Just nd2) (Labels nd1 pre post Set.empty Set.empty (-1) (-1) ns2 {- fc1 -} ls2)
             {- TODO: - update prevSibling for the firstChild of nd1 ??
                      - update firstChild for nd1 -}

             return ()

    ([]                                       ,[record2@(Labels tp2 _ _ hp2 dir2 _ _ ns2 ps2 )])     -- Case 3
          | tp2 > 0   -> do                                                                      -- Case 3.1
              (pre,post,prevSib)  <- insertIsolatedNode nd1 {- TODO: insert two labels after (PreLabel root), root == 0 -}
              store graph1Table (Just nd1) (Labels 0 pre post (Set.singleton nd2) Set.empty (-1) (-1) (-1) prevSib)
              return ()
          | otherwise -> do                                                                      -- Case 3.2
              (pre,post)  <- do
                a <- prevOf (PreLabel nd2) >>= \x -> getLabel x 
                b <-nextOf (PostLabel nd2) >>= \x -> getLabel x
                prend2 <- getLabel (PreLabel nd2)
                posnd2 <- getLabel (PostLabel nd2)
                let pre = average prend2 a 
                let post = average posnd2 b --update siblings and children of root
                return (pre,post) {- TODO: insert one label after (prevOf (PreLabel nd2)) -}
              {-post <- undefined  TODO: insert one label after (PostLabel nd2) -}
              if (ns2 >0 ) then --updating siblings 
                do 
                  res3 <- query firstRow (from graph1Table (at ns2))
                  case res3 of 
                    record3@(Labels _ _ _ _ _ _  _ _ ps) -> store graph1Table (Just ns2) record3{lastSibling = nd1}
                  return ()
              else return ()
              if (ps2 >0 ) then 
                do 
                  res4 <- query firstRow (from graph1Table (at ps2))
                  case res4 of 
                    record4@(Labels _ _ _ _ _ _  _ ns _) -> store graph1Table (Just ps2) record4{nextSibling = nd1}
                  return ()
              else return ()
              
              let record1 = Labels 0 pre post Set.empty Set.empty nd2 nd2 ns2 ps2
              store graph1Table (Just nd2) record2{tree_parent=nd1, nextSibling= (-1), lastSibling = (-1)}
              if Set.null hp2
                then updateDirectInAncestors nd1 record1 (Set.union dir2)
                else updateDirectInAncestors nd1 record1 (Set.insert nd2)

    ([]                                       ,[]                                          ) ->  -- Case 4
          do (pre1,pre2,post1,post2,prevSib1) <- insert2IsolatedNodes nd1 nd2 {- TODO: insert four labels after (PreLabel root) -}
             store graph1Table (Just nd1) (Labels 0   pre1 post1 Set.empty Set.empty nd2  nd2  (-1) prevSib1)
             store graph1Table (Just nd2) (Labels nd1 pre2 post2 Set.empty Set.empty (-1) (-1) (-1) (-1))
             return ()
  par <-  query firstRow (from nodeMapTable (at nd1))
  case par of
    (X pnd edges) ->  store nodeMapTable (Just nd1) (X pnd (nd2:edges))
  return()
--  store nodeMapTable (Just parent) (X pnd (List.nub (nd:edges)))
  where 
    average x y = (div x 2) + (div y 2)
    insertIsolatedNode nd1 = do 
      res <- query firstRow (from graph1Table (at 0))
      case res of 
        record@(Labels _ _ _ _ _ _  rlc _ _) -> if rlc < 0 then 
          do 
            let pre1 = average 0 Main.max
            let post1 = average pre1 Main.max
            store graph1Table (Just 0) record{firstChild = nd1, lastChild =nd1}
            return (pre1,post1, (-1))
          else do
            res1 <- query firstRow (from graph1Table (at rlc))
            case res1 of 
              record1@(Labels _ pr1 ps1 _ _ _ _ _ _)   -> do 
                let pre1 = average ps1 Main.max 
                let post1 = average pre1 Main.max
                store graph1Table (Just rlc) record1{ nextSibling = nd1} 
                store graph1Table (Just 0) record{ lastChild =nd1} 
                return (pre1,post1, rlc)
    insert2IsolatedNodes nd1 nd2 = do 
      res <- query firstRow (from graph1Table (at 0))
      case res of 
        record@(Labels _ _ _ _ _ _  rlc _ _) -> if rlc < 0 then 
          do 
            let pre1 = average 0 Main.max
            let post1 = average pre1 Main.max
            let pre2 = average pre1 post1
            let post2 = average pre2 post1
            store graph1Table (Just 0) record{firstChild = nd1, lastChild =nd1}
            return (pre1,pre2,post1,post2, (-1))
          else do
            res1 <- query firstRow (from graph1Table (at rlc))
            case res1 of 
              record1@(Labels  _ pr1 ps1 _ _ _ _ _ _ )-> do 
                let pre1 = average ps1 Main.max 
                let post1 = average pre1 Main.max
                let pre2 = average pre1 post1 
                let post2 = average pre2 post1 
                store graph1Table (Just rlc) record1{ nextSibling = nd1} 
                store graph1Table (Just 0) record{ lastChild =nd1}
                return (pre1,pre2,post1,post2,rlc )


handleDelete :: Nd -> Nd -> Daison ()
handleDelete nd1 nd2 = do
  istreeEdge <- isTreeEdge nd1 nd2
  deleteEdge nd1 nd2
  case istreeEdge of
    True -> do
      deleteDirectsandAncestors nd1 nd2
      removeTreeParent nd2
--      relabel nd2 
    False -> do
      flag <- isTheOnlyNonTreeEdge nd1 nd2
      if flag then
        do
          record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
          case record of
            [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
              deleteDirectsandAncestors tp nd1
              when ( not $ Set.null dir ) $ do
                record <- query firstRow (from graph1Table (at tp))
                updateDirectInAncestors tp record (Set.union dir)
            _ -> error $ "invalid from handle delete : " ++ show nd1 ++ show nd2
      else return ()
      deleteHopsFrom nd1 nd2
      return ()

addHop :: Nd -> Labels -> Nd -> Daison ()
addHop nd1 (Labels tp pr ps hp dir fc lc ns ls) nd2 = do
  store graph1Table (Just nd1) (Labels tp pr ps (Set.insert nd2 hp) dir fc lc ns ls)
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
    [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
      update_ graph1Table (return (nd, Labels 0 pr ps hp dir fc lc ns ls) )
    

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


reLabelMain :: PrePostRef -> Daison ()
reLabelMain  (PreLabel nd) =  do 
  let d = 3
  let count = 2
  record <- select [(nd, labels) | (labels) <- from graph1Table (at nd)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
      (newd, newcount,v, begin, end) <- mainLoop d count (PreLabel nd) (PreLabel nd) (PreLabel nd)
      reLabel begin end v newd newcount
      return ()
  return ()

reLabel :: PrePostRef -> PrePostRef -> PrePostRef -> Int64 ->Int64 -> Daison()
reLabel begin end v d count = do 
  let n = div d count
  vlabel <- getLabel v
  let newv = vlabel  Bits..&. (Bits.complement d )
  reLabelNodes begin end newv n 0
  return ()


reLabelNodes begin end v n i = do 
  if begin == end then 
    return ()
  else do 
    case begin of 
      (PreLabel nd ) ->   do
        record <- select [(nd, labels) | (labels) <- from graph1Table (at nd)  ] 
        case record of
          [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
            let newlabel = v + i * n
            update_ graph1Table (return (nd, Labels tp newlabel ps  hp dir fc lc ns ls) )
            return()
      (PostLabel nd ) ->   do
        record <- select [(nd, labels) | (labels) <- from graph1Table (at nd)  ] 
        case record of
          [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
            let newlabel = v + i * n
            update_ graph1Table (return (nd, Labels tp pr newlabel   hp dir fc lc ns ls) )
            return()
  newbegin <- nextOf (begin)
  reLabelNodes newbegin end v n (i +1)


mainLoop :: Int64 -> Int64-> PrePostRef ->PrePostRef -> PrePostRef -> Daison (Nd, Int64, PrePostRef, PrePostRef, PrePostRef)
mainLoop d count v begin end = do
  max <- getCounter
  if (d < max) then 
    do 
      (begin1, count1, v1, d1)  <- goPrev (begin) count v d
      (end1, count2, v2, d2)  <- goNext (end) count1 v1 d1
      if (count2 * count2 < fromIntegral (d2 +1)  ) then 
        return (d2, count2, v2, begin1, end1)
      else
        do 
          let d3 = d2 Bits..|. (Bits.shiftL d 1)
          mainLoop d3 count2 v2 begin1 end1
  else 
    return (d, count, v, begin, end)


goPrev :: PrePostRef -> Int64 -> PrePostRef -> Int64 -> Daison (PrePostRef, Int64, PrePostRef, Int64)
goPrev begin count v d = do 
  vlabel <- getLabel v
  newbegin <- prevOf begin 
  newbeginLabel <- getLabel (newbegin)
  if (newbeginLabel > (vlabel Bits..&. (Bits.complement d))) then 
    goPrev newbegin (count +1) v d
  else return (begin, count, v, d)


goNext :: PrePostRef -> Int64 -> PrePostRef -> Int64 -> Daison (PrePostRef, Int64, PrePostRef, Int64)
goNext end count v d = do 
  vlabel <- getLabel v
  newend <- prevOf end
  newendLabel <- getLabel (newend)
  if (newendLabel > (vlabel Bits..|. d)) then 
    goNext newend (count +1) v d
  else return (end, count, v, d)


getLabel :: PrePostRef -> Daison Nd
getLabel (PreLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ]
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> return pr
getLabel (PostLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ]
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> return ps

nextOf :: PrePostRef -> Daison PrePostRef 
nextOf (PreLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if fc >0 then return (PreLabel fc) 
      else return (PostLabel nd)
nextOf (PostLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if ns >0 then return (PreLabel ns)  
      else return (PostLabel trp)

prevOf :: PrePostRef -> Daison PrePostRef
prevOf (PreLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if ls >0 then return  (PostLabel ls) 
      else return (PreLabel trp)
prevOf (PostLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if lc >0 then  return (PostLabel lc)  
      else return (PreLabel nd)

getEdges :: Nd -> Daison [Nd]
getEdges nd = do 
  record <- select [ edgs| ( X n edgs) <- from nodeMapTable (at nd)  ] 
  return . head $ record

deleteDirectsandAncestors :: Nd -> Nd -> Daison()
deleteDirectsandAncestors nd1 nd2 = do
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
      update_ graph1Table (return (nd, Labels tp pr ps hp (Set.delete nd2 dir) fc lc ns ls ) )
      when (tp/=0) $ deleteDirectsandAncestors tp nd2
    _ -> error $ "invalid from deletedirectss and ancestors " ++ show nd1 ++ show nd2


deleteHopsFrom :: Nd -> Nd -> Daison ()
deleteHopsFrom nd1 nd2 = do
  record <- select [(nd1, labels) | (labels) <- from graph1Table (at nd1)  ] 
  case record of
    [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
      update_ graph1Table (return (nd, Labels tp pr ps (Set.delete nd2 hp) dir fc lc ns ls) )
    _ -> error "invalid"  


updateDirectInAncestors :: Nd -> Labels -> (Directs -> Directs) -> Daison ()
updateDirectInAncestors nd (Labels tp pr ps hp dir fc lc ns ls) f = do
  store graph1Table (Just nd) (Labels tp pr ps hp (f dir) fc lc ns ls)
  when (tp /= 0) $ do
    record <- query firstRow (from graph1Table (at tp))
    updateDirectInAncestors tp record f

updatePre :: Nd -> [Nd] -> Daison Bool
updatePre nd visited = do 
  record <- select [(nd,label1) | (label1) <- from graph1Table (at nd)  ] 
  case record of 
    [(nd, Labels trp pr ps hp dir fc lc ns ls  )] -> if pr == ps || elem nd visited then return True
                                           else   
                                             do
                                               c_counter <- getCounter
                                               incrementCounter
                                               update_ graph1Table (return (nd, Labels trp c_counter c_counter hp dir fc lc ns ls ))
                                               return False
    _ -> error "error " 

isTreeEdge :: Nd -> Nd -> Daison Bool
isTreeEdge nd1 nd2 = do
  record <- select [label1 | (label1) <- from graph1Table (at nd1)  ] 
  case record of 
    [(Labels tp pr ps hp dir fc lc ns ls)] -> case (List.elem nd2 hp) of
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
      Labels trp pr ps hp dir fc lc ns ls ->  do 
        if (Set.member nd2 hp) && (Set.size hp ==1) then 
          return True
        else if (Set.member nd2 hp) && (Set.size hp > 1) then
          return False
        else 
          error $ "error from istheonlynontreeedge : nd1 , nd2 : " ++ show nd1 ++ show nd2
    _ -> error " couldnt find record from isonlynontreeedge "


getNdIndex node = do
  nod <- select [ind | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
  case nod of
    [nd] -> return nd
    []   -> do 
      --c_counter <- getCounter
      --incrementCounter >> incrementCounter
      pkey <- insert_ nodeMapTable (X node [])
--      store  graph1Table (Just pkey) (Labels (-1) (-3) (-2) Set.empty Set.empty (-100) (-100) (-100) (-100)  )
      return pkey
    _    -> error $ "ivalid getindex nd :" ++ show nod

updatePost :: Nd -> Daison ()
updatePost nd = do 
  record <- select [(nd,label1) | (label1) <- from graph1Table (at nd)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir fc lc ns ls ->  do 
        c_counter <- getCounter
        incrementCounter
        update_ graph1Table (return (nd, Labels trp pr c_counter hp dir fc lc ns ls ))
      _   -> error "error from updatepost"
    _ -> error "error " 


getParent :: Nd -> Daison Nd
getParent node = do
  record <- select [(node, labels) | (labels) <- from graph1Table (at node) ] 
  case record of
    [] -> error $ "invalid parent node :" ++ show node
    [(ind1,  label1)] -> case label1 of 
      (Labels trp _ _ _ _ _ _ _ _ ) -> return trp
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
--  liftIO $ print $ " counter current value : " ++ show (c_counter+1) 
  update_ counters (return (1, ("counter", c_counter+1) )) 
  


resetCounter :: Daison ()
resetCounter = update_ counters (return (1, ("counter", 0) ))



queryM :: Nd -> Nd -> Daison Bool
queryM nd1 nd2 = do
  label1 <- select [labels | (labels) <- from graph1Table (at nd1)  ] 
  label2 <- select [labels | (labels) <- from graph1Table (at nd2)  ]
  case label1 of 
    [(Labels trp1 pre1 post1 hp1 dir1 fc1 lc1 ns1 ls1)] -> do
      case label2 of
        [(Labels trp2 pre2 post2 hp2 dir2 fc2 lc2 ns2 ls2)] -> if  (pre1 < post2 && post2 <= post1) then return True
                                             else return False
        _ -> error "error "                
    _ -> error "error again "


search :: Nd -> Nd -> Daison Bool
search nd1 nd2 = do
  label1 <- select [labels | (labels) <- from graph1Table (at nd1)  ] 
  label2 <- select [labels | (labels) <- from graph1Table (at nd2)  ]
  case label1 of 
    [(Labels trp1 pre1 post1 hp1 dir1 fc1 lc1 ns1 ls1)] -> do
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




{- 

updateDirects :: Nd -> Nd -> Daison()
updateDirects parent gp = do
  record <- select [(gp,label1) | (label1) <- from graph1Table (at gp)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir fc lc ns ls ->  do
        update_ graph1Table (return (nd, Labels trp pr ps hp (Set.insert parent dir) fc lc ns ls ))
      _ -> error "updatedirects error"
    _   -> liftIO $ print record
  when (gp > 1) $ do
    ggp <- getParent gp
    when (ggp > 1) $ updateDirects parent ggp

processNodes :: GraphMap Node -> Node -> Node -> Daison()
processNodes graph node parent_node = do
  nd <- getNdIndex node
  parent <- getNdIndex parent_node
  par <-  query firstRow (from nodeMapTable (at parent))
  case par of
    (X pnd edges) ->  store nodeMapTable (Just parent) (X pnd (List.nub (nd:edges)))
  handleInsert  parent nd
{-   x <- insertNodeinDB nd parent 
  unless x $ do -}
  let adjacent = Map.lookup node graph
  case adjacent of
      Nothing      -> return () {- do
        nod <- getNdIndex nd 
        record <- select [(nod,label1) | (label1) <- from graph1Table (at nod)  ] 
        liftIO $ print $ " Nothing : " ++ show record
        case record of 
          [(nd, label)] -> case label of
            Labels trp pr ps hp dir fc lc ns ls ->  update_ graph1Table (return (nd, Labels trp pr ps hp dir (-1) (-1) ns ls )) -}
      Just []   -> return (){- do
        nod <- getNdIndex nd 
        record <- select [(nod,label1) | (label1) <- from graph1Table (at nod)  ] 
        liftIO $ print $ " just : " ++ show record
        case record of 
          [(nd, label)] -> case label of
            Labels trp pr ps hp dir fc lc ns ls ->  update_ graph1Table (return (nd, Labels trp pr ps hp dir (-1) (-1) ns ls )) -}
      Just rest    ->do
        mapM_ (\x -> processNodes graph x node ) rest
{-         children <- mapM (\x -> getNdIndex x) rest 
        nod <- getNdIndex nd 
        case filter (>nod) children of 
          [] -> return ()
          [ele] -> updateSiblings  (-1) ele (-1)
          first : second : [] -> updateSiblings  (-1) first second >> updateSiblings first second   (-1)
          list -> do 
            let values =  (( (-1) , head list, head (tail list) ) : zip3 (list) ((tail list)) (tail (tail list)) )++ [( last (init list) ,last list,  (-1))]
            mapM_ (\(left, ele , right) -> updateSiblings left ele right) values
        
        record <- select [(nod,label1) | (label1) <- from graph1Table (at nod)  ] 
        case record of 
          [(nd, label)] -> case label of
            Labels trp pr ps hp dir fc lc ns ls ->  do
              first_child <- getNdIndex (head rest) >>= \fc -> if nd > fc then return (-1) else return fc
              case (tail rest) of 
                []  -> update_ graph1Table (return (nd, Labels trp pr ps hp dir first_child first_child ns ls ))
                child -> do 
                  last_child <- getNdIndex (last rest) >>= \lc -> if nd > lc then return (-1) else return lc
                  update_ graph1Table (return (nd, Labels trp pr ps hp dir first_child last_child ns ls ))
 -}--    getNdIndex nd >>= \nd1 -> updatePost nd1




insertNodeinDB :: Node -> Node ->Daison Bool
insertNodeinDB node parent  = do
  map <- select [ind | (ind, ( X nd edgess )) <- from nodeMapTable everything , nd == node  ]
  par <- if (node == parent) then return [(0,( C 'a',[]))] 
         else select [(ind,(nd, edgess)) | (ind, ( X nd edgess )) <- from nodeMapTable everything , nd == parent  ]
  let parent_ndmp = fst . head $ par
  let edges_par = snd . snd . head $ par 
  let parent_ndc = fst . snd . head $ par
  case map of
    [] -> do
{-       c_counter <- getCounter
      incrementCounter -}
      l_pre <- prevOf (PostLabel parent_ndmp) >>= \x -> getLabel x
      l_post <- getLabel (PostLabel parent_ndmp)
      let pre = average l_pre l_post
      let post = average pre l_post
      liftIO $ print  $ " l_pre : " ++ show l_pre ++ " l_post : " ++ show l_post ++ " pre : " ++ show pre ++ " post : " ++ show post

      pkey <-  insert_ nodeMapTable (X node [])
      store graph1Table (Just pkey) (Labels parent_ndmp pre post Set.empty Set.empty (-100) (-100) (-100) (-100) )    
      
      when (parent_ndmp > 1) $ do
--        parent_record <- select [(parent_ndmp, labels2) | (labels2) <- from graph1Table (at parent_ndmp)  ] 
--        case parent_record of
--          [(indp ,(Labels ptrp ppr pps php pdir pte))] -> update_ graph1Table (return (indp,(Labels ptrp ppr pps php pdir (pkey:pte)) ))
--        parent_ndmprecord <- select [(ind,edgess) | (ind, ( X nd edgess )) <- from nodeMapTable everything , nd == parent  ]
        update_ nodeMapTable  (return (parent_ndmp, (X (parent_ndc) (pkey:edges_par) ) ))

      return False
    [nod]   ->  do
      parent_record <- select [(parent_ndmp, labels2) | (labels2) <- from graph1Table (at parent_ndmp)  ] 
      case parent_record of 
          [] -> error "error "
          [(indp, labelp)] -> case labelp of 
            (Labels ptrp ppr pps php pdir fc lc ns ls) -> do
              update_ graph1Table (return (indp,(Labels ptrp ppr pps (Set.insert (head map) php ) pdir fc lc ns ls ) ))
              when (ptrp > 1) $ updateDirects parent_ndmp ptrp 
      update_ nodeMapTable  (return (parent_ndmp, (X (parent_ndc) (nod:edges_par) ) ))

      return True 
  where
    average x y = (div x 2) + (div y 2)



updateSiblings :: Nd -> Nd -> Nd -> Daison ()
updateSiblings left nd right = do
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> update_ graph1Table (return (nd, Labels trp pr ps hp dir fc lc right left ))






 -}