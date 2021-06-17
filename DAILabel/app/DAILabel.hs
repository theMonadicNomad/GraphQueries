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

type Nd = Key Labels
--  deriving (Eq, Ord, Read, Data)
type Ndc = Char
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

data PrePostRef = PreLabel Nd | PostLabel Nd


data Labels = Labels {
    tree_parent :: Nd,
    pre :: Nd,
    post :: Nd,
    hops :: Hops,
    directs :: Directs,
    firstChild :: Nd,
    lastChild :: Nd,
    nextSibling :: Nd,
    lastSibling :: Nd
} deriving (Typeable, Data )

--type Y = X Special2

data X = X { nd :: Node,  
     edges :: Edges
} deriving (Typeable, Data, Show)



data Node = C Ndc | I Nd deriving (Eq,Data, Ord, Show)

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
      (C 'c', [])
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
  show (Labels a b c d e f g h i) = "TP: " ++  show a ++ " Pre: " ++ show b ++
   " Post:  " ++ show c  ++ " Hops: " ++ show d ++ " Directs:  " ++ show e ++
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
{-     c_counter <- getCounter
    if (c_counter >0) then return ()
    else
      do  -}     
    insert counters (return ( "counter", 0 ))
    let Graph g = graph11
    let graphmap1 =  Map.fromList g
    process graphmap1
    a <- select [ x | x <- from graph1Table everything ]
    b <- select [ x | x <- from nodeMapTable everything ]
    return (a,b)
  mapM_ (\y -> putStrLn (show y) ) a
  mapM_ (\y -> putStrLn (show y) ) b
  closeDB db
--  makeDynamicOperation databaseTest ReadWriteMode


process :: GraphMap Node -> Daison ()
process graphmap = do
  let firstnode = fst $ Map.elemAt 0 graphmap
  processNodes graphmap firstnode firstnode
  parents <- getAllParents firstnode
  processSiblings parents



getLabel :: PrePostRef -> Daison Nd
getLabel (PreLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ]
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> return pr
getLabel (PostLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ]
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> return ps

nextOf :: PrePostRef -> Daison Nd 
nextOf (PreLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if fc >0 then getLabel (PreLabel fc) 
      else return ps
nextOf (PostLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if ns >0 then getLabel (PreLabel ns)  
      else getLabel (PostLabel trp)

prevOf :: PrePostRef -> Daison Nd 
prevOf (PreLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if ls >0 then getLabel (PostLabel ls) 
      else getLabel (PreLabel trp)
prevOf (PostLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if lc >0 then getLabel (PostLabel lc)  
      else return pr


getAllParents :: Node -> Daison (Set Nd)
getAllParents nd = do
  parentsList <- select [a | (ind, ( Labels a b c d e f g h i  )) <- from graph1Table everything , a > 0  ]
  let parents = Set.fromList parentsList
  return parents

processSiblings :: (Set Nd )-> Daison()
processSiblings parents = do
  let parentsList = Set.toList parents
  mapM_ (\x -> getSiblings x  ) parentsList

getSiblings :: Nd -> Daison()
getSiblings nd = do
  edgesList <- select [edges | ( X nds edges ) <- from nodeMapTable (at nd)  ]
  let edges = head edgesList
  case edges of
    [] -> return ()
    [ele] -> updateSiblings (-1) ele (-1)
    first : second : [] -> updateSiblings (-1) first second >> updateSiblings first second (-1)
    rest -> do
      let values = ( last (init rest) ,last rest, -1) : (-1, head rest, head (tail rest) ) : zip3 (rest) ((tail rest)) (tail (tail rest))
      mapM_ (\(x,y,z) -> updateSiblings x y z ) values
  
updateSiblings :: Nd -> Nd -> Nd -> Daison ()
updateSiblings left nd right = do
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> update_ graph1Table (return (nd, Labels trp pr ps hp dir fc lc left right ))


processNodes :: GraphMap Node -> Node -> Node -> Daison()
processNodes graph nd parent = do
  x <- insertNodeinDB nd parent
  unless x $ do
    let adjacent = Map.lookup nd graph
    case adjacent of
 --     Nothing -> return ()
      Nothing      -> do
        nod <- getNdIndex nd 
        record <- select [(nod,label1) | (label1) <- from graph1Table (at nod)  ] 
        case record of 
          [(nd, label)] -> case label of
            Labels trp pr ps hp dir fc lc ns ls ->  update_ graph1Table (return (nd, Labels trp pr ps hp dir (-1) (-1) ns ls ))
      Just []   ->do
        nod <- getNdIndex nd 
        record <- select [(nod,label1) | (label1) <- from graph1Table (at nod)  ] 
        case record of 
          [(nd, label)] -> case label of
            Labels trp pr ps hp dir fc lc ns ls ->  update_ graph1Table (return (nd, Labels trp pr ps hp dir (-1) (-1) ns ls ))

      Just rest    ->do
        mapM_ (\x -> processNodes graph x nd ) rest
        nod <- getNdIndex nd 
        record <- select [(nod,label1) | (label1) <- from graph1Table (at nod)  ] 
        case record of 
          [(nd, label)] -> case label of
            Labels trp pr ps hp dir fc lc ns ls ->  do
              first_child <- getNdIndex (head rest) >>= \fc -> if nd > fc then return (-1) else return fc
              case (tail rest) of 
                []  -> update_ graph1Table (return (nd, Labels trp pr ps hp dir first_child (-1) ns ls ))
                child -> do 
                  last_child <- getNdIndex (last rest) >>= \lc -> if nd > lc then return (-1) else return lc
                  update_ graph1Table (return (nd, Labels trp pr ps hp dir first_child last_child ns ls ))
    getNdIndex nd >>= \nd1 -> updatePost nd1
    

getNdIndex node = do
  nod <- select [ind | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
  case nod of
    [nd] -> return nd
    []   -> do 
      --c_counter <- getCounter
      --incrementCounter >> incrementCounter
      pkey <- insert_ nodeMapTable (X node [])
      store  graph1Table (Just pkey) (Labels (-1) (-3) (-2) Set.empty Set.empty (-100) (-100) (-100) (-100)  )
      return pkey
    _    -> error $ "ivalid getindex nd :" ++ show nod



insertNodeinDB :: Node -> Node -> Daison Bool
insertNodeinDB node parent = do
  map <- select [ind | (ind, ( X nd edgess )) <- from nodeMapTable everything , nd == node  ]
  par <- if (node == parent) then return [(0,( C 'a',[]))] 
         else select [(ind,(nd, edgess)) | (ind, ( X nd edgess )) <- from nodeMapTable everything , nd == parent  ]
  let parent_ndmp = fst . head $ par
  let edges_par = snd . snd . head $ par 
  let parent_ndc = fst . snd . head $ par
  case map of
    [] -> do
      c_counter <- getCounter
      incrementCounter
      pkey <-  insert_ nodeMapTable (X node [])
      store graph1Table (Just pkey) (Labels parent_ndmp c_counter c_counter Set.empty Set.empty (-100) (-100) (-100) (-100) )    
      
      when (parent_ndmp > 0) $ do
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
              when (ptrp > 0) $ updateDirects parent_ndmp ptrp 
      update_ nodeMapTable  (return (parent_ndmp, (X (parent_ndc) (nod:edges_par) ) ))

      return True 
{-     first : rest -> error "duplicate records in the database table, please verify"
 -}





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

updateDirects :: Nd -> Nd -> Daison()
updateDirects parent gp = do
  record <- select [(gp,label1) | (label1) <- from graph1Table (at gp)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir fc lc ns ls ->  do
        update_ graph1Table (return (nd, Labels trp pr ps hp (Set.insert parent dir) fc lc ns ls ))
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
      (Labels trp _ _ _ _ _ _ _ _ ) -> return trp
    _           -> error "multiple parents error "

getCounter :: Daison Nd
getCounter = select [ x | x <- from counters (at 1) ] >>= \p -> return . snd . head $ p  

incrementCounter :: Daison ()
incrementCounter = do
  c_counter <- getCounter  
  liftIO $ print $ " counter current value : " ++ show (c_counter+1) 
  update_ counters (return (1, ("counter", c_counter+1) )) 
  


resetCounter :: Daison ()
resetCounter = update_ counters (return (1, ("counter", 0) ))