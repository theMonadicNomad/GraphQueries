{-# LANGUAGE MultiWayIf #-}
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State as CMS
import Control.Monad.RWS
import Data.List (intercalate)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


type MKDAILabel = RWS Input Output St

newtype Nd = Nd Char
  deriving (Eq, Ord, Show, Read)

type Output = [String]
type Input = Map Nd [Nd]
type Hops = Set Nd
type Directs = Set Nd
type Pre = Int
type Post = Int
type Graph =  [(Nd, [Nd])]

data Labels = Labels {
    tree_parent :: Nd,
    pre :: Pre,
    post :: Post,
    hops :: Hops,
    directs :: Directs
} deriving Show

data St = St {
    dailabel      :: Map Nd Labels,
    parentNodes  :: Map Nd Nd,
    counter      :: Int,
    hopnodes     :: [Nd],
    specialnodes :: [Nd]{- ,
    nonTreeEdges :: (Nd, Nd) -}
} deriving Show


initSt :: St
initSt = St {
    dailabel     = Map.empty,
    parentNodes = Map.empty,
    counter     = 0,
    hopnodes        = [],
    specialnodes    = []{- ,
    nonTreeEdges -}
    }

graph1 :: Graph
graph1 = 
    [ (Nd 'a', [ Nd 'b', Nd 'c'] ),
      (Nd 'b', [ Nd 'c'] )
    ]

graph2 :: Graph
graph2 = 
    [ (Nd 'a', [ Nd 'b', Nd 'c'] ),
      (Nd 'b', [ Nd 'd', Nd 'e', Nd 'f' ] ),
      (Nd 'c', [ Nd 'h' ] ),
      (Nd 'd', [ Nd 'k' ] ),
      (Nd 'e', [ Nd 'g', Nd 'h' ] ),
      (Nd 'f', [ Nd 'g' ] ),
      (Nd 'g', [ Nd 'i', Nd 'j' ] ),
      (Nd 'h', [ Nd 'k' ] ),
      (Nd 'i', [ Nd 'k' ] ),
      (Nd 'j', [ Nd 'k' ] ),
      (Nd 'k', [] )
    ]
   
main = do
    let input = Map.fromList graph2
    let ((), w) = evalRWS (process) input initSt
    print w

process :: MKDAILabel ()
process =  do
    (processNodes (Nd 'a') (Nd 'a') ) 
    current_dailabel <- gets dailabel
    tell [(show current_dailabel)]
    parentnodes <- gets parentNodes
    tell [(show parentnodes)]
    flag <- search (Nd 'a') (Nd 'b')
    tell [(show flag)]

updatePost :: Nd ->MKDAILabel()
updatePost nd = do 
    current_dailabel <- gets dailabel
    current_counter <- gets counter
    let label = Map.lookup nd current_dailabel
    case label of
            Just (Labels trp pr ps hp dir) -> modify $ \st-> st {dailabel = Map.insert nd (Labels trp pr current_counter hp dir) current_dailabel,
                                                            counter = current_counter+1 }
            Nothing -> error "error"

processNodes :: Nd -> Nd -> MKDAILabel()
processNodes nd parent = do
    inp <- ask
    x <- insertNodeinState nd parent
    unless x $ do
        let fun = Map.lookup nd inp
        case fun of
            Nothing       -> return ()
            Just []       -> return () 
            Just rest -> mapM_ (\x -> processNodes x nd) rest 
        updatePost nd

insertNodeinState :: Nd -> Nd -> MKDAILabel Bool
insertNodeinState nd parent = do
    current_dailabel <- gets dailabel
    current_counter <- gets counter
    parentnodes <- gets parentNodes
    chops <- gets hopnodes
   -- tell [(show nd ++ " : "++ show cailabel ++ " \\\n")]
    let label = Map.lookup nd current_dailabel
    case label of 
        Nothing -> (modify $ \st -> st { dailabel = Map.insert nd (Labels parent current_counter current_counter Set.empty Set.empty) current_dailabel,
                                         parentNodes    = Map.insert nd parent parentnodes, 
                                        counter = current_counter+1 } ) >> pure False
        _       -> do
            let label = Map.lookup parent current_dailabel
            case label of 
                Just (Labels trp pr ps hp dir) -> do
                     (modify $ \st -> st { dailabel = Map.insert parent (Labels trp pr ps (Set.insert nd hp) dir) current_dailabel,
                                              hopnodes = nd:chops   } )
                     let grandparent = Map.lookup parent parentnodes
                     case grandparent of
                         Just gp -> updateDirects parent gp
                         Nothing -> error "error grandparent"
                Nothing -> error "error "
            pure True



updateDirects :: Nd -> Nd -> MKDAILabel()
updateDirects parent gp = do
    current_dailabel <- gets dailabel
    let glabel = Map.lookup gp current_dailabel
    case glabel of 
        Just (Labels trp ppr pps php pdir) -> modify $ \st -> st { dailabel = Map.insert gp (Labels trp ppr pps php (Set.insert parent pdir) ) current_dailabel}
        Nothing -> error "error again "
    parentnodes <- gets parentNodes
    let Just ggp = Map.lookup gp parentnodes
    if (ggp == gp) then return()
    else updateDirects parent ggp



query :: Nd -> Nd -> MKDAILabel Bool
query nd1 nd2 = do
    current_dailabel <- gets dailabel
    let label1 = Map.lookup nd1 current_dailabel
    case label1 of 
        Just (Labels trp1 pre1 post1 hp1 dir1) -> do
            let label2 = Map.lookup nd2 current_dailabel
            case label2 of
                Just (Labels trp2 pre2 post2 hp2 dir2) -> 
                    if  (pre1 < post2 && post2 <= post1) then return True
                    else return False
                Nothing -> error "error "                
        Nothing -> error "error again "



search :: Nd -> Nd -> MKDAILabel Bool
search nd1 nd2 = do
    current_dailabel <- gets dailabel
    let label1 = Map.lookup nd1 current_dailabel
    case label1 of
        Just (Labels trp1 pre1 post1 hp1 dir1) -> do
            flag <- query nd1 nd2
            if flag then return True
            else do
                x <- or <$> (mapM (\x -> query x nd2) (Set.toList hp1)) 
                if not x 
                     then or <$> (mapM (\x -> query x nd2) (Set.toList dir1)) 
                else return x

