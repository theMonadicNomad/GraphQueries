{-# LANGUAGE MultiWayIf #-}
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State as CMS
import Control.Monad.RWS
import Data.List (intercalate)
--import Control.Monad.IO.Unlift


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
    hopnodes     :: Set Nd,
    specialnodes :: Set Nd ,
    treeEdges :: Set (Nd, Nd),
    nonTreeEdges :: Set (Nd, Nd) } deriving Show


initSt :: St
initSt = St {
    dailabel     = Map.empty,
    parentNodes = Map.empty,
    counter     = 0,
    hopnodes        = Set.empty,
    specialnodes    = Set.empty,
    treeEdges = Set.empty,
    nonTreeEdges = Set.empty
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
    let ((), upState, w) = runRWS (process) input initSt
    print w
    updateGraph input upState 


updateGraph :: Input-> St-> IO()
updateGraph input upState = do
    putStrLn ("Enter your choice for (I) for Edge Insertion or (D) for Edge Deletion : ")
    choice <- getChar
    putStr (" Enter the first node of the edge that you want to update : ")
    firstChar <- getChar
    putStr (" Enter the second node of the edge that you want to update : ")
    secondChar <- getChar
    case choice of
        'I' -> do
            let (updatedInput, updatedState, w ) = runRWS (handleInsert (Nd firstChar) (Nd secondChar) ) input upState
            print w 
            updateGraph updatedInput updatedState
        'D' ->do
            let (updatedInput, updatedState, w ) = runRWS (handleDelete (Nd firstChar) (Nd secondChar) ) input upState
            print w 
            updateGraph updatedInput updatedState


handleInsert :: Nd -> Nd -> MKDAILabel Input
handleInsert nd1 nd2 = undefined


-- | Updates the state by logic (tree edge and other conditions), Input graph.
handleDelete :: Nd -> Nd -> MKDAILabel Input
handleDelete nd1 nd2 = do
    input <- ask
    istreeEdge <- isTreeEdge nd1 nd2
    case istreeEdge of
                True -> do 
                    -- modify 
                   -- liftIO $ putStr("True")
                    return input
                False -> {- liftIO $  putStr("False") >> -} return input
            

isTreeEdge ::Nd -> Nd -> MKDAILabel Bool
isTreeEdge nd1 nd2 = do
    current_treeEdges <- gets treeEdges
    return $ elem (nd1, nd2)  current_treeEdges


process :: MKDAILabel ()
process =  do
    (processNodes (Nd 'a') (Nd 'a') ) 
    current_dailabel <- gets dailabel
    tell [(show current_dailabel)]
    parentnodes <- gets parentNodes
    tell [(show parentnodes)]
    flag <- search (Nd 'd') (Nd 'a')
    tell [(show flag)]
    current_treeEdges <- gets treeEdges
    tell [(show current_treeEdges)]
    current_nonTreeEdges <- gets nonTreeEdges
    tell [(show current_nonTreeEdges)]
    current_hops <- gets hopnodes
    tell [(show current_hops)]
    current_specialnodes <- gets specialnodes
    tell [(show current_specialnodes)]


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
    current_treeEdges <- gets treeEdges
    current_nonTreeEdges <- gets nonTreeEdges
    current_specialnodes <- gets specialnodes
    chops <- gets hopnodes
   -- tell [(show nd ++ " : "++ show cailabel ++ " \\\n")]
    let label = Map.lookup nd current_dailabel
    case label of 
        Nothing -> (modify $ \st -> st { dailabel = Map.insert nd (Labels parent current_counter current_counter Set.empty Set.empty) current_dailabel,
                                         parentNodes    = Map.insert nd parent parentnodes, 
                                         counter = current_counter+1,
                                         treeEdges = Set.insert (parent, nd) current_treeEdges
                                        } ) >> pure False
        _       -> do
            let label = Map.lookup parent current_dailabel
            case label of 
                Just (Labels trp pr ps hp dir) -> do
                     (modify $ \st -> st { dailabel = Map.insert parent (Labels trp pr ps (Set.insert nd hp) dir) current_dailabel,
                                           hopnodes = Set.insert nd chops,
                                           specialnodes = Set.insert parent current_specialnodes,
                                           nonTreeEdges = Set.insert (parent, nd) current_nonTreeEdges      } )
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


deleteEdge :: Nd -> Nd -> MKDAILabel ()
deleteEdge nd1 nd2 = undefined

insertEdge :: Nd -> Nd -> MKDAILabel ()
insertEdge nd1 nd2 = undefined