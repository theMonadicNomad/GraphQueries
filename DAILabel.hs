{-# LANGUAGE MultiWayIf #-}
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State as CMS
import Control.Monad.RWS
import Data.List (intercalate,find)
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
    nonTreeEdges :: Set (Nd, Nd),
    maxID :: Int } deriving Show


initSt :: St
initSt = St {
    dailabel     = Map.empty,
    parentNodes = Map.empty,
    counter     = 0,
    hopnodes        = Set.empty,
    specialnodes    = Set.empty,
    treeEdges = Set.empty,
    nonTreeEdges = Set.empty,
    maxID = 0
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

graph3 :: Graph
graph3 = 
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
      (Nd 'k', [] ),
      (Nd 'l', [Nd 'm'] ),
      (Nd 'm', [])

    ]
  
   
main = do
    let input = Map.fromList graph3
    let ((), upState, w) = runRWS (process) input initSt
    print w
    makeDynamicOperation input upState 


makeDynamicOperation :: Input-> St-> IO()
makeDynamicOperation input upState = do
    putStrLn ("Enter your choice for (I) for Edge Insertion or (D) for Edge Deletion : ")
    choice <- getChar
    putStrLn (" Enter the first node of the edge that you want to update : ")
    firstChar <- getChar
    putStrLn (" Enter the second node of the edge that you want to update : ")
    secondChar <- getChar
    case choice of
        'I' -> do
            let (updatedInput, updatedState, w ) = runRWS (handleInsert (Nd firstChar) (Nd secondChar) ) input upState
            print w 
            makeDynamicOperation updatedInput updatedState
        'D' ->do
            let (updatedInput, updatedState, w ) = runRWS (handleDelete (Nd firstChar) (Nd secondChar) ) input upState
            print w
            print (show updatedState) 
            makeDynamicOperation updatedInput updatedState


handleInsert :: Nd -> Nd -> MKDAILabel Input
handleInsert nd1 nd2 = do
    isolated1 <- isIsolated nd1
    isolated2 <- isIsolated nd2
    special1 <- isSpecial nd1
    special2 <- isSpecial nd2
    hasparent2 <- hasParent nd2
    input <- ask
    maxid <- gets maxID
    current_dailabel <- gets dailabel
    let newGraph = Map.fromList (insertEdgeinGraph (Map.toList input) nd1 nd2)

    if isolated1 
        then do
            if isolated2 
                then do
                    (modify $ \st -> st { dailabel = Map.insert nd1 (Labels nd2 (maxid+1) (maxid+4) Set.empty Set.empty) current_dailabel  ,
                                          maxID = maxid + 4,
                                          counter = maxid +4 })
                    updated_dailabel <- gets dailabel
                    (modify $ \st -> st { dailabel = Map.insert nd2 (Labels nd2 (maxid+2) (maxid+3) Set.empty Set.empty) updated_dailabel } )
            else if hasparent2
                then do 
                    let glabel = Map.lookup nd1 current_dailabel
                    case glabel of 
                        Just (Labels trp ppr pps php pdir) -> modify $ \st -> st { dailabel = Map.insert nd1 (Labels trp (maxid+1) (maxid+2) (Set.insert nd2 php) pdir) current_dailabel,
                                                                                   maxID = maxid +2,
                                                                                   counter = maxid+2 }
                        Nothing -> error "error again "
            else return ()
    else 
        do
            if isolated2
                then relabel newGraph (Nd 'a') [] >> return ()
            else if hasparent2
                then do 
                    if special1
                        then addHop nd1 nd2 
                    else 
                        getParent nd1 >>= \parent1 -> addDirectinAncestors parent1 nd1 >> addHop nd1 nd2 
            else
                do
                    if special2
                        then addDirectinAncestors nd1 nd2
                    else 
                        getDirects nd2 >>= \dir2 -> addDirectsandAncestors nd1 dir2
                    setTreeParent nd2 nd1
                    relabel newGraph (Nd 'a') [] >> return ()
    return newGraph


setTreeParent :: Nd -> Nd -> MKDAILabel ()
setTreeParent nd1 nd2 = do 
    current_dailabel <- gets dailabel
    let label = Map.lookup nd1 current_dailabel
    case label of 
        Just (Labels trp pr ps hp dir) -> do
                    (modify $ \st -> st { dailabel = Map.insert nd1 (Labels nd2 pr ps hp dir) current_dailabel } )

        Nothing -> error "error "



getParent :: Nd -> MKDAILabel Nd
getParent nd = do
    current_dailabel <- gets dailabel 
    let label = Map.lookup nd current_dailabel
    case label of 
        Just (Labels trp pr ps hp dir) -> return trp
        Nothing -> error "error "
     

getDirects :: Nd -> MKDAILabel Directs
getDirects nd = do
    current_dailabel <- gets dailabel 
    let label = Map.lookup nd current_dailabel
    case label of 
        Just (Labels trp pr ps hp dir) -> return dir
        Nothing -> error "error "1





hasParent :: Nd -> MKDAILabel Bool
hasParent nd = do 
    current_parentnodes <- gets parentNodes
    let flag = Map.lookup nd current_parentnodes
    case flag of 
         Just nd -> return False
         _       -> return True
 

insertEdgeinGraph :: Graph -> Nd -> Nd -> Graph
insertEdgeinGraph graph nd1 nd2 = case (find (\c -> fst c == nd1) graph) of
    Just _ -> [ if x == nd1 then (x, nd2 : y) else (x,y) | (x,y) <- graph]
    Nothing -> (nd1 ,[nd2]) :graph


isIsolated :: Nd -> MKDAILabel Bool
isIsolated nd = do
    input <- ask
    let nodelist = Map.lookup nd input
    case nodelist of
        Just ndlist -> return True
        _           -> return False


isSpecial :: Nd -> MKDAILabel Bool
isSpecial nd =  do
    current_specialnodes <- gets specialnodes
    let flag = Set.member nd current_specialnodes
    return flag




-- | Updates the state by logic (tree edge and other conditions), Input graph.
handleDelete :: Nd -> Nd -> MKDAILabel Input
handleDelete nd1 nd2 = do
    input <- ask
    istreeEdge <- isTreeEdge nd1 nd2
    let newGraph = Map.fromList (removeEdgeFromGraph (Map.toList input) nd1 nd2)
    case istreeEdge of
                True -> do
                    deleteDirectsandAncestors nd1 nd2
                    current_dailabel <- gets dailabel
                    tell ["before relabel DAILABEL HERE"]
                    tell [(show current_dailabel)]
                    reInitializeCounter 
                    tell [ show newGraph]
                    removeTreeParent nd2
                    relabel newGraph (Nd 'a') []
                    updated_dailabel <- gets dailabel
                    tell ["UPDATED DAILABEL HERE"]
                    tell [(show updated_dailabel)]
                    return newGraph
                False -> do
                    flag <- isTheOnlyNonTreeEdge nd1 nd2
                    -- tell [(show flag)]
                    if flag then
                        do
                            current_dailabel <- gets dailabel
                            let label = Map.lookup nd1 current_dailabel
                            case label of 
                                Just (Labels trp pr ps hp dir) -> do
                                    deleteSpecialNode nd1
                                    deleteDirectsandAncestors trp nd1
                                    if null dir then return ()
                                    else 
                                        addDirectsandAncestors trp dir
                                _ -> error "invalid "
                    else {- tell ["fromn else "] >> -} return ()
                    deleteHops nd1 nd2
                    current_dailabel <- gets dailabel
                    -- tell[(show current_dailabel)]
                    return newGraph
            

reInitializeCounter :: MKDAILabel ()
reInitializeCounter = do
    current_counter <- gets counter
    modify $ \st -> st { counter = 0 }

removeTreeParent :: Nd -> MKDAILabel ()
removeTreeParent nd = do
    current_dailabel <- gets dailabel
    let label = Map.lookup nd current_dailabel
    case label of 
        Just (Labels trp pr ps hp dir) -> do
                    (modify $ \st -> st { dailabel = Map.insert nd (Labels nd pr ps hp dir) current_dailabel } )

        Nothing -> error "error "




removeEdgeFromGraph :: Graph -> Nd -> Nd -> Graph
removeEdgeFromGraph graph nd1 nd2 = [ if x == nd1 then (x, remov nd2 y) else (x,y) | (x,y) <- graph]

remov :: Nd ->[Nd] -> [Nd]
remov remNode ys = [ y | y <- ys, y /= remNode] 



deleteSpecialNode :: Nd -> MKDAILabel ()
deleteSpecialNode nd = do
    current_specialnodes <- gets specialnodes
    modify $ \st -> st { specialnodes = Set.delete nd current_specialnodes }


deleteHops :: Nd -> Nd -> MKDAILabel()
deleteHops nd1 nd2 = do
    current_dailabel <- gets dailabel
    let label = Map.lookup nd1 current_dailabel
    case label of 
        Just (Labels trp pr ps hp dir) -> do
                    (modify $ \st -> st { dailabel = Map.insert nd1 (Labels trp pr ps (Set.delete nd2 hp) dir) current_dailabel } )

        Nothing -> error "error "


addHop :: Nd -> Nd -> MKDAILabel ()
addHop nd1 nd2 = do 
    current_dailabel <- gets dailabel
    let label = Map.lookup nd1 current_dailabel
    case label of 
        Just (Labels trp pr ps hp dir) -> do
                    (modify $ \st -> st { dailabel = Map.insert nd1 (Labels trp pr ps (Set.insert nd2 hp) dir) current_dailabel } )

        Nothing -> error "error "


addDirectinAncestors :: Nd -> Nd -> MKDAILabel ()
addDirectinAncestors anc d  = do
    current_dailabel <- gets dailabel
    let label = Map.lookup anc current_dailabel
    case label of 
        Just (Labels trp pr ps hp dir) -> do
            modify $ \st -> st{dailabel = Map.insert anc (Labels trp pr ps hp (Set.insert d dir)) current_dailabel}
            if anc == trp then return ()
            else addDirectinAncestors trp d
        _ -> error "invalid"


addDirectsandAncestors :: Nd -> Set Nd -> MKDAILabel ()
addDirectsandAncestors nd setdirs = do
    current_dailabel <- gets dailabel
    let label = Map.lookup nd current_dailabel
    case label of 
        Just (Labels trp pr ps hp dir) -> do
            modify $ \st -> st{dailabel = Map.insert nd (Labels trp pr ps hp (Set.union dir setdirs)) current_dailabel}
            if nd == trp then return ()
            else addDirectsandAncestors trp setdirs
        _ -> error "invalid"

deleteDirectsandAncestors :: Nd -> Nd -> MKDAILabel ()
deleteDirectsandAncestors nd1 nd2 = do
    current_dailabel <- gets dailabel
    let label = Map.lookup nd1 current_dailabel
    -- tell[(show nd1 ++ " directs deletse :  " ++ show nd2)]
    case label of 
        Just (Labels trp pr ps hp dir) -> do
                    (modify $ \st -> st { dailabel = Map.insert nd1 (Labels trp pr ps hp (Set.delete nd2 dir)) current_dailabel } )
{-                                            hopnodes = Set.insert nd chops,
                                           specialnodes = Set.insert parent current_specialnodes, 
                                           treeEdges = Set.insert (parent, nd) current_nonTreeEdges -}      
                    if nd1 == trp then return ()
                    else deleteDirectsandAncestors trp nd2
        Nothing -> error "error "



isTheOnlyNonTreeEdge ::Nd -> Nd -> MKDAILabel Bool
isTheOnlyNonTreeEdge nd1 nd2 = do
    current_nonTreeEdges <- gets nonTreeEdges
    case (elem (nd1, nd2)  current_nonTreeEdges) of
        True -> if (countOfNonTreeEdges (Set.toList current_nonTreeEdges) nd1) == 1 then return True
                else return False
        False -> error " invalid entry "

countOfNonTreeEdges :: [(Nd,Nd)] -> Nd -> Int
countOfNonTreeEdges [] _ = 0
countOfNonTreeEdges ((x,y):rest) ndd = if x == ndd then (1 + countOfNonTreeEdges rest ndd)
                                   else countOfNonTreeEdges rest ndd

isTreeEdge ::Nd -> Nd -> MKDAILabel Bool
isTreeEdge nd1 nd2 = do
    current_treeEdges <- gets treeEdges
    return $ elem (nd1, nd2)  current_treeEdges



isProcessed :: Nd -> MKDAILabel ()
isProcessed nd = do
    current_dailabel <- gets dailabel
    let label = Map.lookup nd current_dailabel
{-     tell[" from is processed : " ++ show nd ++ " : "]
    tell[(show label)]
 -} case label of 
        Nothing -> processNodes nd nd
        _       -> return ()



process :: MKDAILabel ()
process =  do
    (processNodes (Nd 'a') (Nd 'a') ) 
    inp <- ask
{-     let graph = Map.toList inp
    mapM_ isProcessed [ x | (x,y) <- graph] -}
    current_dailabel <- gets dailabel
    tell [(show current_dailabel)]
{-     parentnodes <- gets parentNodes
    tell [(show parentnodes)]
    flag <- search (Nd 'd') (Nd 'a')
    tell [(show flag)]
    tell [" tree edges ---------"]
    current_treeEdges <- gets treeEdges
    tell [(show current_treeEdges)]
    tell [" non    tree edges ---------"]

    current_nonTreeEdges <- gets nonTreeEdges
    tell [(show current_nonTreeEdges)]
    current_hops <- gets hopnodes
    tell [(show current_hops)]
    current_specialnodes <- gets specialnodes
    tell [(show current_specialnodes)]
 -}

processNodes :: Nd -> Nd -> MKDAILabel()
processNodes nd parent = do
    inp <- ask
    x <- insertNodeinState nd parent
    unless x $ do
        -- tell [(show x)]
        let fun = Map.lookup nd inp
        case fun of
            Nothing       -> return ()
            Just []       -> return () 
            Just rest -> mapM_ (\x -> processNodes x nd) rest 
        updatePost nd
        updateMax 



relabel :: Input -> Nd -> [Nd] -> MKDAILabel [Nd]
relabel input nd visited = do
    x <- updatePre nd visited
    if x then return visited
         else do
            let fun = Map.lookup nd input
            nv <- 
               case fun of
                    Nothing       -> return visited
                    Just []       -> return visited 
                    Just rest -> foldM (\acc y -> relabel input y acc) visited  rest 
            updatePost nd
            updateMax 
            return (nd : nv)


updatePre :: Nd -> [Nd] -> MKDAILabel Bool
updatePre nd visited = do 
    current_dailabel <- gets dailabel
    current_counter <- gets counter
    let label = Map.lookup nd current_dailabel
    -- tell ["from update pre-----------"]
    case label of
        Just (Labels trp pr ps hp dir) -> if pr == ps || elem nd visited then return True 
                                          else do 
                            modify $ \st-> st {dailabel = Map.insert nd (Labels trp current_counter current_counter hp dir) current_dailabel,
                                                            counter = current_counter+1 }
                            tell[( show nd ++  " : " ++ show current_counter ) ]
                            return False
        Nothing -> error "error"

updatePost :: Nd ->MKDAILabel()
updatePost nd = do 
    current_dailabel <- gets dailabel
    current_counter <- gets counter
    let label = Map.lookup nd current_dailabel
    case label of
            Just (Labels trp pr ps hp dir) -> modify $ \st-> st {dailabel = Map.insert nd (Labels trp pr current_counter hp dir) current_dailabel,
                                                            counter = current_counter+1 }
            Nothing -> error "error"

updateMax :: MKDAILabel ()
updateMax = do 
    current_counter <- gets counter
    maxID <- gets maxID
    modify $ \st -> st { maxID = current_counter}

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