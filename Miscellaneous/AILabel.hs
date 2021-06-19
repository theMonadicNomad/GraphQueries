import Control.Monad
import Control.Monad.Reader
import Control.Monad.State as CMS
import Control.Monad.RWS
import Data.List (intercalate)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


type MKAILabel = RWS Input Output St

newtype Nd = Nd Char
  deriving (Eq, Ord, Show, Read)

type Output = [String]
type Input = Map Nd [Nd]
type Hops = Set Nd
type Directs = Set Nd
type Pre = Int
type Post = Int

data Labels = Labels {
    pre :: Pre,
    post :: Post,
    hops :: Hops,
    directs :: Directs
} deriving Show

data St = St {
    ailabel      :: Map Nd Labels,
    parentNodes  :: Map Nd Nd,
    counter      :: Int,
    hopnodes     :: [Nd],
    specialnodes :: [Nd]{- ,
    nonTreeEdges :: (Nd, Nd) -}
} deriving Show


initSt :: St
initSt = St {
    ailabel     = Map.empty,
    parentNodes = Map.empty,
    counter     = 0,
    hopnodes        = [],
    specialnodes    = []{- ,
    nonTreeEdges -}
    }

graph1 :: [(Nd, [Nd])]
graph1 = 
    [ (Nd 'a', [ Nd 'b', Nd 'c'] ),
      (Nd 'b', [ Nd 'c'] )
    ]

graph2 :: [(Nd, [Nd])]
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

process :: MKAILabel ()
process =  do
    (processNodes (Nd 'a') (Nd 'a') ) 
    cailabel <- gets ailabel
    tell [(show cailabel)]
    parentnodes <- gets parentNodes
    tell [(show parentnodes)]

updatePost :: Nd ->MKAILabel()
updatePost nd = do 
    cailabel <- gets ailabel
    ccounter <- gets counter
    let label = Map.lookup nd cailabel
    case label of
            Just (Labels pr ps hp dir) -> modify $ \st-> st {ailabel = Map.insert nd (Labels pr ccounter hp dir) cailabel,
                                                            counter = ccounter+1 }
            Nothing -> error "error"

processNodes :: Nd -> Nd -> MKAILabel()
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

insertNodeinState :: Nd -> Nd -> MKAILabel Bool
insertNodeinState nd parent = do
    cailabel <- gets ailabel
    ccounter <- gets counter
    parentnodes <- gets parentNodes
    chops <- gets hopnodes
   -- tell [(show nd ++ " : "++ show cailabel ++ " \\\n")]
    let label = Map.lookup nd cailabel
    case label of 
        Nothing -> (modify $ \st -> st { ailabel = Map.insert nd (Labels ccounter ccounter Set.empty Set.empty) cailabel,
                                         parentNodes    = Map.insert nd parent parentnodes, 
                                        counter = ccounter+1 } ) >> pure False
        _       -> do
            let label = Map.lookup parent cailabel
            case label of 
                Just (Labels pr ps hp dir) -> do
                     (modify $ \st -> st { ailabel = Map.insert parent (Labels pr ps (Set.insert nd hp) dir) cailabel,
                                              hopnodes = nd:chops   } )
                     let grandparent = Map.lookup parent parentnodes
                     case grandparent of
                         Just gp -> updateDirects parent gp
                         Nothing -> error "error grandparent"
                Nothing -> error "error "
            pure True



updateDirects :: Nd -> Nd -> MKAILabel()
updateDirects parent gp = do
    cailabel <- gets ailabel
    let glabel = Map.lookup gp cailabel
    case glabel of 
        Just (Labels ppr pps php pdir) -> modify $ \st -> st { ailabel = Map.insert gp (Labels ppr pps php (Set.insert parent pdir) ) cailabel}
        Nothing -> error "error again "
    parentnodes <- gets parentNodes
    let Just ggp = Map.lookup gp parentnodes
    if (ggp == gp) then return()
    else updateDirects parent ggp