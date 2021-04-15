import Control.Monad
import Control.Monad.Reader
import Control.Monad.State as CMS
import Control.Monad.RWS
import Data.List (intercalate)

import Data.Map (Map)
import qualified Data.Map as Map


type MKAILabel = RWS Input Output St

newtype Nd = Nd Char
  deriving (Eq, Ord, Show, Read)

type Output = [String]
type Input = Map Nd [Nd]
type Hops = [Nd]
type Directs = [Nd]
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
    appil        :: Map Nd Nd,
    counter      :: Int,
    hopnodes     :: [Nd],
    specialnodes :: [Nd]{- ,
    nonTreeEdges :: (Nd, Nd) -}
} deriving Show


initSt :: St
initSt = St {
    ailabel     = Map.empty,
    appil       = Map.empty,
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


{- process :: [(Nd,[Nd])] -> IO()
process adjacencylist = 
 -}    
main = do
    let input = Map.fromList graph2
    let ((), w) = evalRWS (processNodes (Nd 'a') (Nd 'a') ) input initSt
    print w

process :: MKAILabel ()
process =  do 
    inp <- ask
    let fun = Map.lookup (Nd 'c') inp
    tell [("helloe " ++  show fun )]


processNodes :: Nd -> Nd -> MKAILabel()
processNodes nd parent = do
    inp <- ask
    insertNodeinState nd parent
    let fun = Map.lookup nd inp
    case fun of
        Nothing       -> tell [("nothing here " )]
        Just []       -> tell [("nothing lso " )]
        Just [x]      -> processNodes x nd
        Just (y : rest) -> processNodes y nd >> mapM_ (\x -> processNodes x nd) rest
    tell [[]]


insertNodeinState :: Nd -> Nd -> MKAILabel ()
insertNodeinState nd parent = do
    cailabel <- gets ailabel
    ccounter <- gets counter
    chops <- gets hopnodes
    let label = Map.lookup nd cailabel
    case label of 
        Nothing -> (modify $ \st -> st { ailabel = Map.insert nd (Labels ccounter ccounter [] []) cailabel,
                                        counter = ccounter+1
         } )-- >> tell [ ( "counter updated  :" ++ show (ccounter+1)) ]
        _       -> (modify $ \st -> st { hopnodes = nd:chops   } )-- >> tell [ ( "Hope node inserted :" ++ show nd) ]
    tell [(show cailabel)]

lookUpNdinState :: Nd -> MKAILabel ()
lookUpNdinState nd = do
    cailabel <- gets ailabel
    let label = Map.lookup nd cailabel
    case label of
        Nothing -> tell [("Nothing" )]
        _       -> tell [(show label)]   
    --return (Just cailabel)