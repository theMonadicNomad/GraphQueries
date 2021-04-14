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
}

data St = St {
    ailabel      :: Map Nd Labels,
    appil        :: Map Nd Nd,
    hopnodes     :: [Nd],
    specialnodes :: [Nd]{- ,
    nonTreeEdges :: (Nd, Nd) -}
}


initSt :: St
initSt = St {
    ailabel     = Map.empty,
    appil       = Map.empty,
    hopnodes        = [],
    specialnodes    = []{- ,
    nonTreeEdges -}
    }

graph1 :: [(Nd, [Nd])]
graph1 = 
    [ (Nd 'a', [ Nd 'b', Nd 'c'] ),
      (Nd 'b', [] )
    ]

{- process :: [(Nd,[Nd])] -> IO()
process adjacencylist = 
 -}    
main = do
    let input = Map.fromList graph1
    let ((), w) = evalRWS process input initSt
    print w

process :: MKAILabel ()
process =  do 
    inp <- ask
    let Just fun = Map.lookup (Nd 'a') inp
    tell [("helloe " ++  show fun )]