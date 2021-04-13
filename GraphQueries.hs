{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
import Prelude hiding (id)
data Node a = Node {
    id :: a,
    adjacent :: [a]
} deriving Show

{- instance Show (Node a) where 
    show (Node x y) = " -- " ++ show x ++ " -- "
 -}
data Graph a = Graph [Node a]

data AILabel a = AILabel {
    id  :: a,
    pre :: Int,
    post :: Int,
    hops :: [a],
    directs :: [a] 
}


data Parent a = NoParent | Parent a

data IntervalLabel a = IntervalLabel {
    id   :: a,
    parent :: Parent a
}

charGraph :: Graph Char
charGraph = Graph [ Node 'A' ['B']
                  , Node 'B' []
                  ]
-- record-dotprocessor 
-- > findPreAndPost charGraph
--    [(A, 1, 4), (B, 2, 3)]
--
findPreAndPost :: forall a. Graph a -> [(a, Int, Int)]
findPreAndPost (Graph graph) = [] --[(id (head graph :: Node a), 0, 0)]

findHops :: Graph a -> [(a, [a])]
findHops graph = undefined


main = do
 print $ findPreAndPost charGraph