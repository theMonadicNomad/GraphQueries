data Node a = Node {
    id :: a,
    adjacent :: [Node a]
}

data Graph a = Graph [Node a]

data AILabel a = AILabel {
    pre :: Int,
    post :: Int,
    hops :: [Node a],
    directs :: [Node a] 
}

newtype IntervalLabel a = IntervalLabel (Node a)
