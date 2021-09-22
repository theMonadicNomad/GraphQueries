{-# LANGUAGE DeriveDataTypeable #-}

import System.Random
import Data.Data
import Data.Int
import Data.List
import Data.Maybe
import Control.Monad
import System.Random.SplitMix
type Nd = Int64
--  deriving (Eq, Ord, Read, Data)
type Ndc = Char
type Nds = [Char]

data Graph a = Graph [(a, [a])] 
  deriving Show

generateGraph :: Int64 -> Double ->Graph Node
generateGraph n p =  Graph $ map (\x -> (I x,restList x )) {- list@( -}[1..n]
    where 
        restList x= map I $ sort $ nub (take  (floor (p * fromIntegral (n-x))) $ randomRs (x+1,n) (mkStdGen 3) :: [Int64]  )


data Node = C Ndc | I Nd | S Nds deriving (Eq,Data, Ord, Show)


--ranValues :: StdGen -> Int -> IO ()
--ranValues ::  Int -> Double -> Double -> IO ()
{- ranValues n a b = do 
  gen <- getStdGen
  let values = take n (randomRs (a,b) gen )
  putStrLn $ show (values)
  let (_,newGen) = random gen :: (Double, StdGen)
  putStrLn $ show (newGen ) -}

--a, b



{- generateGraph :: Int64 -> Double ->Graph Node
generateGraph n p =  Graph $ map (\x -> (I x,restList x )) {- list@( -}[1..n]
    where 
        restList x= map I $ sort $ nub (take  (floor (p * fromIntegral (n-x))) $ randomRs (x+1,n) (mkStdGen 3) :: [Int64]  )
 -}

{- genGraph :: Int64 -> Double -> IO Graph Node
genGraph n p = do
  gen <- getStdGen
  let Graph $ foldM (\fgen i -> do ) -}

  
  
  
  
  {- map (\x -> (I x,restList x )) {- list@( -}[1..n]
   where 
        restList x= map I $ List.sort $ List.nub (take  (floor (p * fromIntegral (n-x))) $ randomRs (x+1,n) (mkStdGen 3) :: [Int64]  )
 -}




generateGraph1 :: Int64 -> Double -> IO (Graph Node)
generateGraph1 n p = do
  gen <- getStdGen
  (_, g) <- foldM (\(genV, vs) x -> do
        (gup, vsup) <- restList genV x
        return (gup, vs ++ [(I x, vsup)])) 
    (gen, []) 
    [1..n]
  return $ Graph g
    where
        restList genV x = do
          (vs, ugen) <- ranValues genV ( fromEnum $  (n-x)) 0.0 1.0
          let vs' =
                zipWith (\v i -> if v <= p
                              then Just i
                              else Nothing
                  )
                  vs
                  [(x+1)..(n)]
          return (ugen, I <$> catMaybes vs')

ranValues gen n a b = do
  let values = take n (randomRs (a,b) gen )
  putStrLn $ show (values)
  let (_,newGen) = random gen :: (Double, StdGen)
  putStrLn $ show (newGen ) 
  return (values, newGen)


--take 8  $ unfoldr (Just . nextFloat) (mkSMGen 1337) :: [Float]


  --ranValues :: StdGen -> Int -> IO ()
ranValues1 ::  Int -> Double -> Double -> IO ()
ranValues1 n a b = do 
  gen <- getStdGen
  let values = take n (randomRs (a,b) gen )
  putStrLn $ show (values)
  let (_,newGen) = random gen :: (Double, StdGen)
  putStrLn $ show (newGen ) 

generateGraph12 :: Int64 -> Float -> IO (Graph Node)
generateGraph12 n p = do
  gen <- initSMGen
  (_, g) <- foldM (\(genV, vs) x -> do
        (gup, vsup) <- restList genV x
        return (gup, vs ++ [(I x, vsup)])) 
    (gen, []) 
    [1..n]
  return $ Graph g
    where
        restList genV x = do
          (vs, ugen) <- ranValues12 genV ( fromEnum $  (n-x)) --0.0 1.0
          let vs' =
                zipWith (\v i -> if v <= p
                              then Just i
                              else Nothing
                  )
                  vs
                  [(x+1)..(n)]
          return (ugen, I <$> catMaybes vs')




--ranValues12 ::  Int ->  IO ()
ranValues12  gen n  = do 
--  gen <- initSMGen
  let values = take n  $ unfoldr (Just . nextFloat) (mkSMGen 1337) :: [Float]--take n (randomRs (a,b) gen )
  putStrLn $ show (values)
  let (_,newGen) = nextFloat gen :: (Float, SMGen)
  putStrLn $ show (newGen ) 
  return (values, newGen)


generateTreeGraph :: Int64 -> Int64 -> IO (Graph Node)
generateTreeGraph  total n  = do
  if total == 0
    then return $ Graph []
    else do
      let rt = 1 
      (lrt , ch) <- genChild (total,  rt + 1 , n) rt
      let rest = (\x -> (I x, [])) <$> ([lrt+1 .. total])
      final <- mapM (addMoreEdges total n) (ch ++ rest)
      return $ Graph final

addMoreEdges :: Int64 -> Int64 -> (Node, [Node]) -> IO (Node, [Node])
addMoreEdges total n (I x, []) = if x == total 
    then return (I x , [])
    else do
      gen <- newStdGen
      let (nv,gen') = randomR (0,n) gen
          ls = take (fromEnum nv) (randomRs (x+1,total) gen') 
      return (I x, I <$> (sort $ nub ls))
addMoreEdges total n (I x, ls) = do
  gen <- newStdGen
  let I ns = last ls
      (nv,gen') = randomR (0,n) gen
      ls' = if (ns==total) then [] else take (fromEnum nv) (randomRs (ns+1,total) gen') 
{-   ls' <- mapM (\_ -> do
    gn <- newStdGen
    return $ fst (randomR (ns + 1, total) gn)) [1..(fst $ randomR (0, n) gen)] -}
  return (I x, ls ++ (I <$> (sort $ nub ls')))

genChild :: (Int64, Int64, Int64) -> Int64 -> IO (Int64, [(Node, [Node])])
genChild (total, curr, n) rt = do
  children <- generateChildren n curr total
  let curr' = if null children && curr - 1 == rt
                then curr + 1
                else curr + (fromIntegral (length children ))
  (rt',ch) <-
    if total <= curr'
      then return (rt, [])
      else do
        genChild (total, curr', n) (rt+1) 
  return (rt', (I rt,children) : ch)

generateChildren :: Int64 -> Int64 -> Int64-> IO [Node]
generateChildren n c total =   do
  gen <- newStdGen
  let values = fst $ (randomR (1,n) gen )
  -- putStrLn $ show (values)
--  return (I <$> [c .. ( if (c + values - 1)> total then total else (c+values-1)) ] )
  return (I <$> [c .. (mod (c+ values-1) total) ] )
