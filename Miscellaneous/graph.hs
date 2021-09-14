{-# LANGUAGE DeriveDataTypeable #-}

import System.Random
import Data.Data
import Data.Int
import Data.List
import Data.Maybe
import Control.Monad

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
    return (gup, vs ++ [(I x, vsup)])) (gen, []) {- list@( -}[1..n]
  return $ Graph g
    where
        restList genV x = do
          (vs, ugen) <- ranValues genV (fromEnum $ (n-x)) 0.0 1.0
          let vs' =
                zipWith (\v i -> if v <= p
                              then Just i
                              else Nothing
                  )
                  vs
                  [(x+1)..(n)]
          return (ugen, I <$> catMaybes vs')
--          map I $ sort $ nub (take  (floor (p * fromIntegral (n-x))) $ randomRs (x+1,n) (mkStdGen 3) :: [Int64]  )

ranValues gen n a b = do
  let values = take n (randomRs (a,b) gen )
  putStrLn $ show (values)
  let (_,newGen) = random gen :: (Double, StdGen)
  return (values, newGen)