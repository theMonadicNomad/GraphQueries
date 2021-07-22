{-# LANGUAGE DeriveDataTypeable #-}

import System.Random
import Data.Data
import Data.Int
import Data.List

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
