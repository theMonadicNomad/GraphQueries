{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import Criterion.Main
import System.Random
import           Database.Daison
import           System.Environment
import           Data.Data
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad
import Control.Monad.IO.Class
import qualified System.IO as IO
import Test.QuickCheck 
import Test.QuickCheck.Monadic
import Data.Maybe (fromJust)
import System.Process (callProcess, callCommand)
import qualified Data.Bits as Bits
import Data.Int
import qualified AILabel as AILabel
import qualified DAILabel as DAILabel
import qualified DAILabelModified as DAILabelModified


type Nd = Key AILabel.Labels
--  deriving (Eq, Ord, Read, Data)
type Ndc = Char
type Nds = [Char]

data Graph a = Graph [(a, [a])] 
  deriving Show


type GraphMap a = Map a [a]
data Node = C Ndc | I Nd | S Nds deriving (Eq,Data, Ord, Show)



generateGraph :: Int64 -> Double ->Graph Node
generateGraph n p =  Graph $ map (\x -> (I x,restList x )) {- list@( -}[1..n]
    where 
        restList x= map I $ List.sort $ List.nub (take  (floor (p * fromIntegral (n-x))) $ randomRs (x+1,n) (mkStdGen 3) :: [Int64]  )


main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  putStrLn ("Enter the number of nodes : ")
  inp_1 <- getLine
  putStrLn (" Enter the maximum number of tree edges a node can have : ")
  inp_2 <- getLine
  putStrLn (" Enter the maximum number of non-tree edges a node can have : ")
  inp_3 <- getLine
  putStrLn ("a for AILabel")
  putStrLn ("d for DAILabel")
  putStrLn ("m for DAILabelModified")
  putStrLn ("Enter your choice : ")
  process_char <- getChar
  let n = (read inp_1 :: Int64)
  let d = (read inp_2 :: Int64)
  let p = (read inp_3 :: Int64)
  case process_char of
    'a' -> AILabel.main1 n d p
    'd' -> DAILabel.main1 n d p
    'm' -> DAILabelModified.main1 n d p
    _   -> putStrLn("Try again ") >> main