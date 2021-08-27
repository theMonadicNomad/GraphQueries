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
  putStrLn (" Enter the density : ")
  inp_2 <- getLine
  putStrLn ("a for AILabel")
  putStrLn ("d for DAILabel")
  putStrLn ("m for DAILabelModified")
  putStrLn("b for Benchmarking")
  putStrLn ("Enter your choice : ")
  process_char <- getChar
  let n = (read inp_1 :: Int64)
  let d = (read inp_2 :: Double)
  case process_char of
    'a' -> AILabel.main1 n d
    'd' -> DAILabel.main1 n d
    'm' -> DAILabelModified.main1 n d
    _   -> putStrLn("Try again ") >> main
    
  let Graph g1 = generateGraph n d
  --print $ show g1 
  --let process_char = 'd'
  db <- openDB AILabel.databaseTest
  (a,b)  <- runDaison db ReadWriteMode $ do
{-     tryCreateTable graph1Table
    tryCreateTable counters
    tryCreateTable nodeMapTable
--    insert counters (return ( "l_max", max_bound ))
    insert counters (return ( "counter", 0 )) -}
--    let Graph g = AILabel.graph2
    let graphmap1 =  Map.fromList g1
--    AILabel.process graphmap1
{-     if (process_char == 'd') then dynamicProcess graphmap1 else staticProcess graphmap1
    a <- select [ x | x <- from graph1Table everything ]
    b <- select [ x | x <- from nodeMapTable everything ] -}
    return ([],[])
  putStrLn "FROM MAIN"
{-   mapM_ (\y -> putStrLn (show y) ) a
  mapM_ (\y -> putStrLn (show y) ) b
 -}  -- closeDB db
{-   putStrLn (" Enter the first node : ")
  firstChar <- getLine
  putStrLn (" Enter the second node : ")
  secondChar <- getLine
  let f = I (read firstChar :: Int64)
  let s = I (read secondChar :: Int64)

  defaultMain [ bgroup "searchs" [
    bench "depthfirst search " $ nfIO  (bm_dfsearch f s db ReadWriteMode),
    bench "labelsearch " $ nfIO  (bm_search f s db ReadWriteMode) ]]
 -}  
  --makeDynamicOperation databaseTest ReadWriteMode
  closeDB db

{- bm_dfsearch firstChar secondChar db readwritemode = do 
    -- db <- openDB test_db  
    (a) <- runDaison db readwritemode $ do 
      nd1 <- getNdIndex firstChar
      nd2 <- getNdIndex secondChar
      flag <- dfsearch nd1 nd2
      liftIO $ print  $ " search result df : " ++ show flag 
      x <- return flag 
      return (x)
   -- closeDB db
    return()

bm_search firstChar secondChar db readwritemode = do 
   -- db <- openDB test_db  
    (a) <- runDaison db readwritemode $ do 
      nd1 <- getNdIndex ( firstChar)
      nd2 <- getNdIndex ( secondChar)
      flag <- search nd1 nd2
      liftIO $ print  $ " search result normal rsult : " ++ show flag 
      x <- return flag 
      return (x)
    -- closeDB db
    return()
 -}

