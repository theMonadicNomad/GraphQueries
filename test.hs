{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Data.List (nub,subsequences,uncons,tails)
import Data.Maybe (fromJust)
import Data.Char (isLetter)


data Graph a = Graph [(a, [a])]
    deriving Show


instance Arbitrary (Graph Int) where
  arbitrary = do 
    x <- arbitrary :: Gen Int
    ((nub . filter (>= 0)) <$> listOf1 arbitrary) >>= helper

instance Arbitrary (Graph Char) where
  arbitrary = do 
    x <- arbitrary :: Gen Char
    ((nub . filter isAlphabet) <$> listOf1 arbitrary) >>= helper
    

helper :: (Eq a, Arbitrary a) => [a] -> Gen (Graph a)
helper ls = do
  let tls = tails ls
      l = length tls - 1
      t2ls = take l tls
      gs = map (fromJust . uncons) t2ls

  Graph <$> mapM (\(y,ks) -> do
                            js <- elements . subsequences $ ks
                            return (y, js)
                 ) gs

isAlphabet :: Char -> Bool
isAlphabet ch = if ch `elem` ['a'..'z'] || ch `elem` ['A'..'Z']
		then True 
		else False