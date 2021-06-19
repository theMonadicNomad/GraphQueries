import Test.QuickCheck 



instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where 
  arbitrary = do 
    ns <- Set.fromList <$> liftA2 (++) (replicateM 10 arbitrary) arbitrary

let ns' = map reverse $ drop 2 $ inits $ Set.toList ns 

es <- sublistOf ns' >>= mapM (\(f:ts) -> Edge f <$> elements ts)
return $ Graph ns (Set.fromList es) 

main = quickCheck $ forAll arbitrary (liftA2 (&&) (isDAG :: Graph Integer -> Bool) isForest)