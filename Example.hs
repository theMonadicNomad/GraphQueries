
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveDataTypeable #-}

import           Database.Daison
import           System.Environment
import           Data.Data

type Distance = Int

data City = City { name :: String,
 id :: Int,
 population :: Int,
 paths :: [(Key City, Distance)] 
} deriving (Typeable, Data )
  

instance Show City where 
  show (City a b c d) = a ++ " - " ++ show b

cities :: Table City
cities = withIndex (table "cities") cityPaths

cityPaths :: Index City [(Key City, Distance)]
cityPaths = index cities "paths" paths


main :: IO ()
main = do
  db <- openDB "test1.db"
  x  <- runDaison db ReadWriteMode $ do
         --dropTable people 
    tryCreateTable cities
    insert cities (return ( City "Gothenburg" 1 15000 [ (2, 10)
                                                      , (3 , 5)
                                                      ]))
    insert cities (return ( City "Malmo" 2 1005 []))
    --update people2 (\_ (name,age,x) -> (name,age,10))
      --             (from people2)
    select [ x | x <- from cities everything ]
  print x
  closeDB db

