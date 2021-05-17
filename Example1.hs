{-# LANGUAGE MonadComprehensions #-}
import           Database.Daison
import           System.Environment

people_name :: Index (String, Int) String
people_name = index people "name" fst

people :: Table (String, Int)
people = table "people" `withIndex` people_name


-- (name, id)
-- create a index on name
-- [ ("namex", (\(na, id) -> [na]))
-- ]
main = do
  db <- openDB "test.db"
  let z = show' intShow'
  let y = (show intShow) 5
  let naga = runDaison
  x  <- naga db ReadWriteMode $ do
         --dropTable people
    tryCreateTable people 
    -- Daison { doTransaction = (\db -> sqliteCreate db people)}
    insert people (return ("Aga", 15))
    insert people (return ("Aga", 15))
    --update people2 (\_ (name,age,x) -> (name,age,10))
      --             (from people2)
    select [ x | x <- from people everything ]
  print x
  closeDB db

newtype Show a = Show { show :: a -> String}
newtype S a = S {show' :: a}

intShow :: Show Int
intShow = Show { show = (\x -> Prelude.show x)}


deleteX  :: Daison String
deleteX = Daison { doTransaction= (\db -> callAPi "google.com")}


data DaisonF =
  Create a a
  | Update a b a
  | Delete a ()

intShow' :: S Int
intShow' = S { show' = 5}
