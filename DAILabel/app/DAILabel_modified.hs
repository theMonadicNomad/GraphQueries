module DAILabel_modified where 


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
import Common






dynamicProcess :: GraphMap Node -> Daison ()
dynamicProcess graphmap = do
  let firstnode = fst $ Map.elemAt 0 graphmap
  store nodeMapTable (Just 0) (X (S "root" ) [])
  store graph1Table (Just 0) (Labels 0 0 max_bound Set.empty Set.empty (-100) (-100) (-100) (-100) )    
  processNodes graphmap firstnode (S "root")
  processRemainingNodes graphmap

processRemainingNodes :: GraphMap Node -> Daison ()
processRemainingNodes graphmap = do 
  nodes <- select [nd | (ind, ( X nd nodeindex )) <- from nodeMapTable everything  ]
  let graphlist = Map.toList graphmap
  let nodelist = map (\(x,y) -> x ) graphlist
  let difference = nodelist List.\\ nodes
  liftIO $ print $ " nodes :  " ++ show nodes
  liftIO $ print $ " difference : " ++ show difference
  liftIO $ print $ " nodelist : " ++ show nodelist
  when (length difference > 0 ) $ do 
    processNodes graphmap (head difference) (S "root")
    processRemainingNodes graphmap
  return()



processNodes :: GraphMap Node -> Node -> Node -> Daison()
processNodes graph node parent_node = do
  nd <- getNdIndex node
  parent <- getNdIndex parent_node
  par <-  query firstRow (from nodeMapTable (at parent))
  case par of
    (X pnd edges) -> case (List.elem nd edges ) of
      True -> return ()
      False -> handleInsert  parent nd   
  let adjacent = Map.lookup node graph
  case adjacent of
      Nothing      -> return () 
      Just []   -> return ()
      Just rest    ->do
        mapM_ (\x -> processNodes graph x node ) rest


handleInsert :: Nd -> Nd -> Daison ()
handleInsert nd1 nd2 = do
  liftIO $ print  $ " nd1 : " ++ show nd1 ++ "  nd2 : " ++ show nd2

  res1 <- select (from graph1Table (at nd1))
  res2 <- select (from graph1Table (at nd2))
  
  case (res1,res2) of

    ([record1@(Labels tp1 _ _ hp1 _ fc1 lc1 _ _ )],[record2@(Labels tp2 _ _ hp2 dir2 _ _ ns2 ls2 )])   -- Case 1
          | tp2 == 0 -> do                                                                       -- Case 1.1
              if Set.null hp1
                then updateDirectInAncestors nd1 record1 (Set.union dir2)                        -- Case 1.1.1
                else updateDirectInAncestors nd1 record1 (Set.insert nd2)                        -- Case 1.1.2
              update_ graph1Table (return (nd2, record2{tree_parent=nd1{- , nextSibling=fc1 -}}))
              --liftIO $ print $ " nd :" ++ show nd1 ++ " edges : " ++ show record1
              --liftIO $ print $ " nd :" ++ show nd2 ++ " edges : " ++ show record2
                                   

              if (lc1 <0) then 
                update_ graph1Table (return (nd1, record1{firstChild = nd2, lastChild= nd2}))>>
                updateLabel record1 nd2 record2 (-1)
                else 
                  do update_ graph1Table (return (nd1, record1{lastChild= nd2}))
                     record3 <- query firstRow (from graph1Table (at lc1))
                     update_ graph1Table (return (lc1, record3{nextSibling = nd2}))
                     update_ graph1Table (return (nd2, record2{lastSibling = lc1}))
                     updateLabel record1 lc1 record3 nd2
              when(ls2>0) $
                do 
                  record4 <- query firstRow (from graph1Table (at ls2))
                  update_ graph1Table (return (ls2, record4{nextSibling = ns2}))
                  --liftIO $ print $ " nd :" ++ show ls2 ++ " edges : " ++ show record4 
                  return()
              {- TODO: - update the pre and post labels of nd2 and its children.
                          All the labels must be moved right after PreLabel nd1.
                       - update prevSibling for the firstChild of nd1
                       - update firstChild for nd1 -}
          | otherwise -> do                                                                      -- Case 1.2
              if not (Set.null hp1)
                then addHop nd1 record1 nd2                                                      -- Case 1.2.1
                else do record <- query firstRow (from graph1Table (at tp1))                     -- Case 1.2.2
                        updateDirectInAncestors tp1 record (Set.insert nd1)
                        addHop nd1 record1 nd2
--              liftIO $ print $ " nd1 :" ++ show nd1 ++ " " ++ show record1
--              liftIO $ print $ " nd2 :" ++ show nd2 ++ " " ++ show record2 
              return ()

    ([record1@(Labels tp1 pr1 ps1 _ _ fc1 lc1 _ _ )],[]                                          ) ->  -- Case 2
          do --when ((ps1 - pr1) <=2 ) $ reLabelMain (PreLabel nd1)
--             liftIO $ print $ " nd1 : " ++ show nd1 ++ " nd2 : " ++ show nd2++ " pr :" ++ show pr1 ++ " ps : " ++ show ps1 

             (pre,post,ns2,ls2)  <- if ((ps1 - pr1) <=2 ) 

               then
                 do
                   reLabelMain (PreLabel nd1)
                   a  <- prevOf (PostLabel nd1) >>= \x -> getLabel x 
                   ps1 <- getLabel (PostLabel nd1)
--                   liftIO $ print $ " FROM IF a : " ++ show a ++ " ps1 : " ++ show ps1

                   let pr = average a ps1
                   let ps = average pr ps1
                   (new_fc1, new_lc1, ns2, ls2) <- if lc1 < 0 
                     then return (nd2,nd2,(-1),(-1)) 
                     else do res3 <- query firstRow (from graph1Table (at lc1))
                             case res3 of 
                              record3@(Labels _ _ _ _ _ _  _ ns _) -> store graph1Table (Just lc1) record3{nextSibling = nd2}
                             return (fc1, nd2,(-1), lc1 )
                   newRes1 <- query firstRow (from graph1Table (at nd1))
                   case newRes1 of 
                    newRecord1@(Labels _ _ _ _ _ _  _ _ _) ->store graph1Table (Just nd1) newRecord1{firstChild = new_fc1,lastChild = new_lc1}
                   return (pr,ps,ns2,ls2)

               else do 
                 a <- prevOf (PostLabel nd1) >>= \x -> getLabel x 
                 let pr = average a ps1
                 let ps = average pr ps1
--                 liftIO $ print $ " FROM ELSE a : " ++ show a ++ " ps1 : " ++ show ps1
                 (new_fc1, new_lc1, ns2, ls2) <- if lc1 < 0 
                   then return (nd2,nd2,(-1),(-1)) 
                   else do res3 <- query firstRow (from graph1Table (at lc1))
                           case res3 of 
                            record3@(Labels _ _ _ _ _ _  _ ns _) -> store graph1Table (Just lc1) record3{nextSibling = nd2}
                           return (fc1, nd2,(-1), lc1 )
            
                 store graph1Table (Just nd1) record1{firstChild = new_fc1,lastChild = new_lc1}

                 return (pr,ps,ns2,ls2)


             store graph1Table (Just nd2) (Labels nd1 pre post Set.empty Set.empty (-1) (-1) ns2 {- fc1 -} ls2)
             {- TODO: - update prevSibling for the firstChild of nd1 ??
                      - update firstChild for nd1 -}

             return ()

    ([]                                       ,[record2@(Labels tp2 _ _ hp2 dir2 _ _ ns2 ps2 )])     -- Case 3
          | tp2 > 0   -> do                                                                      -- Case 3.1
              (pre,post,prevSib)  <- insertIsolatedNode nd1 {- TODO: insert two labels after (PreLabel root), root == 0 -}
              store graph1Table (Just nd1) (Labels 0 pre post (Set.singleton nd2) Set.empty (-1) (-1) (-1) prevSib)
              return ()
          | otherwise -> do                                                                      -- Case 3.2
              (pre,post)  <- do
                a <- prevOf (PreLabel nd2) >>= \x -> getLabel x 
                b <-nextOf (PostLabel nd2) >>= \x -> getLabel x
                prend2 <- getLabel (PreLabel nd2)
                posnd2 <- getLabel (PostLabel nd2)
                let pre = average prend2 a 
                let post = average posnd2 b --update siblings and children of root
                return (pre,post) {- TODO: insert one label after (prevOf (PreLabel nd2)) -}
              {-post <- undefined  TODO: insert one label after (PostLabel nd2) -}
              when (ns2 >0 ) $ --updating siblings 
                do 
                  res3 <- query firstRow (from graph1Table (at ns2))
                  case res3 of 
                    record3@(Labels _ _ _ _ _ _  _ _ ps) -> store graph1Table (Just ns2) record3{lastSibling = nd1}
                  return ()
              when (ps2 >0 ) $
                do 
                  res4 <- query firstRow (from graph1Table (at ps2))
                  case res4 of 
                    record4@(Labels _ _ _ _ _ _  _ ns _) -> store graph1Table (Just ps2) record4{nextSibling = nd1}
                  return ()              
              let record1 = Labels 0 pre post Set.empty Set.empty nd2 nd2 ns2 ps2
              store graph1Table (Just nd2) record2{tree_parent=nd1, nextSibling= (-1), lastSibling = (-1)}
              if Set.null hp2
                then updateDirectInAncestors nd1 record1 (Set.union dir2)
                else updateDirectInAncestors nd1 record1 (Set.insert nd2)

    ([]                                       ,[]                                          ) ->  -- Case 4
          do (pre1,pre2,post1,post2,prevSib1) <- insert2IsolatedNodes nd1 nd2 {- TODO: insert four labels after (PreLabel root) -}
             store graph1Table (Just nd1) (Labels 0   pre1 post1 Set.empty Set.empty nd2  nd2  (-1) prevSib1)
             store graph1Table (Just nd2) (Labels nd1 pre2 post2 Set.empty Set.empty (-1) (-1) (-1) (-1))
             return ()
  par <-  query firstRow (from nodeMapTable (at nd1))
  case par of
    (X pnd edges) ->  store nodeMapTable (Just nd1) (X pnd (nd2:edges))
  return()
--  store nodeMapTable (Just parent) (X pnd (List.nub (nd:edges)))
  where 
    average x y = x + div (y-x) 2
    insertIsolatedNode nd1 = do 
      res <- query firstRow (from graph1Table (at 0))
      case res of 
        record@(Labels _ _ _ _ _ _  rlc _ _) -> if rlc < 0 then 
          do 
            let pre1 = average 0 max_bound
            let post1 = average pre1 max_bound
            store graph1Table (Just 0) record{firstChild = nd1, lastChild =nd1}
            return (pre1,post1, (-1))
          else do
            res1 <- query firstRow (from graph1Table (at rlc))
            case res1 of 
              record1@(Labels _ pr1 ps1 _ _ _ _ _ _)   -> do 
                let pre1 = average ps1 max_bound 
                let post1 = average pre1 max_bound
                store graph1Table (Just rlc) record1{ nextSibling = nd1} 
                store graph1Table (Just 0) record{ lastChild =nd1} 
                return (pre1,post1, rlc)
    insert2IsolatedNodes nd1 nd2 = do 
      res <- query firstRow (from graph1Table (at 0))
      case res of 
        record@(Labels _ _ _ _ _ _  rlc _ _) -> if rlc < 0 then 
          do 
            let pre1 = average 0 max_bound
            let post1 = average pre1 max_bound
            let pre2 = average pre1 post1
            let post2 = average pre2 post1
            store graph1Table (Just 0) record{firstChild = nd1, lastChild =nd1}
            return (pre1,pre2,post1,post2, (-1))
          else do
            res1 <- query firstRow (from graph1Table (at rlc))
            case res1 of 
              record1@(Labels  _ pr1 ps1 _ _ _ _ _ _ )-> do 
                let pre1 = average ps1 max_bound 
                let post1 = average pre1 max_bound
                let pre2 = average pre1 post1 
                let post2 = average pre2 post1 
                store graph1Table (Just rlc) record1{ nextSibling = nd1} 
                store graph1Table (Just 0) record{ lastChild =nd1}
                return (pre1,pre2,post1,post2,rlc )


nextOf :: PrePostRef -> Daison PrePostRef 
nextOf (PreLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if fc >0 then return (PreLabel fc) 
      else return (PostLabel nd)
nextOf (PostLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if ns >0 then return (PreLabel ns)  
      else return (PostLabel trp)

prevOf :: PrePostRef -> Daison PrePostRef
prevOf (PreLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if ls >0 then return  (PostLabel ls) 
      else return (PreLabel trp)
prevOf (PostLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> if lc >0 then  return (PostLabel lc)  
      else return (PreLabel nd)

getLabel :: PrePostRef -> Daison Nd
getLabel (PreLabel nd) = do 
--  liftIO $ print $ " getprelabel : " ++show nd
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ]
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> return pr
getLabel (PostLabel nd) = do 
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ]
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> return ps

updateLabel :: Labels ->Nd ->Labels -> Nd -> Daison()
updateLabel record@(Labels ptrp ppr pps _ _ pfc flc pns pls ) nd1 record1@(Labels _ pr1 ps1 _ _ fc1  rlc1 ns1 _) nd2 
  | nd2 < 0 = do  
    let pre1 = average ppr pps
    let post1 = average pre1 pps
    store graph1Table (Just nd1) record1{preL = pre1, postL =post1}
--    liftIO $ print $ " nd :" ++ show nd1 ++ " from if : " ++ show record1
    when (ns1>0) $  updateLabel record nd1 record1{preL = pre1, postL =post1} ns1 
    when  (fc1 > 0 ) $
      do 
        res3 <- query firstRow (from graph1Table (at fc1)) 
        updateLabel record1{preL = pre1, postL =post1} fc1 res3 (-1) 

    return ()
  | otherwise = do
    res2 <- query firstRow (from graph1Table (at nd2))
    case res2 of 
      record2@(Labels _ pr2 ps2 _ _ fc2  rlc2 ns2 _) -> do 
            let pre2 = average pps ps1
            let post2 = average pre2 pps
            store graph1Table (Just nd2) record2{preL = pre2, postL =post2}
--            liftIO $ print $ " nd :" ++ show nd2 ++ " edges : " ++ show record2
            if (ns2 > 0) then updateLabel record nd2 record2{preL = pre2, postL =post2} ns2 else return()
            if (fc2 > 0) then 
              do 
                res3 <- query firstRow (from graph1Table (at fc2)) 
                updateLabel record2{preL = pre2, postL =post2} fc2 res3 (-1) 
              else return()

    return()
  where 
    average x y = x + div (y-x) 2





reLabelMain :: PrePostRef -> Daison ()
reLabelMain v  {- (PreLabel nd) -} =  do 
  let d = 3
  let count = 2
  (newd, newcount,newv, begin, end) <- mainLoop d count v v v --(PreLabel nd) (PreLabel nd) (PreLabel nd)
  reLabelRange begin end newv newd newcount
  
  x <- select [ x | x <- from graph1Table everything ] 
--  liftIO $  print "from relabelMain"
--  liftIO $  mapM_ (\y -> putStrLn (show y) ) x
  return ()

mainLoop :: Int64 -> Int64-> PrePostRef ->PrePostRef -> PrePostRef -> Daison (Nd, Int64, PrePostRef, PrePostRef, PrePostRef)
mainLoop d count v begin end = do
  --max <- getCounter
--  liftIO $ print $ " mainloop v :" ++ show v ++ "  : " ++ show begin
  if (d <  max_bound) then 
    do 
      (begin1, count1, v1, d1)  <- goPrev (begin) count v d
      (end1, count2, v2, d2)  <- goNext (end) count1 v1 d1
      if (count2 * count2 < fromIntegral (d2 +1)  ) then 
        return (d2, count2, v2, begin1, end1)
      else
        do 
          let d3 = d2 Bits..|. (Bits.shiftL d2 1)
          mainLoop d3 count2 v2 begin1 end1
  else 
    return (d, count, v, begin, end)

goPrev :: PrePostRef -> Int64 -> PrePostRef -> Int64 -> Daison (PrePostRef, Int64, PrePostRef, Int64)
goPrev begin count v d = do 
  vlabel <- getLabel v
  newbegin <- prevOf begin 
--  liftIO $ print $ " go new begin :" ++ show newbegin ++ " v : " ++ show v
  newbeginLabel <- getLabel (newbegin)
--  liftIO $ print $ " goPrev :" ++ show newbeginLabel ++ "  : " ++ show (vlabel Bits..&. (Bits.complement d))
  if (newbeginLabel > (vlabel Bits..&. (Bits.complement d))) then 
    goPrev newbegin (count +1) v d
  else return (begin, count, v, d)


goNext :: PrePostRef -> Int64 -> PrePostRef -> Int64 -> Daison (PrePostRef, Int64, PrePostRef, Int64)
goNext end count v d = do 
  vlabel <- getLabel v
  newend <- nextOf end
  newendLabel <- getLabel (newend)  
--  liftIO $ print $ " goNext :" ++ show newendLabel ++ "  : " ++ show (vlabel Bits..|. d)
  if (newendLabel < (vlabel Bits..|. d)) then 
    goNext newend (count +1) v d
  else return (end, count, v, d)

reLabelRange :: PrePostRef -> PrePostRef -> PrePostRef -> Int64 ->Int64 -> Daison()
reLabelRange begin end v d count = do 
  let n = div d count
  vlabel <- getLabel v
  let newv = vlabel  Bits..&. (Bits.complement d )
--  liftIO $ print $ " relabelRange :" ++ show begin ++ "  : " ++ show end
  reLabelNodes begin end newv n 1
  return ()


reLabelNodes begin end v n i = do 
  if begin == end then do
--    liftIO $ print $ " relabelNodes :" ++ show begin ++ "  : " ++ show end ++ ":  " ++ show i
    case begin of 
      (PreLabel nd ) ->   do
        record <- select [(nd, labels) | (labels) <- from graph1Table (at nd)  ] 
        case record of
          [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
            let newlabel = v + i * n
            update_ graph1Table (return (nd, Labels tp newlabel ps  hp dir fc lc ns ls) )
            return()
      (PostLabel nd ) ->   do
        record <- select [(nd, labels) | (labels) <- from graph1Table (at nd)  ] 
        case record of
          [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
            let newlabel = v + i * n
            update_ graph1Table (return (nd, Labels tp pr newlabel   hp dir fc lc ns ls) )
            return()

    return ()
  else do 
--    liftIO $ print $ " relabelNodes :" ++ show begin ++ "  : " ++ show end ++ ":  " ++ show i
    case begin of 
      (PreLabel nd ) ->   do
        record <- select [(nd, labels) | (labels) <- from graph1Table (at nd)  ] 
        case record of
          [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
            let newlabel = v + i * n
            update_ graph1Table (return (nd, Labels tp newlabel ps  hp dir fc lc ns ls) )
            return()
      (PostLabel nd ) ->   do
        record <- select [(nd, labels) | (labels) <- from graph1Table (at nd)  ] 
        case record of
          [(nd , Labels tp pr ps hp dir fc lc ns ls)] -> do
            let newlabel = v + i * n
            update_ graph1Table (return (nd, Labels tp pr newlabel   hp dir fc lc ns ls) )
            return()
    newbegin <- nextOf (begin)
--    liftIO $ print $ " begin : " ++show begin ++ " newbegin : " ++ show newbegin
--    liftIO $ print $ " I : " ++show i ++ " n  : " ++ show n

    reLabelNodes newbegin end v n (i +1)
