-- Functions related to Static AILabel.
-- replacing process in the main function initiates static AILabel process.


processRemainingNodes1 :: GraphMap Node -> Daison ()
processRemainingNodes1 graphmap = do 
  nodes <- select [nd | (ind, ( X nd nodeindex )) <- from nodeMapTable everything  ]
  let graphlist = Map.toList graphmap
  let nodelist = map (\(x,y) -> x ) graphlist
  let difference = nodelist List.\\ nodes
  liftIO $ print $ " nodes :  " ++ show nodes
  liftIO $ print $ " difference : " ++ show difference
  liftIO $ print $ " nodelist : " ++ show nodelist
  when (length difference > 0 ) $ do 
    processNodes1 graphmap (head difference) (head difference)
    processRemainingNodes1 graphmap
  return()

staticProcess :: GraphMap Node -> Daison ()
staticProcess graphmap = do
  let firstnode = fst $ Map.elemAt 0 graphmap
  processNodes1 graphmap firstnode firstnode
  processRemainingNodes1 graphmap

processNodes1 :: GraphMap Node -> Node -> Node -> Daison()
processNodes1 graph nd parent = do
  x <- insertNodeinDB nd parent 
  unless x $ do
    let adjacent = Map.lookup nd graph
    case adjacent of
      Nothing      -> do
        nod <- getNdIndex nd 
        record <- select [(nod,label1) | (label1) <- from graph1Table (at nod)  ] 
        liftIO $ print $ " Nothing : " ++ show record
        case record of 
          [(nd, label)] -> case label of
            Labels trp pr ps hp dir fc lc ns ls ->  update_ graph1Table (return (nd, Labels trp pr ps hp dir (-1) (-1) ns ls )) 
      Just []   ->  do
        nod <- getNdIndex nd 
        record <- select [(nod,label1) | (label1) <- from graph1Table (at nod)  ] 
        liftIO $ print $ " just : " ++ show record
        case record of 
          [(nd, label)] -> case label of
            Labels trp pr ps hp dir fc lc ns ls ->  update_ graph1Table (return (nd, Labels trp pr ps hp dir (-1) (-1) ns ls )) 
      Just rest    ->do
        mapM_ (\x -> processNodes1 graph x nd ) rest
        children <- mapM (\x -> getNdIndex x) rest 
        nod <- getNdIndex nd 
        case filter (>nod) children of 
          [] -> return ()
          [ele] -> updateSiblings  (-1) ele (-1)
          first : second : [] -> updateSiblings  (-1) first second >> updateSiblings first second   (-1)
          list -> do 
            let values =  (( (-1) , head list, head (tail list) ) : zip3 (list) ((tail list)) (tail (tail list)) )++ [( last (init list) ,last list,  (-1))]
            mapM_ (\(left, ele , right) -> updateSiblings left ele right) values
        
        record <- select [(nod,label1) | (label1) <- from graph1Table (at nod)  ] 
        case record of 
          [(nd, label)] -> case label of
            Labels trp pr ps hp dir fc lc ns ls ->  do
              first_child <- getNdIndex (head rest) >>= \fc -> if nd > fc then return (-1) else return fc
              case (tail rest) of 
                []  -> update_ graph1Table (return (nd, Labels trp pr ps hp dir first_child first_child ns ls ))
                child -> do 
                  last_child <- getNdIndex (last rest) >>= \lc -> if nd > lc then return (-1) else return lc
                  update_ graph1Table (return (nd, Labels trp pr ps hp dir first_child last_child ns ls ))
    getNdIndex nd >>= \nd1 -> updatePost nd1
  where 
    getNdIndex node = do
      nod <- select [ind | (ind, ( X nd nodeindex )) <- from nodeMapTable everything , nd == node  ]
      case nod of
        [nd] -> return nd
        []   -> do 
          c_counter <- getCounter
          incrementCounter >> incrementCounter  
          pkey <- insert_ nodeMapTable (X node [])
          store  graph1Table (Just pkey) (Labels (-1) (-3) (-2) Set.empty Set.empty (-100) (-100) (-100) (-100)  )
          return pkey
        _    -> error $ "ivalid getindex nd :" ++ show nod



insertNodeinDB :: Node -> Node ->Daison Bool
insertNodeinDB node parent  = do
  map <- select [ind | (ind, ( X nd edgess )) <- from nodeMapTable everything , nd == node  ]
  par <- if (node == parent) then return [(0,( C 'a',[]))] 
         else select [(ind,(nd, edgess)) | (ind, ( X nd edgess )) <- from nodeMapTable everything , nd == parent  ]
  let parent_ndmp = fst . head $ par
  let edges_par = snd . snd . head $ par 
  let parent_ndc = fst . snd . head $ par
  case map of
    [] -> do
      c_counter <- getCounter
      incrementCounter 
      pkey <-  insert_ nodeMapTable (X node [])
      store graph1Table (Just pkey) (Labels parent_ndmp (c_counter) (c_counter)  Set.empty Set.empty (-100) (-100) (-100) (-100) )    
      
      when (parent_ndmp > 0) $ do
        update_ nodeMapTable  (return (parent_ndmp, (X (parent_ndc) (pkey:edges_par) ) ))
      return False
    [nod]   ->  do
      parent_record <- select [(parent_ndmp, labels2) | (labels2) <- from graph1Table (at parent_ndmp)  ] 
      case parent_record of 
          [] -> error "error "
          [(indp, labelp)] -> case labelp of 
            (Labels ptrp ppr pps php pdir fc lc ns ls) -> do
              update_ graph1Table (return (indp,(Labels ptrp ppr pps (Set.insert (head map) php ) pdir fc lc ns ls ) ))
              when (ptrp > 0) $ updateDirects parent_ndmp ptrp 
      update_ nodeMapTable  (return (parent_ndmp, (X (parent_ndc) (nod:edges_par) ) ))

      return True 
  where
    average x y = x + div (y-x) 2

updatePre :: Nd -> [Nd] -> Daison Bool
updatePre nd visited = do 
  record <- select [(nd,label1) | (label1) <- from graph1Table (at nd)  ] 
  case record of 
    [(nd, Labels trp pr ps hp dir fc lc ns ls  )] -> if pr == ps || elem nd visited then return True
                                           else   
                                             do
                                               c_counter <- getCounter
                                               incrementCounter
                                               update_ graph1Table (return (nd, Labels trp c_counter c_counter hp dir fc lc ns ls ))
                                               return False
    _ -> error "error " 


updatePost :: Nd -> Daison ()
updatePost nd = do 
  record <- select [(nd,label1) | (label1) <- from graph1Table (at nd)  ] 
  case record of 
    [(nd, label)] -> case label of
      Labels trp pr ps hp dir fc lc ns ls ->  do 
        c_counter <- getCounter
        incrementCounter
        update_ graph1Table (return (nd, Labels trp pr c_counter hp dir fc lc ns ls ))
      _   -> error "error from updatepost"
    _ -> error "error " 


updateSiblings :: Nd -> Nd -> Nd -> Daison ()
updateSiblings left nd right = do
  record <- select [label1 | (label1) <- from graph1Table (at nd)  ] 
  case record of
    [(Labels trp pr ps hp dir fc lc ns ls)] -> update_ graph1Table (return (nd, Labels trp pr ps hp dir fc lc right left ))







{- updateLabel :: Labels ->Nd ->Labels -> Nd -> Daison()
updateLabel record@(Labels ptrp ppr pps _ _  ) nd1 record1@(Labels _ pr1 ps1 _ _ ) nd2 
  | nd2 < 0 = do  
    let pre1 = average ppr pps
    let post1 = average pre1 pps
    store graph1Table (Just nd1) record1{preL = pre1, postL =post1}
--    liftIO $ print $ " nd :" ++ show nd1 ++ " from if : " ++ show record1
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
 -}


{- reLabelMain :: PrePostRef -> Daison ()
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
 -}

