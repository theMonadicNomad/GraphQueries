(1,TP: 0 Pre: 0 Post:  21 Hops: [] Directs:  [7,8,9,10,11] Tree Edges : [11,2])
(2,TP: 1 Pre: 1 Post:  18 Hops: [] Directs:  [7,8,9,10] Tree Edges : [10,5,3])
(3,TP: 2 Pre: 2 Post:  5 Hops: [] Directs:  [] Tree Edges : [4])
(4,TP: 3 Pre: 3 Post:  4 Hops: [] Directs:  [] Tree Edges : [])
(5,TP: 2 Pre: 6 Post:  15 Hops: [] Directs:  [7,8,9] Tree Edges : [9,6])
(6,TP: 5 Pre: 7 Post:  12 Hops: [] Directs:  [7,8] Tree Edges : [8,7])
(7,TP: 6 Pre: 8 Post:  9 Hops: [4] Directs:  [] Tree Edges : [])
(8,TP: 6 Pre: 10 Post:  11 Hops: [4] Directs:  [] Tree Edges : [])
(9,TP: 5 Pre: 13 Post:  14 Hops: [4] Directs:  [] Tree Edges : [])
(10,TP: 2 Pre: 16 Post:  17 Hops: [6] Directs:  [] Tree Edges : [])
(11,TP: 1 Pre: 19 Post:  20 Hops: [9] Directs:  [] Tree Edges : [])
(1,X {ndc = 'a', keyLabels = 1})
(2,X {ndc = 'b', keyLabels = 2})
(3,X {ndc = 'd', keyLabels = 3})
(4,X {ndc = 'k', keyLabels = 4})
(5,X {ndc = 'e', keyLabels = 5})
(6,X {ndc = 'g', keyLabels = 6})
(7,X {ndc = 'i', keyLabels = 7})
(8,X {ndc = 'j', keyLabels = 8})
(9,X {ndc = 'h', keyLabels = 9})
(10,X {ndc = 'f', keyLabels = 10})
(11,X {ndc = 'c', keyLabels = 11})




" from Case3 -- HasParent2 Insert (l,k)"
(1,TP: 0 Pre: 0 Post:  21 Hops: [] Directs:  [7,8,9,10,11] Tree Edges : [11,2])
(2,TP: 1 Pre: 1 Post:  18 Hops: [] Directs:  [7,8,9,10] Tree Edges : [10,5,3])
(3,TP: 2 Pre: 2 Post:  5 Hops: [] Directs:  [] Tree Edges : [4])
(4,TP: 3 Pre: 3 Post:  4 Hops: [] Directs:  [] Tree Edges : [])
(5,TP: 2 Pre: 6 Post:  15 Hops: [] Directs:  [7,8,9] Tree Edges : [9,6])
(6,TP: 5 Pre: 7 Post:  12 Hops: [] Directs:  [7,8] Tree Edges : [8,7])
(7,TP: 6 Pre: 8 Post:  9 Hops: [4] Directs:  [] Tree Edges : [])
(8,TP: 6 Pre: 10 Post:  11 Hops: [4] Directs:  [] Tree Edges : [])
(9,TP: 5 Pre: 13 Post:  14 Hops: [4] Directs:  [] Tree Edges : [])
(10,TP: 2 Pre: 16 Post:  17 Hops: [6] Directs:  [] Tree Edges : [])
(11,TP: 1 Pre: 19 Post:  20 Hops: [9] Directs:  [] Tree Edges : [])
(12,TP: -1 Pre: 23 Post:  24 Hops: [4] Directs:  [] Tree Edges : [])
(1,X {ndc = 'a', keyLabels = 1})
(2,X {ndc = 'b', keyLabels = 2})
(3,X {ndc = 'd', keyLabels = 3})
(4,X {ndc = 'k', keyLabels = 4})
(5,X {ndc = 'e', keyLabels = 5})
(6,X {ndc = 'g', keyLabels = 6})
(7,X {ndc = 'i', keyLabels = 7})
(8,X {ndc = 'j', keyLabels = 8})
(9,X {ndc = 'h', keyLabels = 9})
(10,X {ndc = 'f', keyLabels = 10})
(11,X {ndc = 'c', keyLabels = 11})
(12,X {ndc = 'l', keyLabels = 12})







 How to find the previous label

prevOf (PreLabel node) = case prevSibling node of
    Just prev -> PostLabel prev
    Nothing -> PreLabel (parentOf node)

prevOf (PostLabel node) = case lastChild node of
    Just last -> PostLabel last 
    Nothing -> PreLabel node
 


nextOf (PreLabel node) = case firstChild node of
    Just child -> PreLabel child 
    Nothing -> PostLabel node

nextOf (PostLabel node) = case nextSibling node of
    Just next -> PreLabel next
    Nothing -> PostLabel (parentOf node)
 





(1,TP: 0 Pre: 0 Post:  21 Hops: [] Directs:  [7,8,9,10,11]FC : 2 LC :  11 NS: -100 PS : -100)
(2,TP: 1 Pre: 1 Post:  18 Hops: [] Directs:  [7,8,9,10]FC : 3 LC :  10 NS: 11 PS : -1)
(3,TP: 2 Pre: 2 Post:  5 Hops: [] Directs:  []FC : 4 LC :  -1 NS: 5 PS : -1)
(4,TP: 3 Pre: 3 Post:  4 Hops: [] Directs:  []FC : -1 LC :  -1 NS: -1 PS : -1)
(5,TP: 2 Pre: 6 Post:  15 Hops: [] Directs:  [7,8,9]FC : 6 LC :  9 NS: 10 PS : 3)
(6,TP: 5 Pre: 7 Post:  12 Hops: [] Directs:  [7,8]FC : 7 LC :  8 NS: 9 PS : -1)
(7,TP: 6 Pre: 8 Post:  9 Hops: [4] Directs:  []FC : 4 LC :  -1 NS: 8 PS : -1)
(8,TP: 6 Pre: 10 Post:  11 Hops: [4] Directs:  []FC : 4 LC :  -1 NS: -1 PS : 7)
(9,TP: 5 Pre: 13 Post:  14 Hops: [4] Directs:  []FC : 4 LC :  -1 NS: -1 PS : 6)
(10,TP: 2 Pre: 16 Post:  17 Hops: [6] Directs:  []FC : 6 LC :  -1 NS: -1 PS : 5)
(11,TP: 1 Pre: 19 Post:  20 Hops: [9] Directs:  []FC : 9 LC :  -1 NS: -1 PS : 2)

(1,X {nd = C 'a', edges = [11,2]})
(2,X {nd = C 'b', edges = [10,5,3]})
(3,X {nd = C 'd', edges = [4]})
(4,X {nd = C 'k', edges = []})
(5,X {nd = C 'e', edges = [9,6]})
(6,X {nd = C 'g', edges = [8,7]})
(7,X {nd = C 'i', edges = [4]})
(8,X {nd = C 'j', edges = [4]})
(9,X {nd = C 'h', edges = [4]})
(10,X {nd = C 'f', edges = [6]})
(11,X {nd = C 'c', edges = [9]})



(1,TP: 0 Hops: [] Directs:  []FC : -100 LC :  -100 NS: -100 PS : -100)
(2,TP: 1 Hops: [] Directs:  [8,9,10,11,12]FC : 3 LC :  12 NS: -100 PS : -100)
(3,TP: 2 Hops: [] Directs:  [8,9,10,11]FC : 4 LC :  11 NS: 12 PS : -1)
(4,TP: 3 Hops: [] Directs:  []FC : 5 LC :  -1 NS: 6 PS : -1)
(5,TP: 4 Hops: [] Directs:  []FC : -1 LC :  -1 NS: -1 PS : -1)
(6,TP: 3 Hops: [] Directs:  [8,9,10]FC : 7 LC :  10 NS: 11 PS : 4)
(7,TP: 6 Hops: [] Directs:  [8,9]FC : 8 LC :  9 NS: 10 PS : -1)
(8,TP: 7 Hops: [5] Directs:  []FC : -1 LC :  -1 NS: 9 PS : -1)
(9,TP: 7 Hops: [5] Directs:  []FC : -1 LC :  -1 NS: -1 PS : 8)
(10,TP: 6 Hops: [5] Directs:  []FC : -1 LC :  -1 NS: -1 PS : 7)
(11,TP: 3 Hops: [7] Directs:  []FC : -1 LC :  -1 NS: -1 PS : 6)
(12,TP: 2 Hops: [10] Directs:  []FC : -1 LC :  -1 NS: -1 PS : 3)
(1,X {nd = C 'r', edges = []})
(2,X {nd = C 'a', edges = [12,3]})
(3,X {nd = C 'b', edges = [11,6,4]})
(4,X {nd = C 'd', edges = [5]})
(5,X {nd = C 'k', edges = []})
(6,X {nd = C 'e', edges = [10,7]})
(7,X {nd = C 'g', edges = [9,8]})
(8,X {nd = C 'i', edges = [5]})
(9,X {nd = C 'j', edges = [5]})
(10,X {nd = C 'h', edges = [5]})
(11,X {nd = C 'f', edges = [7]})
(12,X {nd = C 'c', edges = [10]})





1,TP: 0 Pre: 0 Post:  9223372036854775807 Hops: fromList [] Directs:  fromList []FC : -100 LC :  -100 NS: -100 PS : -100)
(2,TP: 1 Pre: 4611686018427387903 Post:  6917529027641081854 Hops: fromList [] Directs:  fromList [8,9,10,11,12]FC : 3 LC :  12 NS: -100 PS : -100)
(3,TP: 2 Pre: 5764607523034234878 Post:  6341068275337658366 Hops: fromList [] Directs:  fromList [8,9,10,11]FC : 4 LC :  11 NS: 12 PS : -1)
(4,TP: 3 Pre: 6052837899185946622 Post:  6196953087261802494 Hops: fromList [] Directs:  fromList []FC : 5 LC :  5 NS: 6 PS : -1)
(5,TP: 4 Pre: 6124895493223874558 Post:  6160924290242838526 Hops: fromList [] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : -1)
(6,TP: 3 Pre: 6052837899185946622 Post:  6196953087261802494 Hops: fromList [] Directs:  fromList [8,9,10]FC : 7 LC :  10 NS: 11 PS : 4)
(7,TP: 6 Pre: 6124895493223874558 Post:  6160924290242838526 Hops: fromList [] Directs:  fromList [8,9]FC : 8 LC :  9 NS: 10 PS : -1)
(8,TP: 7 Pre: 6142909891733356542 Post:  6151917090988097534 Hops: fromList [5] Directs:  fromList []FC : -1 LC :  -1 NS: 9 PS : -1)
(9,TP: 7 Pre: 6142909891733356542 Post:  6151917090988097534 Hops: fromList [5] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 8)
(10,TP: 6 Pre: 6124895493223874558 Post:  6160924290242838526 Hops: fromList [5] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 7)
(11,TP: 3 Pre: 6052837899185946622 Post:  6196953087261802494 Hops: fromList [7] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 6)
(12,TP: 2 Pre: 5764607523034234878 Post:  6341068275337658366 Hops: fromList [10] Directs:  fromList []FC : -1 LC :  -1 NS: -1 P


--correct pre and post order
(0,TP: 0 Pre: 0 Post:  9223372036854775807 Hops: fromList [] Directs:  fromList []FC : 1 LC :  1 NS: -100 PS : -100)
(1,TP: 0 Pre: 4611686018427387903 Post:  6917529027641081854 Hops: fromList [] Directs:  fromList [6,7,8,9,10,11]FC : 2 LC :  11 NS: -1 PS : -1)
(2,TP: 1 Pre: 5764607523034234878 Post:  6341068275337658366 Hops: fromList [] Directs:  fromList [6,7,8,9,10]FC : 3 LC :  10 NS: 11 PS : -1)
(3,TP: 2 Pre: 6052837899185946622 Post:  6196953087261802494 Hops: fromList [] Directs:  fromList []FC : 4 LC :  4 NS: 5 PS : -1)
(4,TP: 3 Pre: 6124895493223874558 Post:  6160924290242838526 Hops: fromList [] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : -1)
(5,TP: 2 Pre: 6269010681299730430 Post:  6305039478318694398 Hops: fromList [] Directs:  fromList [6,7,8,9]FC : 6 LC :  9 NS: 10 PS : 3)
(6,TP: 5 Pre: 6287025079809212414 Post:  6296032279063953406 Hops: fromList [7,8] Directs:  fromList [7,8]FC : 7 LC :  8 NS: 9 PS : -1)
(7,TP: 6 Pre: 6291528679436582910 Post:  6293780479250268158 Hops: fromList [4] Directs:  fromList []FC : -1 LC :  -1 NS: 8 PS : -1)
(8,TP: 6 Pre: 6294906379157110782 Post:  6295469329110532094 Hops: fromList [4] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 7)
(9,TP: 5 Pre: 6300535878691323902 Post:  6302787678505009150 Hops: fromList [4] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 6)
(10,TP: 2 Pre: 6323053876828176382 Post:  6332061076082917374 Hops: fromList [6] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 5)
(11,TP: 1 Pre: 6629298651489370110 Post:  6773413839565225982 Hops: fromList [9] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 2)
(0,X {nd = S "root", edges = []})
(1,X {nd = C 'a', edges = []})
(2,X {nd = C 'b', edges = []})
(3,X {nd = C 'd', edges = []})
(4,X {nd = C 'k', edges = []})
(5,X {nd = C 'e', edges = []})
(6,X {nd = C 'g', edges = []})
(7,X {nd = C 'i', edges = []})
(8,X {nd = C 'j', edges = []})
(9,X {nd = C 'h', edges = []})
(10,X {nd = C 'f', edges = []})
(11,X {nd = C 'c', edges = []})




(0,TP: 0 Hops: fromList [] Directs:  fromList []FC : 1 LC :  1 NS: -100 PS : -100)
(1,TP: 0 Hops: fromList [] Directs:  fromList [7,8,9,10,11]FC : 2 LC :  11 NS: -1 PS : -1)
(2,TP: 1 Hops: fromList [] Directs:  fromList [7,8,9,10]FC : 3 LC :  10 NS: 11 PS : -1)
(3,TP: 2 Hops: fromList [] Directs:  fromList []FC : 4 LC :  4 NS: 5 PS : -1)
(4,TP: 3 Hops: fromList [] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : -1)
(5,TP: 2 Hops: fromList [] Directs:  fromList [7,8,9]FC : 6 LC :  9 NS: 10 PS : 3)
(6,TP: 5 Hops: fromList [] Directs:  fromList [7,8]FC : 7 LC :  8 NS: 9 PS : -1)
(7,TP: 6 Hops: fromList [4] Directs:  fromList []FC : -1 LC :  -1 NS: 8 PS : -1)
(8,TP: 6 Hops: fromList [4] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 7)
(9,TP: 5 Hops: fromList [4] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 6)
(10,TP: 2 Hops: fromList [6] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 5)
(11,TP: 1 Hops: fromList [9] Directs:  fromList []FC : -1 LC :  -1 NS: -1 PS : 2)
(0,X {nd = S "root", edges = [1]})
(1,X {nd = C 'a', edges = [11,2]})
(2,X {nd = C 'b', edges = [10,5,3]})
(3,X {nd = C 'd', edges = [4]})
(4,X {nd = C 'k', edges = []})
(5,X {nd = C 'e', edges = [9,6]})
(6,X {nd = C 'g', edges = [8,7]})
(7,X {nd = C 'i', edges = [4]})
(8,X {nd = C 'j', edges = [4]})
(9,X {nd = C 'h', edges = [4]})
(10,X {nd = C 'f', edges = [6]})
(11,X {nd = C 'c', edges = [9]})




(0,TP: 0 Pre: 0 Post:  9223372036854775807)
(1,TP: 0 Pre: 4611686018427387903 Post:  6917529027641081855)
(2,TP: 1 Pre: 5764607523034234879 Post:  6341068275337658367)
(3,TP: 2 Pre: 6052837899185946623 Post:  6196953087261802495)
(4,TP: 3 Pre: 6124895493223874559 Post:  6160924290242838527)
(5,TP: 4 Pre: 6142909891733356543 Post:  6151917090988097535)
(6,TP: 5 Pre: 6147413491360727039 Post:  6149665291174412287)
(7,TP: 6 Pre: 6148539391267569663 Post:  6149102341220990975)
(8,TP: 7 Pre: 6148820866244280319 Post:  6148961603732635647)
(9,TP: 8 Pre: 6148891234988457983 Post:  6148926419360546815)
(10,TP: 9 Pre: 6148908827174502399 Post:  6148917623267524607)
(11,TP: 10 Pre: 6148913225221013503 Post:  6148915424244269055)
(12,TP: 11 Pre: 6148914324732641279 Post:  6148914874488455167)
(13,TP: 12 Pre: 6148914599610548223 Post:  6148914737049501695)
(14,TP: 13 Pre: 6148914668330024959 Post:  6148914702689763327)
(15,TP: 14 Pre: 6148914685509894143 Post:  6148914694099828735)
(16,TP: 15 Pre: 6148914689804861439 Post:  6148914691952345087)
(17,TP: 16 Pre: 6148914690878603263 Post:  6148914691415474175)
(18,TP: 17 Pre: 6148914691147038719 Post:  6148914691281256447)
(19,TP: 18 Pre: 6148914691214147583 Post:  6148914691247702015)
(20,TP: 19 Pre: 6148914691230924799 Post:  6148914691239313407)
(21,TP: 20 Pre: 6148914691235119103 Post:  6148914691237216255)
(22,TP: 21 Pre: 6148914691236167679 Post:  6148914691236691967)
(23,TP: 22 Pre: 6148914691236429823 Post:  6148914691236560895)
(24,TP: 23 Pre: 6148914691236495359 Post:  6148914691236528127)
(25,TP: 24 Pre: 6148914691236511743 Post:  6148914691236519935)
(26,TP: 25 Pre: 6148914691236515839 Post:  6148914691236517887)
(27,TP: 26 Pre: 6148914691236516863 Post:  6148914691236517375)
(28,TP: 27 Pre: 6148914691236517119 Post:  6148914691236517247)
(29,TP: 28 Pre: 6148914691236517183 Post:  6148914691236517215)
(30,TP: 29 Pre: 6148914691236517199 Post:  6148914691236517207)
(31,TP: 30 Pre: 6148914691236517203 Post:  6148914691236517205)
(32,TP: 31 Pre: 6148914691236517204 Post:  6148914691236517204)
(33,TP: 32 Pre: 6148914691236517204 Post:  6148914691236517204)