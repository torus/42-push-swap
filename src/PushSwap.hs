module PushSwap where

import Data.List
import Data.Lists
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Bool (otherwise)
import GHC.Base (otherwise)

newtype StackPair = StackPair ([Int], [Int]) deriving (Eq)

instance Show StackPair where
    show (StackPair (as, bs)) = show (reverse bs) ++ " " ++ show as

pushStack :: [Int] -> Int -> [Int]
pushStack s a = a : s

makeStackPair :: [Int] -> [Int] -> StackPair
makeStackPair l r = StackPair (l, r)

stackA (StackPair a) = fst a
stackB (StackPair a) = snd a

stackSwap :: [a] -> [a]
stackSwap [] = []
stackSwap [a] = [a]
stackSwap (a : b : as) = b : a : as


recOp :: String -> (StackPair -> StackPair) -> (StackPair, [String]) -> (StackPair, [String])
recOp name op (sp, ops) = (op sp, name : ops)

sa :: StackPair -> StackPair
sa p = makeStackPair (stackSwap (stackA p)) (stackB p)

sb :: StackPair -> StackPair
sb p = makeStackPair (stackA p) (stackSwap (stackB p))

ss :: StackPair -> StackPair
ss p = makeStackPair (stackSwap (stackA p)) (stackSwap (stackB p))

pa :: StackPair -> StackPair
pa (StackPair (as, [])) = makeStackPair as []
pa (StackPair (as, b : bs)) = makeStackPair (b : as) bs

pb :: StackPair -> StackPair
pb (StackPair ([], bs)) = makeStackPair [] bs
pb (StackPair (a : as, bs)) = makeStackPair as (a : bs)

stackRotate :: [a] -> [a]
stackRotate [] = []
stackRotate (s : ss) = ss ++ [s]

ra :: StackPair -> StackPair
ra p = makeStackPair (stackRotate (stackA p)) (stackB p)

rb :: StackPair -> StackPair
rb p = makeStackPair (stackA p) (stackRotate (stackB p))

rr :: StackPair -> StackPair
rr p = makeStackPair (stackRotate (stackA p)) (stackRotate (stackB p))

stackReverseRotate :: [a] -> [a]
stackReverseRotate [] = []
stackReverseRotate p = last p : init p

rra :: StackPair -> StackPair
rra p = makeStackPair (stackReverseRotate (stackA p)) (stackB p)

rrb :: StackPair -> StackPair
rrb p = makeStackPair (stackA p) (stackReverseRotate (stackB p))

rrr :: StackPair -> StackPair
rrr p = makeStackPair (stackReverseRotate (stackA p))
                      (stackReverseRotate (stackB p))

repeatOp :: Int -> (StackPair -> StackPair) -> StackPair -> StackPair
repeatOp 0 op sp = sp
repeatOp n op sp = op $ repeatOp (n - 1) op sp

recRepeatOp :: String -> Int -> (StackPair -> StackPair)
            -> (StackPair, [String]) -> (StackPair, [String])
recRepeatOp name n op (sp, ops) = (repeatOp n op sp, replicate n name ++ ops)

topA :: StackPair -> Int
topA sp = head (stackA sp)
topB sp = head (stackB sp)

raRec, rbRec, rraRec, rrbRec,
  paRec, pbRec, saRec, sbRec, ssRec :: (StackPair, [String]) -> (StackPair, [String])

{-
rbRec = recOp "rb" rb
raRec = recOp "ra" ra
-}
rbRec (StackPair (as, []), ops) = (StackPair (as, []), ops)
rbRec (StackPair (as, [b]), ops) = (StackPair (as, [b]), ops)
rbRec x = recOp "rb" rb x

raRec (StackPair ([], bs), ops) = (StackPair ([], bs), ops)
raRec (StackPair ([a], bs), ops) = (StackPair ([a], bs), ops)
raRec x = recOp "ra" ra x

rrbRec (StackPair (as, []), ops) = (StackPair (as, []), ops)
rrbRec (StackPair (as, [b]), ops) = (StackPair (as, [b]), ops)
rrbRec x = recOp "rrb" rrb x

rraRec (StackPair ([], bs), ops) = (StackPair ([], bs), ops)
rraRec (StackPair ([a], bs), ops) = (StackPair ([a], bs), ops)
rraRec x = recOp "rra" rra x

paRec = recOp "pa" pa
pbRec = recOp "pb" pb
saRec = recOp "sa" sa
sbRec = recOp "sb" sb
ssRec = recOp "ss" ss

makeStackPair' :: [Int] -> [Int] -> StackPair
makeStackPair' l r = StackPair (r, reverse l)

median5 :: [Int] -> Int
median5 as
    | odd (length as) = sort as !! (length as `div` 2)
    | otherwise       = sort as !! (length as `div` 2 - 1)

medianOfMedians :: [Int] -> Int
medianOfMedians [] = 0
medianOfMedians as
    | length as > 5 = medianOfMedians $ map median5 (split5 as)
    | otherwise     = median5 as

split5 :: [Int] -> [[Int]]
split5 as
    | length as > 5 = take 5 as : split5 (drop 5 as)
    | otherwise     = [as]

----------

data SortOrder = Asc | Desc deriving (Show, Eq)

sweepRight1 :: (StackPair, [String]) -> ((StackPair, [String]), [(SortOrder, Int)])
sweepRight1 s = (s', lengths)
  where
    ((s', lengths), n) =  iter ((s, []), 0)
    iter ((s@(StackPair (as, bs), ops), lengths), n)
      | null as   = ((s, lengths), n)
      | otherwise = iter $ sweepRight1Iter (s, lengths) n

sweepRight1Iter :: ((StackPair, [String]), [(SortOrder, Int)]) -> Int -> (((StackPair, [String]), [(SortOrder, Int)]), Int)
sweepRight1Iter (s@(StackPair (a1 : a2 : as, bs), ops), lengths) n
  | even n && a1 > a2 || odd n && a1 < a2 = ( (pbRec $ pbRec $ saRec s
                                            , (if even n then Asc else Desc, 2) : lengths)
                                            , n + 1)
  | otherwise                             = ( (pbRec $ pbRec s
                                            , (if even n then Asc else Desc, 2) : lengths)
                                            , n + 1)
--sweepRight1Iter (s@(StackPair ([], bs), ops), lengths) n = ((s, lengths), n)
sweepRight1Iter _ _ = undefined

{-
>>> sweepRight1Iter ((makeStackPair' [] [5,1,7,3,4,6,0,2,8,9], []), []) 0
((([1,5] [7,3,4,6,0,2,8,9],["pb","pb","sa"]),[(Asc,2)]),1)
>>> sweepRight1Iter ((makeStackPair' [1,5] [7,3,4,6,0,2,8,9], []), [(Asc, 2)]) 1
((([1,5,7,3] [4,6,0,2,8,9],["pb","pb"]),[(Desc,2),(Asc,2)]),2)

>>> sweepRight1 (makeStackPair' [] [], [])
(([] [],[]),[])
>>> sweepRight1 (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9], [])
(([1,5,7,3,4,6,2,0,8,9] [],["pb","pb","pb","pb","sa","pb","pb","pb","pb","pb","pb","sa"]),[(Asc,2),(Desc,2),(Asc,2),(Desc,2),(Asc,2)])
-}

{-
adadad <- asc
dada d <- desc
ad  ad <- asc
   dad <- desc

adadada
adadad a
 dada da
  ad ada
    dada

adad  <- asc
 da d <- desc
   ad
-}

sweepLeftIter :: (StackPair, [String]) -> [(SortOrder, Int)] -> [(SortOrder, Int)] -> ((StackPair, [String]), [(SortOrder, Int)], [(SortOrder, Int)])
sweepLeftIter s@(StackPair (as, bs), ops) lengthsL lengthsR
  | null bs   = (s, [], lengthsR)
  | odd $ length lengthsL
              = (recRepeatOp "pa" lenR pa s, tail lengthsL, (fst $ head lengthsL, lenR) : lengthsR)
  | fst (head lengthsL) == Asc
              = ( mergeLeft1Desc s lenL lenR
                , init $ tail lengthsL
                , (Asc, lenL + lenR) : lengthsR
                )
  | otherwise = ( mergeLeft1Asc s lenL lenR
                , init $ tail lengthsL
                , (Desc, lenL + lenR) : lengthsR
                )
    where
      lenL = snd $ last lengthsL
      lenR = snd $ head lengthsL

mergeLeft1Asc :: (StackPair, [String]) -> Int -> Int -> (StackPair, [String])
mergeLeft1Asc s 0 0 = s
mergeLeft1Asc s@(StackPair (as, bs), ops) lenL lenR
  | lenL == 0 = recRepeatOp "pa" lenR pa s
  | lenR == 0 = mergeLeft1Asc (paRec $ rrbRec s) (lenL - 1) 0
  | head bs < last bs = mergeLeft1Asc (paRec s) lenL (lenR - 1)
  | otherwise = mergeLeft1Asc (paRec $ rrbRec s) (lenL - 1) lenR

mergeLeft1Desc :: (StackPair, [String]) -> Int -> Int -> (StackPair, [String])
mergeLeft1Desc s 0 0 = s
mergeLeft1Desc s@(StackPair (as, bs), ops) lenL lenR
  | lenL == 0 = recRepeatOp "pa" lenR pa s
  | lenR == 0 = mergeLeft1Desc (paRec $ rrbRec s) (lenL - 1) 0
  | head bs > last bs = mergeLeft1Desc (paRec s) lenL (lenR - 1)
  | otherwise = mergeLeft1Desc (paRec $ rrbRec s) (lenL - 1) lenR

{-
>>> sweepLeftIter (makeStackPair' [] [], []) [] []
(([] [],[]),[],[])
>>> sweepLeftIter (makeStackPair' [1,5,7,3,4,6,2,0,8,9] [], []) [(Asc,2),(Desc,2),(Asc,2),(Desc,2),(Asc,2)] []
(([1,5,7,3,4,6,2,0] [8,9],["pa","pa"]),[(Desc,2),(Asc,2),(Desc,2),(Asc,2)],[(Asc,2)])
>>> sweepLeftIter (makeStackPair' [1,5,7,3,4,6,2,0] [8,9], []) [(Desc,2),(Asc,2),(Desc,2),(Asc,2)] [(Asc,2)]
(([7,3,4,6] [5,2,1,0,8,9],["pa","rrb","pa","pa","rrb","pa"]),[(Asc,2),(Desc,2)],[(Desc,4),(Asc,2)])
>>> sweepLeftIter (makeStackPair' [7,3,4,6] [5,2,1,0,8,9], []) [(Asc,2),(Desc,2)] [(Desc,4),(Asc,2)]
(([] [3,4,6,7,5,2,1,0,8,9],["pa","pa","pa","pa","rrb"]),[],[(Asc,4),(Desc,4),(Asc,2)])

>>> mergeLeft1Asc (makeStackPair' [1,5,7,3,4,6,2,0] [8,9], []) 2 2
>>> mergeLeft1Asc (makeStackPair' [1,5,7,3,4,6,2,0] [8,9], []) 2 0
([7,3,4,6] [5,2,1,0,8,9],["pa","rrb","pa","pa","rrb","pa"])
([7,3,4,6,2,0] [5,1,8,9],["pa","rrb","pa","rrb"])
>>> mergeLeft1Desc (makeStackPair' [7,3,4,6,8] [5,2,1,0,9], []) 2 3
([] [3,4,6,7,8,5,2,1,0,9],["pa","pa","pa","pa","rrb","pa"])
-}

sweepRightIter :: (StackPair, [String]) -> [(SortOrder, Int)] -> [(SortOrder, Int)] -> ((StackPair, [String]), [(SortOrder, Int)], [(SortOrder, Int)])
sweepRightIter s@(StackPair (as, bs), ops) lengthsL lengthsR
  | null as   = (s, lengthsL, [])
  | odd $ length lengthsR
              = (recRepeatOp "pb" lenL pb s, (fst $ head lengthsR, lenL) : lengthsL,  tail lengthsR)
  | fst (head lengthsR) == Desc
              = ( mergeRight1Desc s lenL lenR
                , (Desc, lenL + lenR) : lengthsL
                , init $ tail lengthsR
                )
  | otherwise = ( mergeRight1Asc s lenL lenR
                , (Asc, lenL + lenR) : lengthsL
                , init $ tail lengthsR
                )
    where
      lenR = snd $ last lengthsR
      lenL = snd $ head lengthsR

mergeRight1Asc :: (StackPair, [String]) -> Int -> Int -> (StackPair, [String])
mergeRight1Asc s 0 0 = s
mergeRight1Asc s@(StackPair (as, bs), ops) lenL lenR
  | lenR == 0         = recRepeatOp "pb" lenL pb s
  | lenL == 0         = mergeRight1Asc (pbRec $ rraRec s) 0          (lenR - 1)
  | head as < last as = mergeRight1Asc (pbRec s)          (lenL - 1) lenR
  | otherwise         = mergeRight1Asc (pbRec $ rraRec s) lenL       (lenR - 1)

mergeRight1Desc :: (StackPair, [String]) -> Int -> Int -> (StackPair, [String])
mergeRight1Desc s 0 0 = s
mergeRight1Desc s@(StackPair (as, bs), ops) lenL lenR
  | lenR == 0         = recRepeatOp "pb" lenL pb s
  | lenL == 0         = mergeRight1Desc (pbRec $ rraRec s) 0          (lenR - 1)
  | head as > last as = mergeRight1Desc (pbRec s)          (lenL - 1) lenR
  | otherwise         = mergeRight1Desc (pbRec $ rraRec s) lenL       (lenR - 1)

{-
>>> mergeRight1Asc (makeStackPair' [] [3,4,6,7,5,2,1,0], []) 4 4
([0,1,2,3,4,5,6,7] [],["pb","pb","pb","rra","pb","pb","pb","rra","pb","rra","pb","rra"])
>>> mergeRight1Desc (makeStackPair' [] [7,6,4,3,0,1,2,5], []) 4 4
([7,6,5,4,3,2,1,0] [],["pb","pb","rra","pb","rra","pb","pb","pb","rra","pb","pb"])

>>> sweepRightIter (makeStackPair' [] [3,4,6,7,5,2,1,0,8,9], []) [] [(Asc,4),(Desc,4),(Desc,2)]
(([3,4,6,7] [5,2,1,0,8,9],["pb","pb","pb","pb"]),[(Asc,4)],[(Desc,4),(Desc,2)])
>>> sweepRightIter (makeStackPair' [3,4,6,7] [8,5,2,1,0,9], []) [(Asc,4)] [(Desc,4),(Desc,2)]
(([3,4,6,7,9,8,5,2,1,0] [],["pb","pb","pb","pb","pb","pb","rra"]),[(Desc,6),(Asc,4)],[])
>>> sweepLeftIter (makeStackPair' [3,4,6,7, 9,8,5,2,1,0] [], []) [(Desc,6),(Asc,4)] []
(([] [9,8,7,6,5,4,3,2,1,0],["pa","pa","pa","rrb","pa","rrb","pa","pa","rrb","pa","rrb","pa","pa","pa"]),[],[(Desc,10)])
>>> mergeLeft1Asc (makeStackPair' [3,4,6,7, 9,8,5,2,1,0] [], []) 4 6
([] [9,8,7,6,5,4,3,2,1,0],["pa","pa","pa","rrb","pa","rrb","pa","pa","rrb","pa","rrb","pa","pa","pa"])

-}

sweepLeft :: (StackPair, [String]) -> [(SortOrder, Int)] -> ((StackPair, [String]), [(SortOrder, Int)])
sweepLeft s lengths = iter s lengths []
  where
    iter s []       lengthsR = (s, lengthsR)
    iter s lengthsL lengthsR = iter s' lengthsL' lengthsR'
      where
        (s', lengthsL', lengthsR') = sweepLeftIter s lengthsL lengthsR

sweepRight :: (StackPair, [String]) -> [(SortOrder, Int)] -> ((StackPair, [String]), [(SortOrder, Int)])
sweepRight s lengths = iter s [] lengths
  where
    iter s lengthsL []       = (s, lengthsL)
    iter s lengthsL lengthsR = iter s' lengthsL' lengthsR'
      where
        (s', lengthsL', lengthsR') = sweepRightIter s lengthsL lengthsR

{-
>>> sweepLeft (makeStackPair' [1,5,7,3,4,6,2,0,8,9] [], []) [2,2,2,2,2] True
Couldn't match expected type ‘Bool -> t’
            with actual type ‘((StackPair, [String]), [(SortOrder, Int)])’
>>> sweepRight (makeStackPair' [] [3,4,6,7,5,2,1,0,8,9], []) [4,4,2] True
Couldn't match expected type ‘Bool -> t’
            with actual type ‘((StackPair, [String]), [(SortOrder, Int)])’
>>> sweepLeft (makeStackPair' [3,4,6,7,9,8,5,2,1,0] [], []) [6,4] True
Couldn't match expected type ‘Bool -> t’
            with actual type ‘((StackPair, [String]), [(SortOrder, Int)])’
-}
outerLoop :: (StackPair, [String]) -> (StackPair, [String])
outerLoop s = iter initState lengths []
  where
    (initState, lengths) = sweepRight1 s
    iter s []       [_]      = s
    iter s lengthsL []       = iter s' [] lengthsR'
      where
        (s', lengthsR') = sweepLeft s lengthsL
    iter s []       lengthsR = iter s' lengthsL' []
      where
        (s', lengthsL') = sweepRight s lengthsR
    iter _ _ _ = undefined
---------

spCompact :: [String] -> [String] -> [String]
spCompact ops [] = ops
spCompact [] (b: bs) = spCompact [b] bs
spCompact (a : as) (b : bs)
  | a == "pa" && b == "pb" = spCompact as bs
  | a == "pb" && b == "pa" = spCompact as bs
  | otherwise              = spCompact (b : a : as) bs


solve :: StackPair -> Maybe (StackPair, [String])
solve sp = Just (sp', spCompact [] ops')
  where
    (sp', ops') = outerLoop (sp, [])

{-
>>> solve (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9])
Just ([] [9,8,7,6,5,4,3,2,1,0],["sa","pb","pb","pb","pb","pb","pb","sa","pb","rrb","pa","pa","rrb","pa","rrb","rra","pb","rra","pb","pb","rrb","pa","rrb","pa","pa","rrb","pa","rrb","pa","pa","pa"])
>>> solve (makeStackPair' [] [11,18,3,7,6,12,17,9,13,14,5,10,19,16,4,8,2,0,15,1])
Just ([] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19],["pb","pb","sa","pb","pb","pb","pb","pb","pb","pb","pb","sa","pb","pb","sa","pb","pb","sa","pb","pb","sa","pb","pb","pb","rrb","pa","pa","rrb","pa","rrb","pa","rrb","pa","pa","pa","pa","rrb","pa","pa","rrb","pa","pa","rrb","pa","pa","rrb","pa","pa","pa","rrb","pb","pb","rra","pb","pb","rra","pb","rra","pb","pb","pb","rra","pb","pb","pb","pb","rra","pb","pb","pb","rra","pb","rra","pa","pa","pa","pa","pa","pa","pa","pa","pa","pa","pa","rrb","pa","rrb","pa","pa","rrb","pa","pa","rrb","pa","pa","rra","pb","pb","rra","pb","rra","pb","rra","pb","pb","rra","pb","rra","pb","rra","pb","pb","pb","pb","rra","pa","pa","pa","pa","pa","pa","pa","pa","pa","pa","pa","pa"])
>>> sweepRight1 (makeStackPair' [] [11,18,3,7,6,12,17,9,13,14,5,10,19,16,4,8,2,0,15,1], [])
(([11,18,7,3,6,12,17,9,13,14,10,5,16,19,8,4,0,2,15,1] [],["pb","pb","pb","pb","sa","pb","pb","sa","pb","pb","sa","pb","pb","sa","pb","pb","pb","pb","pb","pb","pb","pb","sa","pb","pb"]),[(Desc,2),(Asc,2),(Desc,2),(Asc,2),(Desc,2),(Asc,2),(Desc,2),(Asc,2),(Desc,2),(Asc,2)])
>>> sweepLeft (makeStackPair' [11,18,7,3,6,12,17,9,13,14,10,5,16,19,8,4,0,2,15,1] [], []) [2,2,2,2,2,2,2,2,2,2]
No instance for (Num (SortOrder, Int)) arising from the literal ‘2’
>>> sweepRight (makeStackPair' [] [14,13,10,5, 9,16,17,19, 12,8,6,4, 0,2,3,7, 18,15,11,1], []) [4,4,4,4,4]
No instance for (Num (SortOrder, Int)) arising from the literal ‘4’

adadadadad
            dadad


-}
