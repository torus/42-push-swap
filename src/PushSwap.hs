module PushSwap where

import Data.List
import Data.Lists
import Control.Monad.ST
import Data.Array.ST
import Data.STRef

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

raRec, rbRec, paRec, pbRec, saRec, sbRec, ssRec :: (StackPair, [String]) -> (StackPair, [String])
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

spPartitionRight :: Int -> Int -> Int -> (StackPair, [String]) -> ((StackPair, [String]), Int)
spPartitionRight 0 m _ s = (s, m)
spPartitionRight n m pivot (sp, ops)
  | topA sp <= pivot = spPartitionRight (n - 1) m       pivot $ rbRec $ pbRec (sp, ops)
  | otherwise        = spPartitionRight (n - 1) (m + 1) pivot $         pbRec (sp, ops)

spPartitionRight' :: Int -> Int -> (StackPair, [String]) -> ((StackPair, [String]), Int)
spPartitionRight' n pivot s@(sp, ops) = (sweepLeft m (sp', ops'), m)
  where
      ((sp', ops'), m) = spPartitionRight n 0 pivot s

sweepLeft :: Int -> (StackPair, [String]) -> (StackPair, [String])
--sweepLeft m (sp, ops) = recRepeatOp "pa" m pa (sp, ops)
sweepLeft 0 s = s
{-
sweepLeft m s@(StackPair (a1 : a2 : as, b1 : b2 : bs), ops)
  | b2 > b1 && a1 > a2 = sweepLeft (m - 1) $ paRec $ ssRec s
  | b2 > b1            = sweepLeft (m - 1) $ paRec $ sbRec s
  |            a1 > a2 = sweepLeft (m - 1) $ paRec $ saRec s
  | otherwise          = sweepLeft (m - 1) $ paRec         s
-}
sweepLeft m s@(StackPair (as, b1 : b2 : bs), ops)
  | b2 > b1            = sweepLeft (m - 1) $ paRec $ sbRec s
  | otherwise          = sweepLeft (m - 1) $ paRec         s
{-
sweepLeft m s@(StackPair (a1 : a2 : as, bs), ops)
  |            a1 > a2 = sweepLeft (m - 1) $ paRec $ saRec s
  | otherwise          = sweepLeft (m - 1) $ paRec         s
-}
sweepLeft m s
                       = sweepLeft (m - 1) $ paRec         s
-- ...bs b2 b1 a1 a2 as...

spPartitionLeft :: Int -> Int -> Int -> (StackPair, [String]) -> ((StackPair, [String]), Int)
spPartitionLeft 0 m _ s = (s, m)
spPartitionLeft n m pivot s@(StackPair (as, []), ops) = (s, m)
spPartitionLeft n m pivot s@(sp, ops)
  | all (<= pivot) (stackB sp) = (s, m)
  | topB sp <= pivot = spPartitionLeft (n - 1) m       pivot $ rbRec s
  | otherwise        = spPartitionLeft (n - 1) (m + 1) pivot $ paRec s

spPartitionIterLeft :: (StackPair, [String]) -> (StackPair, [String])
spPartitionIterLeft s@(StackPair (as, []), ops) = s
spPartitionIterLeft s@(StackPair (as, [b]), ops) = raRec $ paRec s
spPartitionIterLeft s@(sp, ops)
 | all (> topB sp) (tail $ stackB sp) = raRec $ paRec s
 | otherwise  = fst $ spPartitionRight' m (medianOfMedians $ take m $ stackA sp)
    $ spPartitionIterLeft s'
    where
        (s'@(sp', ops'), m)
            = spPartitionLeft (length (stackA sp)) 0 (medianOfMedians $ stackB sp) s

{-
>>> spPartitionRight' 10 4 (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9], [])
(([2,0,4,3,1] [5,6,7,8,9],["pa","pa","pa","sb","pa","pa","pb","pb","rb","pb","rb","pb","pb","rb","pb","rb","pb","pb","rb","pb","pb"]),5)
>>> spPartitionRight' 5 2 (makeStackPair' [] [2,0,4,3,1,5,7,6,8,9], [])
(([1,0,2] [3,4,5,7,6,8,9],["pa","pa","sb","rb","pb","pb","pb","rb","pb","pb"]),2)
>>> spPartitionLeft 5 0 2 (makeStackPair' [2,0,4,3,1] [5,7,6,8,9], [])
(([1,2,0] [4,3,5,7,6,8,9],["pa","pa","rb"]),2)
>>> spPartitionLeft 3 0 1 (makeStackPair' [2,0,1] [4,3,5,7,6,8,9], [])
(([0,1] [2,4,3,5,7,6,8,9],["pa","rb","rb"]),1)
>>> spPartitionLeft 2 0 0 (makeStackPair' [0,1] [2,4,3,5,7,6,8,9], [])
(([0] [1,2,4,3,5,7,6,8,9],["pa"]),1)
>>> spPartitionIterLeft (makeStackPair' [0] [1,2,4,3,5,7,6,8,9], [])
([] [1,2,4,3,5,7,6,8,9,0],["ra","pa"])

>>> spPartitionIterLeft (makeStackPair' [2,0,4,3,1] [5,7,6,8,9], [])
([3,4,1,2] [5,7,6,8,9,0],["rb","pb","rb","pb","ra","pa","pa","pa","rb"])
>>> spPartitionIterLeft (makeStackPair' [3,4,2,1] [5,7,6,8,9,0], [])
([3,4,2] [5,7,6,8,9,0,1],["ra","pa"])
>>> spPartitionIterLeft (makeStackPair' [4,3,2] [5,7,6,8,9,0,1], [])
([4,3] [5,7,6,8,9,0,1,2],["ra","pa"])
>>> spPartitionIterLeft (makeStackPair' [4,3] [5,7,6,8,9,0,1,2], [])
([4] [5,7,6,8,9,0,1,2,3],["ra","pa"])
>>> spPartitionIterLeft (makeStackPair' [4] [5,7,6,8,9,0,1,2,3], [])
([] [5,7,6,8,9,0,1,2,3,4],["ra","pa"])
>>> spPartitionRight' 5 7 (makeStackPair' [] [5,7,6,8,9,0,1,2,3,4], [])
(([6,7,5] [8,9,0,1,2,3,4],["pa","pa","pb","pb","rb","pb","rb","pb","pb"]),2)
-}

leftLoop :: (StackPair, [String]) -> (StackPair, [String])
leftLoop s@(StackPair (as, []), ops) = s
leftLoop s = leftLoop $ spPartitionIterLeft s

outerLoop :: Int -> (StackPair, [String]) -> (StackPair, [String])
outerLoop 0 sp = sp
outerLoop m s@(StackPair (as, bs), ops)
  = outerLoop m'' s'''
    where
      (s'@(sp', ops'), m')        = spPartitionRight' m (medianOfMedians $ take m as) s
      s''@(sp'', ops'')           = leftLoop s'
      (s'''@(sp''', ops'''), m'') = sweepRight m' s''

sweepRight :: Int -> (StackPair, [String]) -> ((StackPair, [String]), Int)
sweepRight 0 s = (s, 0)
sweepRight m (StackPair ([], bs), ops) = undefined
sweepRight m s@(StackPair (a : as, bs), ops)
  | all (> a) (take (m - 1) as) = sweepRight (m - 1) $ raRec s
  | otherwise    = (s, m)

{-
>>> spPartitionRight' 10 4 (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9], [])
(([2,0,4,3,1] [5,6,7,8,9],["pa","pa","pa","sb","pa","pa","pb","pb","rb","pb","rb","pb","pb","rb","pb","rb","pb","pb","rb","pb","pb"]),5)
>>> leftLoop (makeStackPair' [2,0,4,3,1] [5,7,6,8,9], [])
([] [5,7,6,8,9,0,1,2,3,4],["ra","pa","ra","pa","ra","pa","rb","pb","rb","pb","pb","ra","pa","pa","pa","pa","rb","rb","rb","pb","rb","pb","ra","pa","pa","pa","rb"])
>>> sweepRight 5 (makeStackPair' [] [5,7,6,8,9,0,1,2,3,4], [])
(([] [7,6,8,9,0,1,2,3,4,5],["ra"]),4)

>>> spPartitionRight' 4 8 (makeStackPair' [] [7,6,8,9,0,1,2,3,4,5], [])
(([8,6,7] [9,0,1,2,3,4,5],["pa","pb","rb","pb","rb","pb","pb"]),1)
>>> leftLoop (makeStackPair' [8,6,7] [9,0,1,2,3,4,5], [])
([] [9,0,1,2,3,4,5,6,7,8],["ra","pa","ra","pa","rb","pb","pb","ra","pa","pa","pa","rb","rb"])
>>> sweepRight 1 (makeStackPair' [] [9,0,1,2,3,4,5,6,7,8], [])
(([] [0,1,2,3,4,5,6,7,8,9],["ra"]),0)
>>> outerLoop 10 (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9], [])
([] [0,1,2,3,4,5,6,7,8,9],["ra","ra","ra","ra","ra","ra","pa","ra","pa","ra","pa","rb","pb","rb","pb","pb","ra","pa","pa","pa","pa","rb","rb","rb","pb","rb","pb","ra","pa","pa","pa","rb","pa","pa","pa","sb","pa","pa","pb","pb","rb","pb","rb","pb","pb","rb","pb","rb","pb","pb","rb","pb","pb"])
>>> length $ snd $ outerLoop 10 (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9], [])
53
>>> length $ spCompact [] $ snd $ outerLoop 10 (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9], [])
49
-}

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
    (sp', ops') = outerLoop (length $ stackA sp) (sp, [])

{-
>>> solve (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9])
Just ([] [0,1,2,3,4,5,6,7,8,9],["pb","pb","rb","pb","pb","rb","pb","rb","pb","pb","rb","pb","rb","sb","pa","pa","pa","rb","pa","pa","pa","ra","pb","rb","pb","rb","rb","rb","pa","pa","pa","pa","ra","pb","pb","rb","pb","rb","pa","ra","pa","ra","pa","ra","ra","ra","ra","ra","ra"])
-}
