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
rbRec = recOp "rb" rb
raRec = recOp "ra" ra
paRec = recOp "pa" pa
pbRec = recOp "pb" pb
saRec = recOp "sa" sa
sbRec = recOp "sb" sb
ssRec = recOp "ss" ss

psPartition :: Int -> (StackPair, [String]) -> (StackPair, [String])
psPartition m (sp, ops)
    | m == 0    = (sp, ops)
    | c == 0    = (next . goback)                 (sp', ops')
    | otherwise = (next . goback . psPartition c) (sp', ops')
    where
        (sp', c, ops')  = psPartitionIter    0       p    (sp, ops)
        goback          = psPartitionIterRev (m - c) p' p
        next            = psPartition (m - c - 1)
        p               = medianOfMedians (stackA sp)
        p'              = medianOfMedians $ take (m - c) (stackB sp')

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

{-
>>> psPartition 10 ((makeStackPair' [] [5,1,7,3,4,6,0,2,8,9]), [])
>>> psPartition 10 ((makeStackPair' [] [1,2,3,4,5,6,7,8,9,10]), [])
([0,1,2,3,4,5,6,7,8,9] [],["rb","pb","pa","rb","rb","rb","pb","ra","pb","sa","ra","sa","ra","pb","pb","sa","pa","ra","pa","rb","pa","pa","rb","rb","pb","ra","pb","sa","pa","rb","pa","rb","rb","pb","ra","pb","sa","ra","sa","pb","sa","ra","sa","pb","sa","pb","sa","ra","sa","ra","ra","pb","sa","pb","sa","ra","sa","pb","sa","pb","sa","ra","pb","sa"])
([1,2,3,4,5,6,7,8,9,10] [],["rb","rb","pb","ra","pb","pa","pa","rb","rb","rb","rb","rb","pb","ra","pb","sa","pa","rb","pa","rb","rb","pb","ra","pb","sa","ra","sa","pb","sa","ra","sa","pb","sa","pb","sa","ra","sa","pb","sa","ra","sa","ra","sa","ra","sa","ra","sa","ra","sa","pb","sa","ra","sa","ra","sa","ra","sa","ra","sa","ra","sa","ra","sa","ra","ra","ra","ra","ra","ra","pb","pb","pb"])

>>> median5 [1]
1

>>> split5 [1,2,3,4,5,6,7,8,9,10]
[[1,2,3,4,5],[6,7,8,9,10]]

>>> map median5 [[1,2,3,4,5],[6,7,8,9,10]]
[3,8]

>>> medianOfMedians [11,12,13,14,15,16,17,18,19,20]
>>> medianOfMedians [1,2,3,4]
>>> medianOfMedians [1,2]
>>> medianOfMedians [1]
13
2
1
1
-}


psPartitionIter :: Int -> Int -> (StackPair, [String]) -> (StackPair, Int, [String])
psPartitionIter c p (sp, ops) = part c p (sp, ops)
    where
        part c p (sp, ops) =
            if c < length (stackA sp) then
                if topA sp' > p then
                    psPartitionIter (c + 1) p $ raRec (sp', ops')
                else
                    psPartitionIter c       p $ pbRec (sp', ops')
            else
                (sp, c, ops)
        (sp', ops') = swap (sp, ops)
        swap (sp, ops)
--            | canSb sp && canSa sp = ssRec (sp, ops)
--            | canSb sp             = sbRec (sp, ops)
            | canSa sp             = saRec (sp, ops)
            | otherwise            = (sp, ops)
        canSb sp = length (stackB sp) > 1 && (topB sp < (stackB sp !! 1))
        canSa sp = length (stackA sp) > 1 && (topA sp > (stackA sp !! 1))

psPartitionIterRev :: Int -> Int -> Int -> (StackPair,  [String]) -> (StackPair, [String])
psPartitionIterRev 0 p pp (sp, ops) = (sp, ops)
psPartitionIterRev c p pp (sp, ops)
    | topB sp == pp = (psPartitionIterRev (c - 1) p pp . rbRec) (sp, ops)
    | otherwise     = op (sp, ops)
    where
        op = if not (null (stackA sp)) && topA sp > p then
                psPartitionIterRev (c - 1) p pp . paRec . raRec
            else
                psPartitionIterRev (c - 1) p pp . paRec

makeStackPair' :: [Int] -> [Int] -> StackPair
makeStackPair' l r = StackPair (r, reverse l)

solve :: StackPair -> Maybe (StackPair, [String])
solve sp
    | valid (stackA sp') = Just (sp', reverse ops)
    | otherwise = Nothing
    where
        (sp', ops) = recRepeatOp "pa" len pa $ psPartition len (sp, [])
        len = length (stackA sp)
        valid [] = True
        valid [x] = True
        valid (x1 : x2 : xs) = x1 < x2 && valid (x2 : xs)

{-
>>> solve (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9])
Just ([] [0,1,2,3,4,5,6,7,8,9],["ra","pb","ra","pb","pb","ra","pb","pb","ra","ra","pb","pb","pb","ra","ra","pb","ra","pb","rb","rb","pa","rb","pa","pb","ra","pb","rb","rb","pa","pa","rb","pa","ra","pa","pb","pb","ra","ra","pb","ra","pb","rb","rb","pa","rb","pb","rb","pa","pa","pa","pa","pa","pa","pa","pa","pa","pa"])
-}
