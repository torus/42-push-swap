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


recOp :: [Char] -> (StackPair -> StackPair) -> (StackPair, [[Char]]) -> (StackPair, [[Char]])
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

recRepeatOp :: [Char] -> Int -> (StackPair -> StackPair)
            -> (StackPair, [[Char]]) -> (StackPair, [[Char]])
recRepeatOp name n op (sp, ops) = (repeatOp n op sp, replicate n name ++ ops)

topA :: StackPair -> Int
topA sp = head (stackA sp)
topB sp = head (stackB sp)

raRec, rbRec, paRec, pbRec :: (StackPair, [[Char]]) -> (StackPair, [[Char]])
rbRec = recOp "rb" rb
raRec = recOp "ra" ra
paRec = recOp "pa" pa
pbRec = recOp "pb" pb

psPartition :: Int -> (StackPair, [[Char]]) -> (StackPair, [[Char]])
psPartition m (sp, ops)
    | m == 0    = (sp, ops)
    | c == 0    = (next . rbRec . goback)                 (sp', ops')
    | otherwise = (next . rbRec . goback . psPartition c) (sp', ops')
    where
--        (sp', c, ops')  = psPartitionIter    0           (pivot (stackA sp)) (sp, ops)
        (sp', c, ops')  = psPartitionIter    0           (topA sp) (sp, ops)
        goback          = psPartitionIterRev (m - c - 1) (topB sp')
        next            = psPartition (m - c - 1)
        pivot as        = medianOfMedians as

median5 :: [Int] -> Int
median5 as
    | odd (length as) = sort as !! (length as `div` 2)
    | otherwise       = sort as !! ((length as `div` 2) - 1)

medianOfMedians :: [Int] -> Int
medianOfMedians as
    | length as > 5 = medianOfMedians $ map median5 (split5 as)
    | otherwise     = median5 as

split5 :: [Int] -> [[Int]]
split5 as
    | length as > 5 = take 5 as : split5 (drop 5 as)
    | otherwise     = [as]

{-
>>> psPartition 7 ((makeStackPair' [] [1,2,3,4,5,6,7]), [])
>>> psPartition 10 ((makeStackPair' [] [1,2,3,4,5,6,7,8,9,10]), [])
([1,2,3,4,5,6,7] [],["rb","rb","rb","rb","rb","rb","rb","pb","ra","pb","ra","ra","pb","ra","ra","ra","pb","ra","ra","ra","ra","pb","ra","ra","ra","ra","ra","pb","ra","ra","ra","ra","ra","ra","pb"])
([1,2,3,4,5,6,7,8,9,10] [],["rb","rb","rb","rb","rb","rb","rb","rb","rb","rb","pb","ra","pb","ra","ra","pb","ra","ra","ra","pb","ra","ra","ra","ra","pb","ra","ra","ra","ra","ra","pb","ra","ra","ra","ra","ra","ra","pb","ra","ra","ra","ra","ra","ra","ra","pb","ra","ra","ra","ra","ra","ra","ra","ra","pb","ra","ra","ra","ra","ra","ra","ra","ra","ra","pb"])


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


psPartitionIter :: Int -> Int -> (StackPair, [[Char]]) -> (StackPair, Int, [[Char]])
psPartitionIter c p (sp, ops) =
    if c < length (stackA sp) then
        if topA sp > p then
            psPartitionIter (c + 1) p $ raRec (sp, ops)
        else
            psPartitionIter c       p $ pbRec (sp, ops)
    else
        (sp, c, ops)

psPartitionIterRev :: Int -> Int -> (StackPair,  [[Char]]) -> (StackPair, [[Char]])
psPartitionIterRev 0 p (sp, ops) = (sp, ops)
psPartitionIterRev c p (sp, ops) =
        if not (null (stackA sp)) && topA sp > p then
            psPartitionIterRev (c - 1) p $ paRec $ raRec (sp, ops)
        else
            psPartitionIterRev (c - 1) p $ paRec (sp, ops)

makeStackPair' :: [Int] -> [Int] -> StackPair
makeStackPair' l r = StackPair (r, reverse l)

solve :: StackPair -> Maybe (StackPair, [[Char]])
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
Nothing
-}
