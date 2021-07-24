module PushSwap where

import Data.List (sortOn)
import Data.Lists (mergeBy)
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

rbRec :: (StackPair, [[Char]]) -> (StackPair, [[Char]])
rbRec = recOp "rb" rb
paRec = recOp "pa" pa
raRec = recOp "ra" ra

psPartition :: StackPair -> Int -> [[Char]] -> (StackPair, [[Char]])
psPartition sp m ops
    | m == 0    = (sp, ops)
    | c == 0    = psPartition sp''  (m - 1)     ops''
    | otherwise = psPartition sp''' (m - c - 1) ops'''
    where
        p = topA sp
        (sp', c, ops')  = psPartitionIter sp 0 p ops
        (sp'',  ops'')  = rbRec $ psPartitionIterRev (m - 1) p (sp', ops')
        (sp''', ops''') = rbRec $ psPartitionIterRev (m - c - 1) p $ psPartition sp' c ops'
--      (sp'',  ops'')  = rbRec $ recRepeatOp "pa" (m - 1) pa (sp', ops')
--      (sp''', ops''') = rbRec $ recRepeatOp "pa" (m - c - 1) pa $ psPartition sp' c ops'

psPartitionIter :: StackPair -> Int -> Int -> [[Char]] -> (StackPair, Int, [[Char]])
psPartitionIter sp c p ops =
    if c < length (stackA sp) then
        if topA sp > p then
            psPartitionIter (ra sp) (c + 1) p ("ra" : ops)
        else
            psPartitionIter (pb sp) c p ("pb" : ops)
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
        (sp', ops) = recRepeatOp "pa" len pa $ psPartition sp len []
        len = length (stackA sp)
        valid [] = True
        valid [x] = True
        valid (x1 : x2 : xs) = x1 < x2 && valid (x2 : xs)

{-
>>> repeatOp 3 pb $ makeStackPair' [] [1,2,3,4,5]
>>> makeStackPair' [4,5] [0,1,2,3]
[1,2,3] [4,5]
[4,5] [0,1,2,3]
>>> psPartitionIter (makeStackPair' [] [3,2,1,0,4,5,6]) 0 3
([3,2,1,0] [4,5,6],3)
>>> psPartitionIter (makeStackPair' [3,2,1,0] [4,5,6]) 0 4
>>> psPartition (makeStackPair' [] [3,2,1,0,4,5,6]) 7
([3,2,1,0,4] [5,6],2)
[0,1,2,3,4,5,6] []
 -}
