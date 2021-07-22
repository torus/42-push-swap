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

topA :: StackPair -> Int
topA sp = head (stackA sp)

psPartition :: StackPair -> Int -> StackPair
psPartition sp m
    | m == 0    = sp
    | c == 0    = psPartition (repeatOp (m - 1) pb sp') (m - 1)
    | c == 1    = psPartition ((repeatOp (m - 1) pb . rb . pb) sp') (m - 1)
    | otherwise = psPartition sp' c
    where
        p = topA sp
        (sp', c) = psPartitionIter sp 0 p

psPartitionIter :: StackPair -> Int -> Int -> (StackPair, Int)
psPartitionIter sp c p =
    if c < length (stackA sp) then
        if topA sp > p then
            psPartitionIter (ra sp) (c + 1) p
        else
            psPartitionIter (pb sp) c p
    else
        (sp, c)

makeStackPair' :: [Int] -> [Int] -> StackPair
makeStackPair' l r = StackPair (r, reverse l)

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
[6,3,2,1,0,4,5] []
 -}
