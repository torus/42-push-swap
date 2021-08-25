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

---------

solve :: StackPair -> Maybe (StackPair, [String])
solve = undefined

{-
>>> solve (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9])
Just ([] [0,1,2,3,4,5,6,7,8,9],["ra","pb","ra","pb","pb","ra","pb","pb","ra","ra","pb","pb","pb","ra","ra","pb","ra","pb","rb","rb","pa","rb","pa","pb","ra","pb","rb","rb","pa","pa","rb","pa","ra","pa","pb","pb","ra","ra","pb","ra","pb","rb","rb","pa","rb","pb","rb","pa","pa","pa","pa","pa","pa","pa","pa","pa","pa"])
-}

----------

spPartitionRight :: Int -> Int -> (StackPair, [String]) -> (StackPair, [String])
spPartitionRight 0 _ s = s
spPartitionRight n pivot (sp, ops)
  | topA sp <= pivot = spPartitionRight (n - 1) pivot $ rbRec $ pbRec (sp, ops)
  | otherwise        = spPartitionRight (n - 1) pivot $ raRec (sp, ops)

spPartitionLeft :: Int -> Int -> (StackPair, [String]) -> (StackPair, [String])
spPartitionLeft 0 _ s = s
spPartitionLeft n pivot (StackPair (as, []), ops) = (StackPair (as, []), ops)
spPartitionLeft n pivot (sp, ops)
  | topB sp <= pivot = spPartitionLeft (n - 1) pivot $ rbRec         (sp, ops)
  | otherwise        = spPartitionLeft (n - 1) pivot $         paRec (sp, ops)

{-
>>> spPartitionRight 10 4 (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9], [])
([2,0,4,3,1] [5,7,6,8,9],["ra","ra","rb","pb","rb","pb","ra","rb","pb","rb","pb","ra","rb","pb","ra"])
>>> spPartitionRight 5 2 (makeStackPair' [] [2,0,4,3,1,5,7,6,8,9], [])
([1,0,2] [5,7,6,8,9,4,3],["rb","pb","ra","ra","rb","pb","rb","pb"])
>>> spPartitionLeft 5 2 (makeStackPair' [2,0,4,3,1] [5,7,6,8,9], [])
([2,0,1] [4,3,5,7,6,8,9],["rb","rb","pa","pa","rb"])
-}
