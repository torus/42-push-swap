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

sweepRight1 :: (StackPair, [String]) -> ((StackPair, [String]), [Int])
sweepRight1 s = (s', length)
  where
    ((s', length), n) =  iter ((s, []), 0)
    iter ((s@(StackPair (as, bs), ops), lengths), n)
      | null as   = ((s, lengths), n)
      | otherwise = iter $ sweepRightIter (s, lengths) n

sweepRightIter :: ((StackPair, [String]), [Int]) -> Int -> (((StackPair, [String]), [Int]), Int)
sweepRightIter (s@(StackPair (a1 : a2 : as, bs), ops), lengths) n
  | even n && a1 > a2 || odd n && a1 < a2 = ((pbRec $ pbRec $ saRec s, 2 : lengths), n + 1)
  | otherwise                             = ((pbRec $ pbRec s, 2 : lengths), n + 1)
--sweepRightIter (s@(StackPair ([], bs), ops), lengths) n = ((s, lengths), n)
sweepRightIter _ _ = undefined

{-
>>> sweepRightIter ((makeStackPair' [] [5,1,7,3,4,6,0,2,8,9], []), []) 0
((([1,5] [7,3,4,6,0,2,8,9],["pb","pb","sa"]),[2]),1)
>>> sweepRightIter ((makeStackPair' [1,5] [7,3,4,6,0,2,8,9], []), [2]) 1
((([1,5,7,3] [4,6,0,2,8,9],["pb","pb"]),[2,2]),2)
>>> sweepRightIter ((makeStackPair' [1,5,7,3] [4,6,0,2,8,9], []), [2,2]) 2
((([1,5,7,3,4,6] [0,2,8,9],["pb","pb"]),[2,2,2]),3)
>>> sweepRightIter ((makeStackPair' [1,5,7,3,4,6] [0,2,8,9], []), [2,2,2]) 3
((([1,5,7,3,4,6,2,0] [8,9],["pb","pb","sa"]),[2,2,2,2]),4)
>>> sweepRightIter ((makeStackPair' [1,5,7,3,4,6,2,0] [8,9], []), [2,2,2,2]) 4
((([1,5,7,3,4,6,2,0,8,9] [],["pb","pb"]),[2,2,2,2,2]),5)
>>> sweepRightIter ((makeStackPair' [1,5,7,3,4,6,2,0,8,9] [], []), [2,2,2,2,2]) 5
Prelude.undefined

>>> sweepRight1 (makeStackPair' [] [], [])
(([] [],[]),[])
>>> sweepRight1 (makeStackPair' [] [5,1,7,3,4,6,0,2,8,9], [])
(([1,5,7,3,4,6,2,0,8,9] [],["pb","pb","pb","pb","sa","pb","pb","pb","pb","pb","pb","sa"]),[2,2,2,2,2])
-}
outerLoop = undefined
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
Just ([] [0,1,2,3,4,5,6,7,8,9],["pb","pb","rb","pb","pb","rb","pb","rb","pb","ra","pb","rb","pa","pa","pa","rb","rb","pa","rb","rb","pa","pa","pa","ra","ra","ra","ra","ra","pb","ra","pa","ra","ra","ra"])
>>> solve (makeStackPair' [] [11,18,3,7,6,12,17,9,13,14,5,10,19,16,4,8,2,0,15,1])
Just ([] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19],["pb","pb","pb","rb","pb","rb","pb","rb","pb","pb","pb","pb","pb","pb","rb","pb","pb","pb","pb","rb","pb","pb","rb","ra","pb","ra","pa","pa","pa","pa","pa","pa","pa","pa","pa","pa","pa","pa","rb","pa","pa","pa","pa","rb","pa","pa","ra","ra","ra","ra","ra","ra","pb","pb","pb","rb","pb","pb","rb","pb","pb","pb","rb","pb","pb","ra","pa","pa","pa","pa","pa","pa","rb","pa","rb","rb","pa","rb","pa","pa","ra","ra","ra","ra","pb","pb","ra","ra","pb","pb","ra","pa","pa","rb","pb","ra","pa","pa","pa","ra","ra","ra"])
-}
