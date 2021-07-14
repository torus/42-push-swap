module PushSwap where

import Data.List (sortOn)
import Data.Lists (mergeBy)
import Control.Monad.ST
import Data.Array.ST
import Data.STRef

newtype StackPair = StackPair ([Int], [Int]) deriving (Show, Eq)

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

stackHash :: [Int] -> Int
stackHash [] = 0
stackHash (a : as) = (a + stackHash as * 256) `mod` 100000

hash :: StackPair -> Int
hash (StackPair (a, b)) = (stackHash a + stackHash b) `mod` 100000

{-
>>> stackHash [10, 20, 30]
71210
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in sa theStackPair
([2,1,3,4,5],[6,7,8,9,10])
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in sb theStackPair
([1,2,3,4,5],[7,6,8,9,10])
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in ss theStackPair
([2,1,3,4,5],[7,6,8,9,10])
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in pa theStackPair
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in pb theStackPair
([6,1,2,3,4,5],[7,8,9,10])
([2,3,4,5],[1,6,7,8,9,10])
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in ra theStackPair
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in rb theStackPair
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in rr theStackPair
([2,3,4,5,1],[6,7,8,9,10])
([1,2,3,4,5],[7,8,9,10,6])
([2,3,4,5,1],[7,8,9,10,6])
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in rra theStackPair
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in rrb theStackPair
>>> let theStackPair = ([1 .. 5], [6 .. 10]) in rrr theStackPair
([5,1,2,3,4],[6,7,8,9,10])
([1,2,3,4,5],[10,6,7,8,9])
([5,1,2,3,4],[10,6,7,8,9])
 -}

scoreA :: [Int] -> Int
scoreA s = iter s 0 (-1)
    where
        iter [] _ _ = 0
        iter (a : as) i b = (if b < a then 0 else b - a) + iter as (i + 1) a

scoreB :: [Int] -> Int
scoreB s = iter s (length s - 1) $ length s
    where
        iter [] _ _ = 0
        iter (a : as) i b = (if b > a then 0 else a - b) + iter as (i - 1) a

score :: StackPair -> Int
score (StackPair (a, b)) = scoreA a + scoreB b + length b

{-
>>> scoreA [0,1,2,3,4]
>>> scoreA [1,2,3,4,0]
>>> scoreA [0,1,3,2,4]
>>> scoreA [1,0,3,2,4]
0
20
2
4
>>> scoreB [0,1,2,3,4]
>>> scoreB [4,3,2,1,0]
40
20
38
36
0
 -}

newtype Move = Move ([Char], StackPair -> StackPair)

instance Show Move where
    show (Move (a, b)) = show a

ops :: [Move]
ops = [ Move ("sa", sa),   Move ("sb", sb),   Move ("ss", ss)
      , Move ("pa", pa),   Move ("pb", pb)
      , Move ("ra", ra),   Move ("rb", rb),   Move ("rr", rr)
      , Move ("rra", rra), Move ("rrb", rrb), Move ("rrr", rrr)]

moves :: StackPair -> [Move]
moves p = ops

move :: StackPair -> Move -> Position
move p (Move m) = snd m p

{-
>>> moves (StackPair ([4,0,1,2,3],[]))
[("ra",0,StackPair ([0,1,2,3,4],[])),("rr",0,StackPair ([0,1,2,3,4],[])),("sa",12,StackPair ([0,4,1,2,3],[])),("ss",12,StackPair ([0,4,1,2,3],[])),("pb",17,StackPair ([0,1,2,3],[4])),("sb",20,StackPair ([4,0,1,2,3],[])),("pa",20,StackPair ([4,0,1,2,3],[])),("rb",20,StackPair ([4,0,1,2,3],[])),("rrb",20,StackPair ([4,0,1,2,3],[])),("rra",30,StackPair ([3,4,0,1,2],[])),("rrr",30,StackPair ([3,4,0,1,2],[]))]
>>> moves (StackPair ([0,1,2],[4,3]))
[("sa",22,StackPair ([1,0,2],[4,3])),("sb",22,StackPair ([0,1,2],[3,4])),("rb",22,StackPair ([0,1,2],[3,4])),("rrb",22,StackPair ([0,1,2],[3,4])),("ss",24,StackPair ([1,0,2],[3,4])),("ra",26,StackPair ([1,2,0],[4,3])),("rra",26,StackPair ([2,0,1],[4,3])),("pb",27,StackPair ([1,2],[0,4,3])),("rr",28,StackPair ([1,2,0],[3,4])),("rrr",28,StackPair ([2,0,1],[3,4])),("pa",29,StackPair ([4,0,1,2],[3]))]
-}

makeHashTable :: ST s (STArray s Int [Position])
makeHashTable = do
    newArray (0, 100000 - 1) []

type Position = StackPair

type Path = ([Move], Position, Int)
type Frontier = [Path]

solved :: Position -> Bool
solved p = score p == 0

solve :: Position -> (Maybe [Move], Int)
solve start = (solution, total)
    where
        (solution, total) = runST
            $ do { pa <- makeHashTable
                 ; n <- newSTRef (0 :: Int)
                 ; result <- bfs pa [([], start, score start)] []
                 ; elems <- getElems pa
                 ; let total = foldr k 0 elems
                 ; return (result, total)
                 }
                where
                    k as m = m + length as

totalCost :: Path -> Int
totalCost (_, _, c) = c
--totalCost _ = 0

bfs :: STArray s Int [Position] -> Frontier -> Frontier -> ST s (Maybe [Move])
bfs pa [] [] = return Nothing
bfs pa [] mqs = bfs pa mqs []
bfs pa ((ms, p, c) : mps) mqs
    = if solved p then return (Just (reverse ms))
      else do { ps <- readArray pa k
              ; if p `elem` ps then bfs pa mps mqs
              ; else do { writeArray pa k (p : ps)
                        ; bfs pa mps
                              $ mergeBy cmp (succs (ms, p, c)) mqs
                        }
              }
    where
        k = hash p
        cmp a b = compare (totalCost a) (totalCost b)

succs :: Path -> [Path]
succs (ms, p, c) = [(m : ms, move p m, score (move p m) + length ms + 1)
                    | m <- moves p]

{-
>>> solve (StackPair ([0,1,2,3,4,5],[]))
>>> solve (StackPair ([1,0,2,3,4,5],[]))
>>> solve (StackPair ([1,0,2,3,5,4],[]))
Just []
Just ["sa"]
Just ["rra","rra","sa","ra","ra","sa"]

-}
