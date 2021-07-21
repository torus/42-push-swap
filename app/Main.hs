module Main where

import PushSwap

shuffled :: [Int]
shuffled = [2, 1, 5, 3, 4, 0]

exec ss = do { print (solve (StackPair (ss, []))) }

main :: IO ()
main = do
    { exec [0,1,2,3,4]
    ; exec [1,0,2,3,4]
    ; exec [4,0,1,2,3]
    ; exec [2, 1, 5, 3, 4, 0]
    ; exec [2, 1, 5, 6, 3, 4, 0]
    --; exec [2, 7, 1, 5, 6, 3, 4, 0]
    }
