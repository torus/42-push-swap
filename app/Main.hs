module Main where

import System.Random
import System.Random.Shuffle
import PushSwap

exec ss = do { print $ length ss
             ; print ops
             ; print sp'
             ; print $ length ops }
             where (sp', ops) = solve (StackPair (ss, []))

main :: IO ()
main = do
    { exec [0,1,2,3,4]
    ; exec [1,0,2,3,4]
    ; exec [4,0,1,2,3]
    ; exec [2, 1, 5, 3, 4, 0]
    ; exec [2, 1, 5, 6, 3, 4, 0]
    ; exec [2, 7, 1, 5, 6, 3, 4, 0]
    ; gen <- newStdGen
    ; exec $ shuffle' [0..99] 100 gen
    ; gen <- newStdGen
    ; exec $ shuffle' [0..99] 100 gen

    }
