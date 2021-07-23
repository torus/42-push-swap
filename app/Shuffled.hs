module Main where

import Data.List
import Data.String
import System.Random
import System.Random.Shuffle
import System.Environment

main :: IO ()
main = do
    { args <- getArgs
    ; let num = read (head args) :: Int
    ; gen <- newStdGen 
    ; putStrLn $ intercalate "" $ intersperse " " $ map show $ shuffle' [0 .. num - 1] num gen
    ; return ()
    }

{-
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
-}
