module Main where

import System.Environment
import PushSwap

exec' ss = do { print $ length ss
             ; print ops
             ; print sp'
             ; print $ length ops }
             where (sp', ops) = solve (StackPair (ss, []))

exec ss = do { mapM putStrLn ops}
    where (sp', ops) = solve (StackPair (ss, []))

main :: IO ()
main = do
    { args <- getArgs
    ; let nums = map read args :: [Int]
--    ; print nums
    ; exec nums
    ; return ()
    }
