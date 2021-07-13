module Main where

import PushSwap

{-
main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
-}

main :: IO ()
main = do
    { print (solve (StackPair (shuffled,[])))
    }
