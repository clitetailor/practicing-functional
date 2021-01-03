module Main where

import RedBlackTree as T

main :: IO ()
main = do
  rootNode <- T.newNode T.Red 2

  n1 <- T.newNode T.Red (-1)
  n2 <- T.newNode T.Red (4)
  n3 <- T.newNode T.Black 5

  putStrLn $ show $ foldl insertNode rootNode [n1, n2, n3]
