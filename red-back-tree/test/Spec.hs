module Main where

import RedBlackTree as T
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ spec

spec :: SpecWith ()
spec = do
  describe "RedBlackTree" $ do
    describe "getParent" $ do
      it "returns parent node" $ do
        node <- T.newNode T.Red 1

        T.getParent node `shouldBe` Nothing
