{-# LANGUAGE ScopedTypeVariables #-}
module Tests.BinTreeTests where

import Data.Char (isDigit)
import Data.List (foldl', nub, sort)
import qualified Test.QuickCheck      as QC
import qualified E06_BinTree as BinTree
import E06_BinTree
import Test.Hspec

tests = do
    describe "BinTree.insert" $ do
        it "insert 1 into an empty tree" $ do
            BinTree.insert (1 :: Int) Nil `shouldBe` Node 1 Nil Nil

    describe "BinTree.insert" $ do
        it "insert 2 1 3 4 into an empty tree" $ do
            BinTree.insert (4 :: Int) (BinTree.insert 1 (BinTree.insert 3 (BinTree.insert 2 Nil)))
              `shouldBe` Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil))

    describe "BinTree.insert" $ do
        it "make sure duplicates are ignored" $ do
            BinTree.insert (3 :: Int) (BinTree.insert 1 (BinTree.insert 3 (BinTree.insert 2 Nil)))
              `shouldBe` Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)

    describe "BinTree.inorder" $ do
        it "make sure an in-order traversal ouputs a sorted list" $ do
            let propSorted (xs :: [Int]) = BinTree.inorder (foldl' (flip BinTree.insert) Nil xs) == (sort . nub) xs
             in QC.quickCheck propSorted

main = hspec tests
