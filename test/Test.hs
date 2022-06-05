module Main(main) where

import Test.Hspec
import CKY
import Data.Array

main :: IO ()
main = hspec $ do
    tests

tests :: Spec
tests = do
    describe "standard" $ do
        it "sample" $
            shouldBe "a" "a"
        it "sentence" $
            shouldBe (length sentence) 8
        it "genArr" $
            shouldBe (genArr 2 ! (2,2)) [Z]
        it "genArr 2" $
            shouldBe (genArr 8 ! (8,8)) [Z]
        it "zipWithIndex s" $
            shouldBe (last $ zipWithIndex sentence) (8,"drawer")
        it "running 26" $
            shouldBe (head $ running (2,6)) ((2,2),(3,6))
        it "running 26 2" $
            shouldBe (last $ running (2,6)) ((2,5),(6,6))

