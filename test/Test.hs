module Main where

import qualified Data.ByteString as B
import Test.Hspec
import Data.Bioparser

main :: IO ()
main = hspec $ do

    describe "Data.Bioparser.decodeFastq" $
      it "can compose properly with encodeFastq" $ do
            fq <- B.readFile "test/SP1.fq"
            let d = decodeFastq fq
            let e = decodeFastq . encodeFastq $ d
            e `shouldBe` d

    describe "Data.Bioparser.decodeFasta" $
        it "can compose properly with encodeFasta" $ do
            fs <- B.readFile "test/sample.fasta"
            let d = decodeFasta fs
            let e = decodeFasta . encodeFasta $ d
            e `shouldBe` d
