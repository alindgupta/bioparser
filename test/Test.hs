module Main where

import qualified Data.ByteString as B
import Test.Hspec
import Test.QuickCheck
import Data.Bioparser
import Data.Bioparser.Prim
import Data.Bioparser.Combinators


main :: IO ()
main = hspec $ do

    describe "Data.Bioparser.decodeFastq" $ do
        it "can compose properly with encodeFastq" $ do
            fq <- B.readFile "/Users/Alind/Desktop/SP1.fq"
            let d = decodeFastq fq
            let e = fmap (decodeFastq . encodeFastq) d
            case e of
                Right ed -> ed `shouldBe` d
                _ -> return ()

    describe "Data.Bioparser.decodeFasta" $ do
        it "can compose properly with encodeFasta" $ do
            fs <- B.readFile "test/DMproteome.fasta"
            let d = decodeFasta fs
            let e = fmap (decodeFasta . encodeFasta) d
            case e of
                Right ed -> ed `shouldBe` d
                _ -> return ()


            
            
       
