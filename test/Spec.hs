import Criterion.Main

import qualified Data.ByteString as B
import Data.Bioparser
import Data.Bioparser.Types
import Data.Vector

main :: IO ()
main = defaultMain [
   bench "fasta-parser-1" $ 
     nfIO $ fmap decodeFasta (B.readFile "test/DMproteome.fasta")
 , bench "fasta-parser-2" $
     nfIO $ fmap decodeFasta (B.readFile "test/HSproteome.fasta")
 , bench "fasta-parser-1+2" $
     nfIO $ fmap decodeFasta (B.readFile "test/Gproteome.fasta")
 , bench "fileInput" $
     nfIO $ B.readFile "test/Gproteome.fasta"
 , bench "decEncDec" $
       nfIO $ fmap decEncDec (B.readFile "test/DMproteome.fasta")
        ]

decEncDec :: B.ByteString -> Data.Vector.Vector Data.Bioparser.Types.FastaRecord
decEncDec = decodeFasta . encodeFasta . decodeFasta
