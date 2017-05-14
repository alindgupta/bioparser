import Criterion.Main

import Control.DeepSeq
import qualified Data.ByteString as B
import Data.Bioparser
import Data.Bioparser.Combinators
import Data.Bioparser.Util
import Data.Bioparser.Prim
import qualified Data.Vector as V
import Data.Attoparsec.ByteString (parseOnly)
import Control.Applicative


main = do
    defaultMain [
          bench "parser1" $ 
            nfIO $ fmap (fmap V.length . decodeFasta) (B.readFile "test/DMproteome.fasta")
        , bench "parser2" $
            nfIO $ fmap decodeFasta (B.readFile "test/hsproteome.fasta")
        , bench "parse1+2" $
            nfIO $ fmap decodeFasta (B.readFile "test/Gproteome.fasta")
        , bench "fileInput" $
            nfIO $ B.readFile "test/Gproteome.fasta"
        ]
