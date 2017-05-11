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

fasta1 = seq "" (B.readFile "test/DMproteome.fasta")
fasta2 = seq "" (B.readFile "test/HSproteome.fasta")

main = do
    defaultMain [
          bench "parser1" $ 
            nfIO $ fmap decodeFasta fasta1
        , bench "parser2" $
            nfIO $ fmap decodeFasta fasta2
        ]
