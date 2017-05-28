import Criterion.Main

import Control.DeepSeq
import qualified Data.ByteString as B
import Data.Bioparser
import Data.Bioparser.Combinators
import Data.Bioparser.Types
import Data.Bioparser.Prim
import qualified Data.Vector as V
import Data.Attoparsec.ByteString (parse, parseOnly)
import Control.Applicative


main = do
    defaultMain [
          bench "fasta-parser-1" $ 
            nfIO $ fmap decodeFasta (B.readFile "test/DMproteome.fasta")
        , bench "fasta-parser-2" $
            nfIO $ fmap decodeFasta (B.readFile "test/HSproteome.fasta")
        , bench "fasta-parser-1+2" $
            nfIO $ fmap decodeFasta (B.readFile "test/Gproteome.fasta")
        , bench "fileInput" $
            nfIO $ B.readFile "test/Gproteome.fasta"
        -- , bench "decEncDec" $
            -- nfIO $ fmap decEncDec (B.readFile "test/DMproteome.fasta")
        ]


-- decEncDec content = (decodeFasta . encodeFasta) <$> decodeFasta content
    
    
