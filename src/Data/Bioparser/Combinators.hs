-- |
--      Module: Data.Bioparser.Combinators
--
--      High-level Parser combinators for FASTA and FASTQ formats
--      

module Data.Bioparser.Combinators
    ( parseFasta        -- * parse .fs, .fasta file formats
    , parseFastq        -- * parse .fq, .fastq file formtas
    ) where

import Control.Applicative
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Attoparsec.ByteString (Parser, endOfInput)

import Data.Bioparser.Prim
import Data.Bioparser.Util (FastaRecord(..), FastqRecord(..))


-- | Parse a single fasta record, i.e defline-sequence
fastaRecord :: Parser FastaRecord
fastaRecord = curry FastaRecord <$> fastaDefline <*> multSeq

-- | Parse multiple fasta records and store as a vector
parseFasta :: Parser (Vector FastaRecord)
parseFasta = V.fromList <$> many fastaRecord -- <* endOfInput

-- | Parse a single fastq record, i.e. defline-sequence-plusline-scores
fastqRecord :: Parser FastqRecord
fastqRecord = (\a b c d -> FastqRecord (a,b,c,d))
          <$> fastqDefline
          <*> multSeq
          <*> plusLine
          <*> scoreLine

-- | Parse multiple fastq records and store as a vector
parseFastq :: Parser (Vector FastqRecord)
parseFastq = V.fromList <$> many fastqRecord

