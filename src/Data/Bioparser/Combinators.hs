-- |
--      Module: Data.Bioparser.Combinators
--
--      Parser combinators for FASTA and FASTQ formats
--      using attoparsec
--
--      Maintainer: Alind Gupta
--      Require parseOnly rather than parse


module Data.Bioparser.Combinators
    ( parseFasta   -- * parse .fs, .fasta file formats
    , parseFastq   -- * parse .fq, .fastq file formtas
    , fastaRecord
    ) where

import Control.Applicative
import Data.Word (Word8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Attoparsec.ByteString (Parser, endOfInput)

import Data.Bioparser.Prim
import Data.Bioparser.Util (FastaRecord(..), FastqRecord(..))

-- | Parsing a single fasta record, i.e defline <*> rawSeq

--fastaRecord :: Parser FastaRecord
fastaRecord = curry FastaRecord <$> defline <*> rawSeq

--parseFasta :: Parser (Vector FastaRecord)
parseFasta = V.fromList <$> many fastaRecord <* endOfInput

--fastqRecord :: Parser FastqRecord
fastqRecord = (\a b c d -> FastqRecord (a,b,c,d))
          <$> defline
          <*> rawSeq
          <*> plusline
          <*> scoreline

--parseFastq :: Parser (Vector FastqRecord)
parseFastq = V.fromList <$> many fastqRecord

