{- |
   Module      : Data.Bioparser.Combinators
   Maintainer  : Alind Gupta <alind.gupta@mail.utoronto.ca>
   Stability   : 0.1
   Portability : portable
This module exports methods for parsing bytestrings.
Currently supports fasta and (Sanger) fastq files.

TODO: Support for SAM/BAM.
-}

module Data.Bioparser.Combinators
    ( parseFasta       
    , parseFastq
    ) where

import Control.Applicative
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Attoparsec.ByteString (Parser)

import Data.Bioparser.Prim
import Data.Bioparser.Types (FastaRecord(..), FastqRecord(..))
  
-- | Parse a single fasta record, i.e defline-sequence
fastaRecord :: Parser FastaRecord
fastaRecord = curry FastaRecord <$> fastaDefline <*> multSeqFasta

-- | Parse multiple fasta records and store as a vector
parseFasta :: Parser (Vector FastaRecord)
parseFasta = V.fromList <$> many fastaRecord

-- | Parse a single fastq record, i.e. defline-sequence-plusline-scores
fastqRecord :: Parser FastqRecord
fastqRecord = (\a b c d -> FastqRecord (a,b,c,d))
  <$> fastqDefline
  <*> multSeqFastq
  <*> plusLine
  <*> scoreLine

-- | Parse multiple fastq records and store as a vector
parseFastq :: Parser (Vector FastqRecord)
parseFastq = V.fromList <$> many fastqRecord
