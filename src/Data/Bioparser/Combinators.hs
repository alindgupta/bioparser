-- |
--      Module: Data.Bioparser.Combinators
--
--      Parser combinators for FASTA and FASTQ formats
--      using attoparsec
--
--      Maintainer: Alind Gupta
--


module Data.Bioparser.Combinators
    ( parseFasta   -- * parse .fs, .fasta file formats
    , parseFastq   -- * parse .fq, .fastq file formtas
    ) where

import Data.Attoparsec.ByteString
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.Vector as V

import Data.Bioparser.Prim
import Data.Bioparser.Util

-- | Parsing a single fasta record, i.e defline <*> rawSeq

fastaRecord = (\d r -> (d,r)) <$> defline <*> rawSeq

parseFasta = V.fromList <$> many fastaRecord <* endOfInput 

fastqRecord = (\a b c d -> (a,b,c,d))
          <$> defline
          <*> rawSeq
          <*> plusline
          <*> scoreline

parseFastq = V.fromList <$> many fastqRecord <* endOfInput

