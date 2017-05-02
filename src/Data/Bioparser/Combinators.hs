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

-- | Parsing a single fasta record, i.e defline <*> rawSeq

fastaRecord :: Parser ([Word8], [Word8])
fastaRecord = (\d r -> (d,r)) <$> defline <*> rawSeq

parseFasta :: Parser (Vector ([Word8], [Word8]))
parseFasta = V.fromList <$> many fastaRecord <* endOfInput

fastqRecord :: Parser ([Word8], [Word8], [Word8], [Word8])
fastqRecord = (\a b c d -> (a,b,c,d))
          <$> defline
          <*> rawSeq
          <*> plusline
          <*> scoreline

parseFastq :: Parser (Vector ([Word8], [Word8], [Word8], [Word8]))
parseFastq = V.fromList <$> many fastqRecord

