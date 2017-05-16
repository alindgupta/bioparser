-- |
--      Module: Data.Bioparser.Prim
--
--      Primitive parsers for FASTA and FASTQ files
--      ASCII encoding is assumed      
--

module Data.Bioparser.Prim
    ( fastaDefline          -- * defline beginning with '>'
    , fastqDefline          -- * defline beginning with '@'
    , multSeqFasta          -- * raw sequence for fasta, ignores newlines
    , multSeqFastq          -- * raw sequence for fastq, ignores newline
    , scoreLine             -- * quality scores for fastq
    , plusLine              -- * id for fastq (beginning with '+')
    , endOfLine             -- * handle \r, \n
    ) where

import Data.Word (Word8)
import Data.Attoparsec.ByteString     
import qualified Data.Attoparsec.ByteString as A
import Control.Applicative ((*>), (<*), (<|>))

import Data.Bioparser.Types

-- | Utility function
notEndOfLine :: Word8 -> Bool
notEndOfLine c = (c /= 10) && (c /= 13)
{-# INLINE notEndOfLine #-}

-- | Parses end of line characters '\n' and '\r'
endOfLine :: Parser Word8
endOfLine = A.word8 10 <|> A.word8 13
{-# INLINE endOfLine #-}

-- | Utility function, reads a single line delimited by a newline char
singleLine = A.takeWhile notEndOfLine <* endOfLine
{-# INLINE singleLine #-}


-------------------------------------------------
--      Defline
-------------------------------------------------

-- | Parse defline beginning with symb
defline :: Parser Word8 -> Parser Defline
defline symb = symb *> singleLine
{-# INLINE defline #-}

fastaDefline = defline (A.word8 62)

fastqDefline = defline (A.word8 64)


-------------------------------------------------
--      Sequence parsing
-------------------------------------------------

-- | Monadic parser for a sequence
-- Newline characters are ignored
-- Parser finishes if '>' is found
-- but not on endOfInput so parseOnly is required
multSeqFasta :: Parser Sequence
multSeqFasta = loop
  where
    loop = do
        rawSeq <- singleLine
        m <- A.peekWord8
        case m of
            Just x | x == 62 -> return rawSeq
            _                -> mappend rawSeq <$> multSeqFasta

-- | Identical to multSeqFasta
-- but stops when it encounters '@' or '+'
multSeqFastq :: Parser Sequence
multSeqFastq = loop
  where
    loop = do
        rawSeq <- singleLine
        m <- A.peekWord8
        case m of
            Just x | x == 64 || x == 43 -> return rawSeq
            _                           -> mappend rawSeq <$> multSeqFastq


-------------------------------------------------
--      Scoreline (quality scores)
-------------------------------------------------

scoreLine :: Parser ScoreLine
scoreLine = singleLine
{-# INLINE scoreLine #-}

-------------------------------------------------
--      Plusline
-------------------------------------------------

plusLine :: Parser PlusLine
plusLine = A.word8 43 *> singleLine
{-# INLINE plusLine #-}
