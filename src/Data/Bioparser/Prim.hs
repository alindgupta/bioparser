-- |
--      Module: Data.Bioparser.Prim
--
--      Primitives for FASTA and FASTQ file format parsers
--      (dependancy for Data.Bioparser.Combinators)
--      ASCII is assumed
--
--      Maintainer: Alind Gupta
--


module Data.Bioparser.Prim
    ( fastaDefline
    , fastqDefline          -- * sequence id and/or description ('>' or '@')
    , multSeq         -- * raw sequence
    , scoreLine        -- * quality scores for fastq
    , plusLine         -- * optional id for fastq (beginning with '+')
    , endOfLine        -- * handle \r, \n, \r\n
    ) where

import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString     
import qualified Data.Attoparsec.ByteString as A
import Control.Applicative ((*>), (<*), (<|>))

import Data.Bioparser.Util

-- | Utility function for other parsers
notEndOfLine :: Word8 -> Bool
notEndOfLine c = (c /= 10) && (c /= 13)
{-# INLINE notEndOfLine #-}

-- | \n or \r
endOfLine :: Parser Word8
endOfLine = A.word8 10 <|> A.word8 13
{-# INLINE endOfLine #-}

-- | Utility function, reads a single line delimited by a newline char
singleLine = A.takeWhile notEndOfLine <* endOfLine
{-# INLINE singleLine #-}

-------------------------------------------------
--      Defline
-------------------------------------------------


fastaDefline = defline (A.word8 62)

fastqDefline = defline (A.word8 64)

defline :: Parser Word8 -> Parser Defline
defline symb = symb *> A.takeWhile notEndOfLine <* endOfLine
{-# INLINE defline #-}


-------------------------------------------------
--      Sequence parsing
-------------------------------------------------

multSeq :: Parser Sequence
multSeq = loop
  where
    loop = do
        rawSeq <- singleLine
        m <- A.peekWord8
        case m of
            Just x | x == 62 || x == 64 -> return rawSeq
            _                           -> mappend rawSeq <$> multSeq


-------------------------------------------------
--      Scoreline (quality scores)
-------------------------------------------------

scoreLine :: Parser ScoreLine
scoreLine = singleLine
{-# INLINE scoreLine #-}

-------------------------------------------------
--      Plusline parsing
-------------------------------------------------

plusLine :: Parser PlusLine
plusLine = A.word8 43 *> singleLine
{-# INLINE plusLine #-}
