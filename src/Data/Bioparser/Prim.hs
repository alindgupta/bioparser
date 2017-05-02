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
    ( defline       -- * sequence id and/or description ('>' or '@')
    , rawSeq        -- * raw sequence
    , scoreline     -- * quality scores for fastq
    , plusline      -- * optional id for fastq (beginning with '+')
    , eol           -- * handle \r, \n, \r\n
    ) where

import Data.Word (Word8)
import Data.Attoparsec.ByteString
import Data.Attoparsec.Combinator
import Control.Applicative


-- | \n or \r or \r\n
eol :: Parser Word8
eol = word8 10 <|> word8 13 <|> (word8 13 *> word8 10)


-------------------------------------------------
--      Defline
-------------------------------------------------

-- | Initial defline character for fasta
deflFs :: Parser Word8
deflFs = word8 62    -- '>'

-- | Initial defline character for fastq
deflFq :: Parser Word8
deflFq = word8 64    -- '@'

-- | Defline description, required
deflRest :: Parser [Word8]
deflRest = many (satisfy notEol)
  where notEol x = x /= 10 && x /= 13

defline :: Parser [Word8]
defline = (deflFs <|> deflFq) *> deflRest <* eol


-------------------------------------------------
--      Raw sequence parsing
-------------------------------------------------


rawSeq :: Parser [Word8]
rawSeq = mconcat <$> sepBy' multBase eol <* lookAhead (deflFs <|> deflFq)
    where multBase = init <$> many (satisfy notEOL)
          notEOL x = x /= 10 && x /= 13


-------------------------------------------------
--      Scoreline (quality scores)
-------------------------------------------------

scoreline :: Parser [Word8]
scoreline = many (satisfy notEol)
  where notEol x = x /= 10 && x /= 13


-------------------------------------------------
--      Plusline parsing
-------------------------------------------------

plusline :: Parser [Word8]
plusline = word8 43 *> many (satisfy notEol) <* eol
    where notEol x = x /= 10 && x /= 13
