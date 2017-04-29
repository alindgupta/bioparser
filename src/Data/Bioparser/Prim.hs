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

import Data.ByteString
import Data.Attoparsec.ByteString
import Control.Applicative


-- | \n or \r or \r\n
eol = word8 10 <|> word8 13 <|> (word8 13 *> word8 10)


-------------------------------------------------
--      Defline
-------------------------------------------------

-- | Initial defline character for fasta
deflFs = word8 62    -- '>'

-- | Initial defline character for fastq
deflFq = word8 64    -- '@'

-- | Defline description, required
deflRest = many (satisfy notEol)
  where notEol x = x /= 10 && x /= 13

defline = (deflFs <|> deflFq) *> deflRest <* eol


-------------------------------------------------
--      Raw sequence parsing
-------------------------------------------------

multBase = many (satisfy notDefl) <* (deflFs <|> deflFq)
  where notDefl x = x /= 62 && x /= 64 && x /= 10 && x /= 13

rawSeq = mconcat <$> sepBy1 multBase eol


-------------------------------------------------
--      Scoreline (quality scores)
-------------------------------------------------

scoreline = many (satisfy notEol)
  where notEol x = x /= 10 && x /= 13


-------------------------------------------------
--      Plusline parsing
-------------------------------------------------

plusline = word8 43 *> many (satisfy notEol) <* eol
    where notEol x = x /= 10 && x /= 13
