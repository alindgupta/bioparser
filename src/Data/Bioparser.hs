module Data.Bioparser
    ( decodeFasta       -- * fasta -> vector of fasta records
    , encodeFasta       -- * vector -> fasta format
    , decodeFastq       
    , encodeFastq 
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Monoid ((<>))

import Data.Bioparser.Combinators
import Data.Bioparser.Types

decodeFasta :: ByteString -> Either String (Vector FastaRecord)
decodeFasta = parseOnly parseFasta

decodeFastq :: ByteString -> Either String (Vector FastqRecord)
decodeFastq = parseOnly parseFastq

-- obeys the following law, should test for this more extensively
-- decodeFasta .  encodeFasta . decodeFasta = decodeFasta
-- encodeFasta puts a "\n" at the end of file so may not be
-- exactly equal to input i.e.
-- fmap (== f) (encodeFasta $ decodeFasta f) may not return True

-- EXTREMELY SLOW!!! (~500X slower than decodeFasta)
--

n = B.singleton '\n'
{-# INLINE n #-}

encodeFasta :: Vector FastaRecord -> ByteString
encodeFasta = foldr ((<>) . fastaEncoder) mempty
  where fastaEncoder (FastaRecord (d,s)) = ">" <> d <> n <> s <> n

encodeFastq :: Vector FastqRecord -> ByteString
encodeFastq = foldr ((<>) . fastqEncoder) mempty
  where fastqEncoder (FastqRecord (d,s,p,sc)) = "@" <> d <> n <> s <> n
                                                <> "+" <> p <> n <> sc <> n
