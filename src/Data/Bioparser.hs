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

decodeFasta :: ByteString -> Vector FastaRecord
decodeFasta x = case feed (parse parseFasta x) "\n" of
                    Done _ r -> r
                    _        -> error "parse error"
                    

decodeFastq :: ByteString -> Vector FastqRecord
decodeFastq x = case feed (parse parseFastq x) B.empty of
                    Done _ r -> r
                    _        -> error "parse error"

-- obeys the following law, should test for this more extensively
-- decodeFasta .  encodeFasta . decodeFasta = decodeFasta
-- encodeFasta puts a "\n" at the end of file so may not be
-- exactly equal to input i.e.
-- fmap (== f) (encodeFasta $ decodeFasta f) may not return True

encodeFasta :: Vector FastaRecord -> ByteString
encodeFasta = foldr (mappend . fastaEncoder) mempty
  where fastaEncoder (FastaRecord (d,s)) = ">" <> d <> "\n" <> s <> "\n"

encodeFastq :: Vector FastqRecord -> ByteString
encodeFastq = foldr (mappend . fastqEncoder) mempty
  where fastqEncoder (FastqRecord (d,s,p,sc)) = "@" <> d <> "\n" <> s <> "\n"
                                                <> "+" <> p <> "\n" <> sc <> "\n"
