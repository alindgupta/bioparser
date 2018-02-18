module Data.Bioparser
    ( decodeFasta       -- * fasta -> vector of fasta records
    , encodeFasta       -- * vector -> fasta format
    , decodeFastq       
    , encodeFastq 
    ) where

{-# LANGUAGE ViewPatterns #-}

import Data.Attoparsec.ByteString
import Data.Bioparser.Combinators
import Data.Bioparser.Types
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Vector (Vector)
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

decodeFasta :: ByteString -> Maybe (Vector FastaRecord)
decodeFasta x = case parseResult x of
    Done _ r -> Just r
    _        -> Nothing
  where
    parseResult = flip feed B.empty . parse parseFasta

decodeFastq :: ByteString -> Vector FastqRecord
decodeFastq x = case feed (parse parseFastq x) B.empty of
                    Done _ r -> r
                    _        -> error "parse error"

encodeFasta :: Vector FastaRecord -> ByteString
encodeFasta = foldr (mappend . fastaEncoder) mempty
  where fastaEncoder (FastaRecord (d,s)) = ">" <> d <> "\n" <> s <> "\n"

encodeFastq :: Vector FastqRecord -> ByteString
encodeFastq = foldr (mappend . fastqEncoder) mempty
  where fastqEncoder (FastqRecord (d,s,p,sc)) = "@" <> d <> "\n" <> s <> "\n"
                                                <> "+" <> p <> "\n" <> sc <> "\n"
