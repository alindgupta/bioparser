module Data.Bioparser
    ( decodeFasta       -- * fasta -> vector of fasta records
    , encodeFasta       -- * vector -> fasta format
    , decodeFastq       
    , encodeFastq 
    ) where

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString
import Data.Vector (Vector)

import Data.Bioparser.Combinators
import Data.Bioparser.Types

decodeFasta :: ByteString -> Either String (Vector FastaRecord)
decodeFasta = parseOnly parseFasta

decodeFastq :: ByteString -> Either String (Vector FastqRecord)
decodeFastq = parseOnly parseFastq

encodeFasta :: Vector FastaRecord -> ByteString
encodeFasta = undefined

encodeFastq :: Vector FastaRecord -> ByteString
encodeFastq = undefined
