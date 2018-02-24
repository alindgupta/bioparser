{- |
   Module      : Data.Bioparser.Combinators
   Maintainer  : Alind Gupta <alind.gupta@mail.utoronto.ca>
   Stability   : 0.1
   Portability : portable
This module exports methods for parsing bytestrings.
Currently supports fasta and (Sanger) fastq files.

TODO: Support for SAM/BAM.
-}

module Data.Bioparser.Combinators
    (
    -- * functions
    decode
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Attoparsec.ByteString (Parser)
import Data.Bioparser.Prim
import Data.Bioparser.Types ( ParserType(..)
                            , ParserOutput(..))
import Data.ByteString ()
import Data.Vector (Vector, fromList)
import Prelude

-- | Reader monad wrapper for parser @decodeWith@.
decode :: (Monad m, MonadReader ParserType m)
  => m (Parser (Vector (ParserOutput a)))
decode = do
  parserType <- ask
  case parserType of
    Fasta -> return $ decodeWith Fasta
    Fastq -> return $ decodeWith Fastq

-- | Documentation pending.
decodeWith :: ParserType -> Parser (Vector (ParserOutput a))
decodeWith Fasta = let
  singleRecord =
    curry FastaRecord
    <$> lineWith 62
    <*> (manyLines Fasta)
  in fromList <$> many singleRecord

decodeWith Fastq = let
  singleRecord =
    (\a b c d -> FastqRecord (a,b,c,d))
    <$> lineWith 64
    <*> manyLines Fastq
    <*> singleLine
    <*> lineWith 43
  in fromList <$> many singleRecord
