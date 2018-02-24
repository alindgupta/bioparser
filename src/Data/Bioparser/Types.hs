{- |
   Module      : Data.Bioparser.Types
   Maintainer  : Alind Gupta <alind.gupta@mail.utoronto.ca>
   Stability   : 0.1
   Portability : portable
This module exports utility datatypes.
-}

module Data.Bioparser.Types
  (
    -- * datatypes
    Record(..)
  , ParserType(..)
  , ParserOutput(..)
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Vector (Vector)
import Prelude

data ParserType = Fasta
                | Fastq
  deriving (Eq, Enum)

-- | Parser outputs.
data ParserOutput i =
    Empty i      -- ^ parsing error
  | FastaRecord  -- ^ fasta record
    (ByteString, ByteString)
  | FastqRecord  -- ^ fastq record
    (ByteString, ByteString, ByteString, ByteString)
  deriving (Eq)

newtype Record t = Record (Vector t)
  deriving (Functor, Applicative)

-- Required for benchmarking purposes
instance NFData (Record r) where
  rnf r = seq r ()
