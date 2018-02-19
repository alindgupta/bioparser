{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

{- |
   Module      : Data.Bioparser.Types
   Maintainer  : Alind Gupta <alind.gupta@mail.utoronto.ca>
   Stability   : 0.1
   Portability : portable
This module exports utility datatypes.
-}

module Data.Bioparser.Types
  ( Record(..)
  , FastaRecord
  , FastqRecord
  , ParserType(..)
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Vector (Vector)
import Prelude

data ParserType = Fasta | Fastq
  deriving (Enum, Eq)

type FastqRecord = (ByteString, ByteString, ByteString, ByteString)
type FastaRecord = (ByteString, ByteString)

newtype Record t = Record (Vector t)
  deriving (Functor, Applicative)

-- Required for benchmarking purposes
instance NFData (Record r) where
  rnf r = seq r ()
