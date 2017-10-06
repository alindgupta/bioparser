-- |
--       Module: Data.Bioparser.Types
--
--       Newtype wrappers
--
--     


module Data.Bioparser.Types
    ( Defline
    , Sequence
    , PlusLine
    , ScoreLine
    , FastaRecord(..)
    , FastqRecord(..)
    ) where

import Data.ByteString (ByteString)
import Control.DeepSeq

type Defline = ByteString
type Sequence = ByteString
type PlusLine = ByteString
type ScoreLine = ByteString

newtype FastaRecord = FastaRecord (Defline, Sequence)
    deriving (Show, Eq)

-- | Required for benchmarking purposes
instance NFData FastaRecord where
    rnf (FastaRecord _) = ()

instance NFData FastqRecord where
    rnf _ = ()

newtype FastqRecord = FastqRecord (Defline, Sequence, PlusLine, ScoreLine)
    deriving (Show, Eq)
