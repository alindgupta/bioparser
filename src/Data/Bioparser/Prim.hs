{- |
   Module      : Data.Bioparser.Prim
   Maintainer  : Alind Gupta <alind.gupta@mail.utoronto.ca>
   Stability   : 0.1
   Portability : portable
This module exports primitive parser combinators.
TODO - rename the `lines` function
-}

module Data.Bioparser.Prim
    ( defline
    , lines
    ) where

import Control.Applicative ((*>), (<*), (<|>), liftA2)
import Data.Attoparsec.ByteString     
import Data.Bioparser.Types
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import Data.Word (Word8)
import Prelude hiding (takeWhile, lines)

n :: Word8
n = 10

r :: Word8
r = 13

notEndOfLine :: Word8 -> Bool
notEndOfLine ch = ch /= n && ch /= r

endOfLine :: Parser Word8
endOfLine = word8 r <|> word8 n

singleLine :: Parser ByteString
singleLine = takeWhile notEndOfLine <* endOfLine

defline :: Parser Word8 -> Parser ByteString
defline symb = symb *> singleLine

fastaDefline :: Parser ByteString
fastaDefline = defline $ word8 62

fastqDefline :: Parser ByteString
fastqDefline = defline $ word8 64

-- parses multiple lines of sequence
-- and combines them into one
lines :: ParserType -> Parser ByteString
lines Fasta = do
  line <- singleLine
  next <- peekWord8
  if next == Just 10 || next == Just 62 || isNothing next
    then return line
    else mappend line <$> lines Fasta

lines Fastq = do
  line <- singleLine
  next <- peekWord8
  if next == Just 43 || next == Just 64
    then return line
    else mappend line <$> lines Fasta
