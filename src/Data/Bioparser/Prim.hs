{- |
   Module      : Data.Bioparser.Prim
   Maintainer  : Alind Gupta <alind.gupta@mail.utoronto.ca>
   Stability   : 0.1
   Portability : portable
This module exports primitive parser combinators.
TODO - rename the `lines` function
-}

module Data.Bioparser.Prim
    (
      -- * functions
      defline  
    , lines
    ) where

import Control.Applicative ((*>), (<*), (<|>), liftA2)
import Data.Attoparsec.ByteString     
import Data.Bioparser.Types
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import Data.Word (Word8)
import Prelude hiding (takeWhile, lines)


-- | newline char
n :: Word8
n = 10


-- | newline char for carriage return
r :: Word8
r = 13


-- | Not the end of a line
-- This is not a `Parser`, simply a utility function
notEndOfLine :: Word8 -> Bool
notEndOfLine ch = ch /= n && ch /= r


-- | Parse end of line characters
-- i.e. either '\n' or '\r'
endOfLine :: Parser Word8
endOfLine = word8 r <|> word8 n


-- | A line delimited by an end of line character
singleLine :: Parser ByteString
singleLine = takeWhile notEndOfLine <* endOfLine


-- | A single line starting with a particular character
lineWith :: Parser Word8 -> Parser ByteString
lineWith symb = symb *> singleLine


-- | Parses multiple lines of a sequence (of nucleotides etc)
-- and combines them into one
lines1 :: Parser ByteString
lines1 = do
  line <- singleLine
  next <- peekWord8
  if next == Just 10 || next == Just 62 || isNothing next
    then return line
    else mappend line <$> lines Fasta
lines2 :: Parser ByteString
lines2 = do
  line <- singleLine
  next <- peekWord8
  if next == Just 43 || next == Just 64
    then return line
    else mappend line <$> lines Fasta

-- | Wrap into a reader monad to access ParserType
-- Not sure if this is necessary and if I can get by
-- with a ((->) Parser) approach but its OK for now.
getSequence :: (Monad m, MonadReader ParserType m) => m (Parser ByteString)
getSequence = do
  parserType <- ask
  case parserType of
    Fasta -> parseExcept [10, 62]  -- ^ chars > and \n
    Fastq -> parseExcept [43, 64]  -- ^ chars + and >
  where
    parseExcept :: [Word8] -> Parser ByteString
    parseExcept args = do
      line <- singleLine
      next <- peekWord8
      if next `elem` Just <$> args || isNothing next
        then return line
        else mappend line <$> parseExcept args
