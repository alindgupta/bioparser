module Data.Bioparser.Prim
    (  ) where

import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import Control.Applicative ((*>), (<*), pure, (<|>))


notEndOfLine :: Word8 -> Bool
notEndOfLine c = (c /= 10) && (c /= 13)
{-# INLINE notEndOfLine #-}

endOfLine = A.word8 10 <|> A.word8 13

fastaDefline = defline $ A.word8 62   --  '>'

fastqDefline = defline $ A.word8 64   --  '@'

{-# INLINE defline #-}
defline :: Parser Word8 -> Parser B.ByteString
defline start = start *> A.takeWhile notEndOfLine <* endOfLine

multBase' :: Parser B.ByteString
multBase' = loop 
  where
    loop = do
        rawSeq <- A.takeWhile notEndOfLine <* endOfLine
        m <- A.peekWord8
        case m of
            Just x | x == 62 || x == 64 -> return mempty
            _ -> fmap (mappend rawSeq) loop



scoreLine :: Parser B.ByteString
scoreLine = A.takeWhile notEndOfLine <* endOfLine
{-# INLINE scoreLine #-}

plusLine :: Parser B.ByteString
plusLine = A.word8 43 *> A.takeWhile notEndOfLine <* endOfLine
{-# INLINE plusLine #-}



