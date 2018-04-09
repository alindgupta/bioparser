module Data.Bioparser
    ( 
    -- * functions
      decodeM
    , encodeM
    ) where

import System.Environment
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

decodeM :: (MonadIO m) => ReaderT ParserType (Maybe (Vector (ParserOutput a)))
decodeM p = case parseResult of
  Done _ r -> return $ Just r
  _        -> mzero
  where parseResult :: Vector (ParserOutput a)
        parseResult = decode p

encodeM Fasta = foldr (mappend . f) mempty
  where f (FastaRecord (x,y)) = ">"
                             <> x
                             <> '\n'
                             <> s
                             <> '\n'
