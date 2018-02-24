## bioparser
A fast(er) parser for FASTA and FASTQ files using the *attoparsec* library. ASCII only.

### Usage
Build with stack.

```haskell

import qualified Data.ByteString as BS

main = do
  f <- BS.readFile "x.fasta"
  let x = decodeFasta f
  -- do something with x :: Vector FastaRecord

```
Branch ```slave-monadwriter``` to include stats, almost certainly going to be slower than ```master```. Use ```master``` for speed.
