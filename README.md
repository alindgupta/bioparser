## bioparser
A fast(er) parser for .fasta or .fastq formats using the *attoparsec* library. ASCII encoding is assumed.

Master [git repository](https://github.com/fushitarazu/bioparser):
* `git clone git://github.com/fushitarazu/bioparser.git`

### Usage
Build with stack.

```haskell

import qualified Data.ByteString as BS

main = do
  f <- BS.readFile "x.fasta"
  let x = decodeFasta f
  -- do something with x :: Vector FastaRecord

```
