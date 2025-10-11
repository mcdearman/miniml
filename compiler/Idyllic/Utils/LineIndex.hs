module Idyllic.Utils.LineIndex
  ( LineIndex (..),
    buildLineIndex,
    offsetToLineCol,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U

newtype LineIndex = LineIndex {lineStarts :: U.Vector Int} deriving (Show, Eq)

buildLineIndex :: ByteString -> LineIndex
buildLineIndex bs = LineIndex . U.fromList $ 0 : map (+ 1) (B.elemIndices 0x0A bs)

offsetToLineCol :: LineIndex -> Int -> (Int, Int)
offsetToLineCol (LineIndex starts) !offset =
  let !i = binarySearch starts offset
      !lineStart = starts U.! i
      !col = offset - lineStart + 1
   in (i + 1, col)

binarySearch :: U.Vector Int -> Int -> Int
binarySearch !v !x = go 0 (U.length v - 1)
  where
    go !low !high
      | low > high = high
      | midVal == x = mid
      | midVal < x = go (mid + 1) high
      | otherwise = go low (mid - 1)
      where
        mid = (low + high) `div` 2
        midVal = v U.! mid
