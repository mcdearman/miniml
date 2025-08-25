module MMC.Common.LineIndex
  ( LineIndex (..),
    buildLineIndex,
    offsetToLineCol,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U

data LineIndex
  = LineIndex {lineStarts :: !(U.Vector Int)}
  deriving (Show, Eq)

buildLineIndex :: Text -> LineIndex
buildLineIndex txt = LineIndex (U.fromList (0 : scan txt))
  where
    scan t = [i + 1 | (i, c) <- zip [0 ..] (T.unpack t), c == '\n']

offsetToLineCol :: LineIndex -> Int -> (Int, Int)
offsetToLineCol (LineIndex starts) !offset =
  let !i = binarySearch starts offset
      !lineStart = starts U.! i
      !col = offset - lineStart + 1
   in (i + 1, col)

{-# INLINE binarySearch #-}
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
