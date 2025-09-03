module MMC.Utils.Span (Span (..), toPair, slice) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

data Span = Span {spanStart :: Int, spanEnd :: Int}
  deriving (Show, Eq, Ord)

toPair :: Span -> (Int, Int)
toPair (Span s e) = (s, e)

slice :: Span -> ByteString -> ByteString
slice (Span s e) bs = BS.take (e - s) (BS.drop s bs)

-- instance Pretty Span where
--   pretty (Loc s e) = pack $ show s <> ".." <> show e

instance Semigroup Span where
  Span s1 e1 <> Span s2 e2 = Span (min s1 s2) (max e1 e2)

data Spanned a = Spanned a Span
  deriving (Show, Eq, Ord)

unSpan :: Spanned a -> a
unSpan (Spanned v _) = v

getSpan :: Spanned a -> Span
getSpan (Spanned _ s) = s

-- instance (Pretty a) => Pretty (Spanned a) where
--   pretty (Located v s) = pretty v <> " @ " <> pretty s

instance Functor Spanned where
  fmap f (Spanned v s) = Spanned (f v) s
