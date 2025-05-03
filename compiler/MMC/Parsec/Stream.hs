{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MMC.Parsec.Stream where

import Data.Kind (Type)

class Stream s where
  data Token s :: Type
  data Tokens s :: Type

  tokenToChunk :: forall c -> (s ~ c) => Token s -> Tokens s
  tokenToChunk pxy = tokensToChunk pxy . tokenToChunk pxy

  tokensToChunk :: forall c -> (s ~ c) => Tokens s -> Tokens s

  chunkToTokens :: forall c -> (s ~ c) => Tokens s -> [Token s]

  chunkLength :: forall c -> (s ~ c) => Tokens s -> Int

  chunkEmpty :: forall c -> (s ~ c) => Tokens s -> Bool
  chunkEmpty pxy ts = chunkLength pxy ts <= 0

  take1' :: s -> Maybe (Token s, s)

  takeN' :: Int -> s -> Maybe (Tokens s, s)

  takeWhile' :: (Token s -> Bool) -> s -> (Tokens s, s)

instance (Ord a) => Stream [a] where
  data Token [a] = Token a
  data Tokens [a] = Tokens [a]

  tokenToChunk _ (Token x) = Tokens [x]
  tokensToChunk _ (Tokens xs) = Tokens xs
  chunkToTokens _ (Tokens xs) = xs
  chunkLength _ = length
  chunkEmpty _ = null
  take1' [] = Nothing
  take1' (x : xs) = Just (Token x, xs)
  takeN' n s
    | n <= 0 = Just (Tokens [], s)
    | otherwise = case takeWhile' (const True) s of
        (Tokens xs, ys) -> Just (Tokens (take n xs), ys)
  takeWhile' = span