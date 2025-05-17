{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MMC.Parsec.Stream where

import Data.Kind (Type)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

class Stream s where
  type Token s :: Type
  type Tokens s :: Type

  tokenToChunk :: forall c -> (s ~ c) => Token s -> Tokens s
  tokenToChunk pxy = tokensToChunk pxy . pure

  tokensToChunk :: forall c -> (s ~ c) => [Token s] -> Tokens s

  chunkToTokens :: forall c -> (s ~ c) => Tokens s -> [Token s]

  chunkLength :: forall c -> (s ~ c) => Tokens s -> Int

  chunkEmpty :: forall c -> (s ~ c) => Tokens s -> Bool
  chunkEmpty t ts = chunkLength t ts <= 0

  take1' :: s -> Maybe (Token s, s)

  takeN' :: Int -> s -> Maybe (Tokens s, s)

  takeWhile' :: (Token s -> Bool) -> s -> (Tokens s, s)

instance (Ord a) => Stream [a] where
  type Token [a] = a
  type Tokens [a] = [a]

  tokenToChunk _ = pure
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1' [] = Nothing
  take1' (x : xs) = Just (x, xs)
  takeN' n s
    | n <= 0 = Just ([], s)
    | null s = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile' = span

instance Stream Text where
  type Token Text = Char
  type Tokens Text = Text

  tokenToChunk _ = T.singleton
  tokensToChunk _ = pack
  chunkToTokens _ = unpack
  chunkLength _ = T.length
  chunkEmpty _ = T.null
  take1' s = case unpack s of
    [] -> Nothing
    (x : xs) -> Just (x, pack xs)
  takeN' n s
    | n <= 0 = Just (T.empty, s)
    | T.null s = Nothing
    | otherwise = Just (T.splitAt n s)
  takeWhile' = T.span

instance Stream TL.Text where
  type Token TL.Text = Char
  type Tokens TL.Text = TL.Text

  tokenToChunk _ = TL.singleton
  tokensToChunk _ = TL.pack
  chunkToTokens _ = TL.unpack
  chunkLength _ = fromIntegral . TL.length
  chunkEmpty _ = TL.null
  take1' s = case TL.unpack s of
    [] -> Nothing
    (x : xs) -> Just (x, TL.pack xs)
  takeN' n s
    | n <= 0 = Just (TL.empty, s)
    | TL.null s = Nothing
    | otherwise = Just (TL.splitAt (fromIntegral n) s)
  takeWhile' = TL.span