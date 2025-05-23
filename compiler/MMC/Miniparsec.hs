{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MMC.Miniparsec
  ( Parsec,
  -- runParsec,
  )
where

import Control.Monad.Identity (Identity, runIdentity)
import Data.Void (Void)
import MMC.Miniparsec.Internal (ParsecT, runParsecT)

-- | Pure parser (no effects)
type Parsec e s = ParsecT e s Void Identity

-- | Run a pure parser
-- runParsec :: Parsec e s a -> s -> Result e a
-- runParsec p xs = runIdentity (runParsecT p xs)
