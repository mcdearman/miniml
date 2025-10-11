{-# LANGUAGE RankNTypes #-}

module Idyllic.Build where

import Control.Monad.State
import Data.Hashable (Hashable)

-- An abstract store containing a key/value map and persistent build information
data Store i k v -- i = info, k = key, v = value

initialise :: i -> (k -> v) -> Store i k v
initialise = undefined

getInfo :: Store i k v -> i
getInfo = undefined

putInfo :: i -> Store i k v -> Store i k v
putInfo = undefined

getValue :: k -> Store i k v -> v
getValue = undefined

putValue :: (Eq k) => k -> v -> Store i k v -> Store i k v
putValue = undefined

data Hash v -- a compact summary of a value with a fast equality check

hash :: (Hashable v) => v -> Hash v
hash = undefined

getHash :: (Hashable v) => k -> Store i k v -> Hash v
getHash = undefined

-- Build tasks (see ğ3.2)
newtype Task c k v = Task {run :: forall f. (c f) => (k -> f v) -> f v}

type Tasks c k v = k -> Maybe (Task c k v)

-- Build system (see ğ3.3)
type Build c i k v = Tasks c k v -> k -> Store i k v -> Store i k v

-- Build system components: a scheduler and a rebuilder (see ğ5)
type Scheduler c i ir k v = Rebuilder c ir k v -> Build c i k v

type Rebuilder c ir k v = k -> v -> Task c k v -> Task (MonadState ir) k v
