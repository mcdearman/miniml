{-# LANGUAGE RankNTypes #-}

module MMC.Build where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.State
import Data.Hashable (Hashable)
import Data.Text (Text)
import Error.Diagnose (Diagnostic)
import MMC.Common (InputMode (..), LineIndex (..), buildLineIndex)

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

data PipelineEnv = PipelineEnv
  { src :: !Text,
    flags :: ![Text],
    mode :: !InputMode,
    lineIndex :: !LineIndex,
    errors :: TVar [Diagnostic Text]
  }
  deriving (Eq)

class HasDiagnostic e where
  toDiagnostic :: e -> Diagnostic Text

defaultPipelineEnv :: Text -> IO PipelineEnv
defaultPipelineEnv src = do
  errVar <- newTVarIO []
  pure
    PipelineEnv
      { src = src,
        flags = [],
        mode = InputModeInteractive,
        lineIndex = buildLineIndex src,
        errors = errVar
      }

type PipelineM = ReaderT PipelineEnv IO