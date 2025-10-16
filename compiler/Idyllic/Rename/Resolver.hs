module Idyllic.Rename.Resolver where

import Control.Monad.State (MonadState (get, put), State, runState)
import Data.Text (Text)
import Idyllic.Rename.HIR
import Idyllic.Rename.Symbol (Symbol (Symbol))
import Idyllic.Syn.AST (SynNode (..))
import qualified Idyllic.Syn.AST as AST

newtype RenameError = REUnboundName Text deriving (Show, Eq)

data Env = Env
  { nameMap :: [(Text, Int)],
    nameCounter :: Int,
    nodeIdCounter :: Int,
    errors :: [RenameError]
  }
  deriving (Show, Eq)

defaultEnv :: Env
defaultEnv = Env [] 0 0 []

find :: Text -> Rename (Maybe Int)
find name = do
  env <- get
  case lookup name (nameMap env) of
    Just n -> pure (Just n)
    Nothing -> do
      put env {errors = REUnboundName name : errors env}
      pure Nothing

freshName :: Text -> Rename Int
freshName name = do
  env <- get
  put env {nameMap = (name, nameCounter env) : nameMap env, nameCounter = nameCounter env + 1}
  pure $ nameCounter env

freshNodeId :: Rename Int
freshNodeId = do
  env <- get
  put env {nodeIdCounter = nodeIdCounter env + 1}
  pure $ nodeIdCounter env

type Rename a = State Env a

rename :: AST.Expr -> Either [RenameError] Expr
rename expr =
  let (result, env) = runState (renameExpr expr) defaultEnv
   in case errors env of
        [] -> Right result
        errs -> Left (reverse errs)

renameExpr :: AST.Expr -> Rename Expr
renameExpr expr = go $ synNodeKind expr
  where
    go :: AST.ExprKind -> Rename Expr
    go (AST.ExprInt n) = do
      nodeId <- freshNodeId
      pure $ HirNode nodeId (ExprInt n) (synNodeSpan expr)
    go (AST.ExprVar x) = do
      nodeId <- freshNodeId
      var <- find $ synNodeKind x
      let sym = Symbol <$> var <*> pure (synNodeSpan x)
      let m = maybe ExprError ExprVar sym
      pure $ HirNode nodeId m (synNodeSpan expr)
    go (AST.ExprLet bind body) = do
      (binds, body') <- renameBind bind body
      nodeId <- freshNodeId
      pure $ HirNode nodeId (ExprLet binds body') (synNodeSpan expr)
      where
        renameBind :: AST.Bind -> AST.Expr -> Rename ([Bind], Expr)
        renameBind (AST.BindName x e) b = do
          x' <- Symbol <$> freshName $ synNodeKind x -- we add the name to the env first since laziness means it can be recursive
          expr' <- renameExpr e
          body' <- renameExpr b
          pure ([BindName x' e'], body')
        renameBind (AST.BindFun f args expr) b = do
          f' <- Symbol <$> freshName f
          args' <- mapM (fmap Symbol . freshName) args
          expr' <- renameExpr expr
          body' <- renameExpr b
          pure ([BindFun f' args' expr'], body')
    go (AST.ExprLam x body) = do
      x' <- Symbol <$> freshName x
      body' <- renameExpr body
      pure $ ExprLam x' body'
    go (AST.ExprApp f a) = ExprApp <$> renameExpr f <*> renameExpr a
