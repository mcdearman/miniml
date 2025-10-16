module Idyllic.Rename.Resolver where

import Control.Monad.State (MonadState (get, put), State, runState)
import Data.Text (Text)
import Idyllic.Rename.HIR
import Idyllic.Rename.Symbol (Symbol (Symbol))
import qualified Idyllic.Syn.AST as AST

newtype RenameError = REUnboundName Text deriving (Show, Eq)

data Env = Env
  { nameMap :: [(Text, Int)],
    counter :: Int,
    errors :: [RenameError]
  }
  deriving (Show, Eq)

defaultEnv :: Env
defaultEnv = Env [] 0 []

find :: Text -> RenameState (Maybe Int)
find name = do
  env <- get
  case lookup name (nameMap env) of
    Just n -> pure (Just n)
    Nothing -> do
      put env {errors = REUnboundName name : errors env}
      pure Nothing

freshName :: Text -> RenameState Int
freshName name = do
  env <- get
  put env {nameMap = (name, counter env) : nameMap env, counter = counter env + 1}
  pure $ counter env

type RenameState a = State Env a

rename :: AST.Expr -> Either [RenameError] Expr
rename expr =
  let (result, env) = runState (renameExpr expr) defaultEnv
   in case errors env of
        [] -> Right result
        errs -> Left (reverse errs)

renameExpr :: AST.Expr -> RenameState Expr
renameExpr (AST.ExprInt n) = pure $ ExprInt n
renameExpr (AST.ExprVar x) = maybe ExprError (ExprVar . Symbol) <$> find x
renameExpr (AST.ExprLet bind body) = do
  (binds, body') <- renameBind bind body
  pure $ ExprLet binds body'
  where
    renameBind :: AST.Bind -> AST.Expr -> RenameState ([Bind], Expr)
    renameBind (AST.BindName x expr) b = do
      x' <- Symbol <$> freshName x -- we add the name to the env first since laziness means it can be recursive
      expr' <- renameExpr expr
      body' <- renameExpr b
      pure ([BindName x' expr'], body')
    renameBind (AST.BindFun f args expr) b = do
      f' <- Symbol <$> freshName f
      args' <- mapM (fmap Symbol . freshName) args
      expr' <- renameExpr expr
      body' <- renameExpr b
      pure ([BindFun f' args' expr'], body')
renameExpr (AST.ExprLam x body) = do
  x' <- Symbol <$> freshName x
  body' <- renameExpr body
  pure $ ExprLam x' body'
renameExpr (AST.ExprApp f a) = ExprApp <$> renameExpr f <*> renameExpr a
