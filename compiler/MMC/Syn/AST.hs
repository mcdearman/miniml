{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module MMC.Syn.AST where

import Data.Maybe (listToMaybe, mapMaybe)
import MMC.Syn.GreenNode (SyntaxKind (..))
import MMC.Syn.SyntaxNode (SyntaxNode, nodeChildren, nodeKind)

class AstNode a where
  castToNode :: SyntaxNode -> Maybe a
  syntaxNode :: a -> SyntaxNode

newtype Module = Module SyntaxNode
  deriving (Show, Eq)

moduleName :: Module -> Maybe UpperCaseIdent
moduleName (Module node) = findMap (castToNode @UpperCaseIdent) (nodeChildren node)

moduleDecls :: Module -> [Decl]
moduleDecls (Module node) = mapMaybe (castToNode @Decl) (nodeChildren node)

pattern ModuleP :: Maybe UpperCaseIdent -> [Decl] -> Module
pattern ModuleP name decls <-
  (\m -> (moduleName m, moduleDecls m) -> (name, decls))

{-# COMPLETE ModuleP #-}

instance AstNode Module where
  castToNode :: SyntaxNode -> Maybe Module
  castToNode node = case nodeKind node of
    SyntaxKindModule -> Just (Module node)
    _ -> Nothing

  syntaxNode (Module node) = node

newtype ModuleDecls = ModuleDecls SyntaxNode deriving (Show, Eq)

data Decl
  = DeclImport ImportDecl
  | DeclClassDecl ClassDecl
  deriving (Show, Eq)

instance AstNode Decl where
  castToNode node = undefined

  syntaxNode d = case d of
    DeclImport i -> syntaxNode i
    DeclClassDecl c -> syntaxNode c

newtype ImportDecl = ImportDecl SyntaxNode deriving (Show, Eq)

instance AstNode ImportDecl where
  castToNode node = undefined

  syntaxNode (ImportDecl node) = node

data ClassDecl
  = ClassDeclSig Sig
  | ClassDeclBind Bind
  deriving (Show, Eq)

instance AstNode ClassDecl where
  castToNode node = undefined

  syntaxNode c = case c of
    ClassDeclSig s -> syntaxNode s
    ClassDeclBind b -> syntaxNode b

newtype Sig = Sig SyntaxNode deriving (Show, Eq)

instance AstNode Sig where
  castToNode node = undefined

  syntaxNode (Sig node) = node

data Bind
  = BindFun FunBind
  | BindPat PatBind
  deriving (Show, Eq)

instance AstNode Bind where
  castToNode node = undefined

  syntaxNode b = case b of
    BindFun f -> syntaxNode f
    BindPat p -> syntaxNode p

newtype FunBind = FunBind SyntaxNode deriving (Show, Eq)

instance AstNode FunBind where
  castToNode node = undefined

  syntaxNode (FunBind node) = node

newtype PatBind = PatBind SyntaxNode deriving (Show, Eq)

instance AstNode PatBind where
  castToNode node = undefined

  syntaxNode (PatBind node) = node

newtype UpperCaseIdent = UpperCaseIdent SyntaxNode
  deriving (Show, Eq)

instance AstNode UpperCaseIdent where
  castToNode node = case nodeKind node of
    SyntaxKindUppercaseIdent -> Just (UpperCaseIdent node)
    _ -> Nothing

  syntaxNode (UpperCaseIdent node) = node

newtype LowerCaseIdent = LowerCaseIdent SyntaxNode
  deriving (Show, Eq)

instance AstNode LowerCaseIdent where
  castToNode node = case nodeKind node of
    SyntaxKindLowercaseIdent -> Just (LowerCaseIdent node)
    _ -> Nothing

  syntaxNode (LowerCaseIdent node) = node

data Lit
  = LitInt IntLit
  | LitChar CharLit
  | LitString StringLit
  deriving (Show, Eq)

instance AstNode Lit where
  castToNode node = case nodeKind node of
    SyntaxKindInt -> LitInt <$> castToNode @IntLit node
    SyntaxKindChar -> LitChar <$> castToNode @CharLit node
    SyntaxKindString -> LitString <$> castToNode @StringLit node
    _ -> Nothing

  syntaxNode l = case l of
    LitInt i -> syntaxNode i
    LitChar c -> syntaxNode c
    LitString s -> syntaxNode s

newtype IntLit = IntLit SyntaxNode deriving (Show, Eq)

instance AstNode IntLit where
  castToNode :: SyntaxNode -> Maybe IntLit
  castToNode node = case nodeKind node of
    SyntaxKindInt -> Just (IntLit node)
    _ -> Nothing

  syntaxNode :: IntLit -> SyntaxNode
  syntaxNode (IntLit node) = node

newtype CharLit = CharLit SyntaxNode deriving (Show, Eq)

instance AstNode CharLit where
  castToNode :: SyntaxNode -> Maybe CharLit
  castToNode node = case nodeKind node of
    SyntaxKindChar -> Just (CharLit node)
    _ -> Nothing

  syntaxNode :: CharLit -> SyntaxNode
  syntaxNode (CharLit node) = node

newtype StringLit = StringLit SyntaxNode deriving (Show, Eq)

instance AstNode StringLit where
  castToNode :: SyntaxNode -> Maybe StringLit
  castToNode node = case nodeKind node of
    SyntaxKindString -> Just (StringLit node)
    _ -> Nothing

  syntaxNode :: StringLit -> SyntaxNode
  syntaxNode (StringLit node) = node

-- Helpers
findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = listToMaybe . mapMaybe f