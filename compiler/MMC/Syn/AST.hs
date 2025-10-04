{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module MMC.Syn.AST where

import Data.Maybe (listToMaybe, mapMaybe)
import MMC.Syn.GreenNode (SyntaxKind (..), Token)
import MMC.Syn.SyntaxNode (SyntaxNode, nodeChildren, nodeKind)

class AstNode a where
  castToNode :: SyntaxNode -> Maybe a
  syntaxNode :: a -> SyntaxNode

class AstToken a where
  castToToken :: SyntaxNode -> Maybe a
  syntaxToken :: a -> Token

newtype Module = Module SyntaxNode
  deriving (Show, Eq)

{-# INLINEABLE moduleName #-}
moduleName :: Module -> Maybe Ident
moduleName (Module node) = findMap (castToNode @Ident) (nodeChildren node)

{-# INLINEABLE moduleDecls #-}
moduleDecls :: Module -> [Decl]
moduleDecls (Module node) = mapMaybe (castToNode @Decl) (nodeChildren node)

pattern ModuleP :: Maybe Ident -> [Decl] -> Module
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
  = DeclImport !ImportDecl
  | DeclClassDecl !ClassDecl
  | DeclClassDef !ClassDef
  | DeclInstanceDecl !InstanceDecl
  | DeclRecordDef !RecordDef
  deriving (Show, Eq)

instance AstNode Decl where
  castToNode node = case nodeKind node of
    SyntaxKindImportDecl -> Just (DeclImport (ImportDecl node))
    SyntaxKindClassDecl -> DeclClassDecl <$> castToNode @ClassDecl node
    SyntaxKindClassDef -> Just (DeclClassDef (ClassDef node))
    SyntaxKindInstanceDecl -> Just (DeclInstanceDecl (InstanceDecl node))
    SyntaxKindRecordDef -> Just (DeclRecordDef (RecordDef node))
    _ -> Nothing

  syntaxNode d = case d of
    DeclImport i -> syntaxNode i
    DeclClassDecl c -> syntaxNode c
    DeclClassDef c -> syntaxNode c
    DeclInstanceDecl i -> syntaxNode i
    DeclRecordDef r -> syntaxNode r

newtype ImportDecl = ImportDecl SyntaxNode deriving (Show, Eq)

instance AstNode ImportDecl where
  castToNode node = case nodeKind node of
    SyntaxKindImportDecl -> Just (ImportDecl node)
    _ -> Nothing

  syntaxNode (ImportDecl n) = n

newtype RecordDef = RecordDef SyntaxNode deriving (Show, Eq)

{-# INLINEABLE recordDefName #-}
recordDefName :: RecordDef -> Maybe Ident
recordDefName (RecordDef n) = findMap (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE recordDefTypeParams #-}
recordDefTypeParams :: RecordDef -> [Ident]
recordDefTypeParams (RecordDef n) = mapMaybe (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE recordDefFields #-}
recordDefFields :: RecordDef -> [RecordField]
recordDefFields (RecordDef n) = mapMaybe (castToNode @RecordField) (nodeChildren n)

pattern RecordDefP :: Maybe Ident -> [Ident] -> [RecordField] -> RecordDef
pattern RecordDefP name typeParams fields <-
  (\r -> (recordDefName r, recordDefTypeParams r, recordDefFields r) -> (name, typeParams, fields))

{-# COMPLETE RecordDefP #-}

instance AstNode RecordDef where
  castToNode node = case nodeKind node of
    SyntaxKindRecordDef -> Just (RecordDef node)
    _ -> Nothing

  syntaxNode (RecordDef n) = n

newtype RecordField = RecordField SyntaxNode deriving (Show, Eq)

{-# INLINEABLE recordFieldName #-}
recordFieldName :: RecordField -> Maybe Ident
recordFieldName (RecordField n) = findMap (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE recordFieldAnno #-}
recordFieldAnno :: RecordField -> Maybe Anno
recordFieldAnno (RecordField n) = findMap (castToNode @Anno) (nodeChildren n)

pattern RecordFieldP :: Maybe Ident -> Maybe Anno -> RecordField
pattern RecordFieldP name anno <-
  (\f -> (recordFieldName f, recordFieldAnno f) -> (name, anno))

{-# COMPLETE RecordFieldP #-}

instance AstNode RecordField where
  castToNode node = case nodeKind node of
    SyntaxKindRecordField -> Just (RecordField node)
    _ -> Nothing

  syntaxNode (RecordField n) = n

newtype ClassDef = ClassDef SyntaxNode deriving (Show, Eq)

{-# INLINEABLE classDefName #-}
classDefName :: ClassDef -> Maybe Ident
classDefName (ClassDef n) = findMap (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE classDefSuperclasses #-}
classDefSuperclasses :: ClassDef -> [Ident]
classDefSuperclasses (ClassDef n) = mapMaybe (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE classDefTypeParams #-}
classDefTypeParams :: ClassDef -> [Ident]
classDefTypeParams (ClassDef n) = mapMaybe (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE classDefDecls #-}
classDefDecls :: ClassDef -> [ClassDecl]
classDefDecls (ClassDef n) = mapMaybe (castToNode @ClassDecl) (nodeChildren n)

pattern ClassDefP :: Maybe Ident -> [Ident] -> [Ident] -> [ClassDecl] -> ClassDef
pattern ClassDefP name superclasses typeParams decls <-
  (\c -> (classDefName c, classDefSuperclasses c, classDefTypeParams c, classDefDecls c) -> (name, superclasses, typeParams, decls))

{-# COMPLETE ClassDefP #-}

instance AstNode ClassDef where
  castToNode node = case nodeKind node of
    SyntaxKindClassDef -> Just (ClassDef node)
    _ -> Nothing

  syntaxNode (ClassDef n) = n

data ClassDecl
  = ClassDeclSig Sig
  | ClassDeclBind Bind
  deriving (Show, Eq)

instance AstNode ClassDecl where
  castToNode node = case nodeKind node of
    SyntaxKindClassDeclSig -> Just (ClassDeclSig (Sig node))
    SyntaxKindClassDeclBind -> ClassDeclBind <$> castToNode @Bind node
    _ -> Nothing

  syntaxNode c = case c of
    ClassDeclSig s -> syntaxNode s
    ClassDeclBind b -> syntaxNode b

newtype InstanceDecl = InstanceDecl SyntaxNode deriving (Show, Eq)

{-# INLINEABLE instanceDeclClassName #-}
instanceDeclClassName :: InstanceDecl -> Maybe Ident
instanceDeclClassName (InstanceDecl n) = findMap (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE instanceDeclTypeParams #-}
instanceDeclTypeParams :: InstanceDecl -> [Ident]
instanceDeclTypeParams (InstanceDecl n) = mapMaybe (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE instanceDeclAnno #-}
instanceDeclAnno :: InstanceDecl -> Maybe Anno
instanceDeclAnno (InstanceDecl n) = findMap (castToNode @Anno) (nodeChildren n)

{-# INLINEABLE instanceDeclClassDecls #-}
instanceDeclClassDecls :: InstanceDecl -> [ClassDecl]
instanceDeclClassDecls (InstanceDecl n) = mapMaybe (castToNode @ClassDecl) (nodeChildren n)

pattern InstanceDeclP :: Maybe Ident -> [Ident] -> Maybe Anno -> [ClassDecl] -> InstanceDecl
pattern InstanceDeclP className typeParams anno decls <-
  (\i -> (instanceDeclClassName i, instanceDeclTypeParams i, instanceDeclAnno i, instanceDeclClassDecls i) -> (className, typeParams, anno, decls))

{-# COMPLETE InstanceDeclP #-}

instance AstNode InstanceDecl where
  castToNode node = case nodeKind node of
    SyntaxKindInstanceDecl -> Just (InstanceDecl node)
    _ -> Nothing

  syntaxNode (InstanceDecl n) = n

newtype Sig = Sig SyntaxNode deriving (Show, Eq)

{-# INLINEABLE sigNames #-}
sigNames :: Sig -> [Ident]
sigNames (Sig node) = mapMaybe (castToNode @Ident) (nodeChildren node)

{-# INLINEABLE sigTypeParams #-}
sigTypeParams :: Sig -> [Ident]
sigTypeParams (Sig node) = mapMaybe (castToNode @Ident) (nodeChildren node)

{-# INLINEABLE sigAnno #-}
sigAnno :: Sig -> Maybe Anno
sigAnno (Sig node) = findMap (castToNode @Anno) (nodeChildren node)

pattern SigP :: [Ident] -> [Ident] -> Maybe Anno -> Sig
pattern SigP names typeParams anno <-
  (\s -> (sigNames s, sigTypeParams s, sigAnno s) -> (names, typeParams, anno))

{-# COMPLETE SigP #-}

instance AstNode Sig where
  castToNode node = case nodeKind node of
    SyntaxKindSig -> Just (Sig node)
    _ -> Nothing

  syntaxNode (Sig n) = n

data Expr
  = ExprLit !Lit
  | ExprIdent !Ident
  | ExprApp !AppExpr
  | ExprLam !LamExpr
  | ExprLet !LetExpr
  | ExprNeg !Expr
  | ExprIf !IfExpr
  | ExprMatch !MatchExpr
  | ExprList !ListExpr
  | ExprTuple !TupleExpr
  | ExprCons !ConsExpr
  | ExprRecord !RecordExpr
  | ExprRecordAccess !RecordAccessExpr
  | ExprRecordUpdate !RecordUpdateExpr
  | ExprUnit !UnitExpr
  deriving (Show, Eq)

instance AstNode Expr where
  castToNode node = case nodeKind node of
    SyntaxKindExprLit -> Just (ExprLit (Lit node))
    SyntaxKindExprIdent -> Just (ExprIdent (Ident node))
    SyntaxKindExprApp -> Just (ExprApp (AppExpr node))
    SyntaxKindExprLam -> Just (ExprLam (LamExpr node))
    SyntaxKindExprLet -> Just (ExprLet (LetExpr node))
    SyntaxKindExprNeg -> ExprNeg <$> castToNode @Expr node
    SyntaxKindExprIf -> Just (ExprIf (IfExpr node))
    SyntaxKindExprMatch -> Just (ExprMatch (MatchExpr node))
    SyntaxKindExprList -> Just (ExprList (ListExpr node))
    SyntaxKindExprTuple -> Just (ExprTuple (TupleExpr node))
    SyntaxKindExprCons -> Just (ExprCons (ConsExpr node))
    SyntaxKindExprRecord -> Just (ExprRecord (RecordExpr node))
    SyntaxKindExprRecordAccess -> Just (ExprRecordAccess (RecordAccessExpr node))
    SyntaxKindExprRecordUpdate -> Just (ExprRecordUpdate (RecordUpdateExpr node))
    SyntaxKindExprUnit -> Just (ExprUnit (UnitExpr node))
    _ -> Nothing

  syntaxNode e = case e of
    ExprLit l -> syntaxNode l
    ExprIdent i -> syntaxNode i
    ExprApp a -> syntaxNode a
    ExprLam l -> syntaxNode l
    ExprLet l -> syntaxNode l
    ExprNeg n -> syntaxNode n
    ExprIf i -> syntaxNode i
    ExprMatch m -> syntaxNode m
    ExprList l -> syntaxNode l
    ExprTuple t -> syntaxNode t
    ExprCons c -> syntaxNode c
    ExprRecord r -> syntaxNode r
    ExprRecordAccess r -> syntaxNode r
    ExprRecordUpdate r -> syntaxNode r
    ExprUnit u -> syntaxNode u

newtype AppExpr = AppExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE appExprFun #-}
appExprFun :: AppExpr -> Maybe Expr
appExprFun (AppExpr n) = findMap (castToNode @Expr) (nodeChildren n)

{-# INLINEABLE appExprArgs #-}
appExprArgs :: AppExpr -> [Expr]
appExprArgs (AppExpr n) = mapMaybe (castToNode @Expr) (nodeChildren n)

pattern AppExprP :: Maybe Expr -> [Expr] -> AppExpr
pattern AppExprP fun args <-
  (\a -> (appExprFun a, appExprArgs a) -> (fun, args))

{-# COMPLETE AppExprP #-}

instance AstNode AppExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprApp -> Just (AppExpr node)
    _ -> Nothing

  syntaxNode (AppExpr n) = n

newtype LamExpr = LamExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE lamExprParams #-}
lamExprParams :: LamExpr -> [Pattern]
lamExprParams (LamExpr n) = mapMaybe (castToNode @Pattern) (nodeChildren n)

{-# INLINEABLE lamExprBody #-}
lamExprBody :: LamExpr -> Maybe Expr
lamExprBody (LamExpr n) = findMap (castToNode @Expr) (nodeChildren n)

pattern LamExprP :: [Pattern] -> Maybe Expr -> LamExpr
pattern LamExprP params body <-
  (\l -> (lamExprParams l, lamExprBody l) -> (params, body))

{-# COMPLETE LamExprP #-}

instance AstNode LamExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprLam -> Just (LamExpr node)
    _ -> Nothing

  syntaxNode (LamExpr n) = n

newtype LetExpr = LetExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE letExprBinds #-}
letExprBinds :: LetExpr -> [Bind]
letExprBinds (LetExpr n) = mapMaybe (castToNode @Bind) (nodeChildren n)

{-# INLINEABLE letExprBody #-}
letExprBody :: LetExpr -> Maybe Expr
letExprBody (LetExpr n) = findMap (castToNode @Expr) (nodeChildren n)

pattern LetExprP :: [Bind] -> Maybe Expr -> LetExpr
pattern LetExprP binds body <-
  (\l -> (letExprBinds l, letExprBody l) -> (binds, body))

{-# COMPLETE LetExprP #-}

instance AstNode LetExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprLet -> Just (LetExpr node)
    _ -> Nothing

  syntaxNode (LetExpr n) = n

newtype IfExpr = IfExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE ifExprCond #-}
ifExprCond :: IfExpr -> Maybe Expr
ifExprCond (IfExpr n) = findMap (castToNode @Expr) (nodeChildren n)

{-# INLINEABLE ifExprThen #-}
ifExprThen :: IfExpr -> Maybe Expr
ifExprThen (IfExpr n) = findMap (castToNode @Expr) (nodeChildren n)

{-# INLINEABLE ifExprElse #-}
ifExprElse :: IfExpr -> Maybe Expr
ifExprElse (IfExpr n) = findMap (castToNode @Expr) (nodeChildren n)

pattern IfExprP :: Maybe Expr -> Maybe Expr -> Maybe Expr -> IfExpr
pattern IfExprP cond thenE elseE <-
  (\i -> (ifExprCond i, ifExprThen i, ifExprElse i) -> (cond, thenE, elseE))

{-# COMPLETE IfExprP #-}

instance AstNode IfExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprIf -> Just (IfExpr node)
    _ -> Nothing

  syntaxNode (IfExpr n) = n

newtype MatchExpr = MatchExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE matchExprScrutinee #-}
matchExprScrutinee :: MatchExpr -> Maybe Expr
matchExprScrutinee (MatchExpr n) = findMap (castToNode @Expr) (nodeChildren n)

{-# INLINEABLE matchExprCases #-}
matchExprCases :: MatchExpr -> [MatchCase]
matchExprCases (MatchExpr n) = mapMaybe (castToNode @MatchCase) (nodeChildren n)

pattern MatchExprP :: Maybe Expr -> [MatchCase] -> MatchExpr
pattern MatchExprP scrutinee cases <-
  (\m -> (matchExprScrutinee m, matchExprCases m) -> (scrutinee, cases))

{-# COMPLETE MatchExprP #-}

instance AstNode MatchExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprMatch -> Just (MatchExpr node)
    _ -> Nothing

  syntaxNode (MatchExpr n) = n

newtype MatchCase = MatchCase SyntaxNode deriving (Show, Eq)

{-# INLINEABLE matchCasePattern #-}
matchCasePattern :: MatchCase -> Maybe Pattern
matchCasePattern (MatchCase n) = findMap (castToNode @Pattern) (nodeChildren n)

{-# INLINEABLE matchCaseRhs #-}
matchCaseRhs :: MatchCase -> Maybe Rhs
matchCaseRhs (MatchCase n) = findMap (castToNode @Rhs) (nodeChildren n)

pattern MatchCaseP :: Maybe Pattern -> Maybe Rhs -> MatchCase
pattern MatchCaseP pat rhs <-
  (\c -> (matchCasePattern c, matchCaseRhs c) -> (pat, rhs))

{-# COMPLETE MatchCaseP #-}

instance AstNode MatchCase where
  castToNode node = case nodeKind node of
    SyntaxKindMatchCase -> Just (MatchCase node)
    _ -> Nothing

  syntaxNode (MatchCase n) = n

newtype ListExpr = ListExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE listExprElems #-}
listExprElems :: ListExpr -> [Expr]
listExprElems (ListExpr n) = mapMaybe (castToNode @Expr) (nodeChildren n)

pattern ListExprP :: [Expr] -> ListExpr
pattern ListExprP elems <- (listExprElems -> elems)

{-# COMPLETE ListExprP #-}

instance AstNode ListExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprList -> Just (ListExpr node)
    _ -> Nothing

  syntaxNode (ListExpr n) = n

newtype TupleExpr = TupleExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE tupleExprElems #-}
tupleExprElems :: TupleExpr -> [Expr]
tupleExprElems (TupleExpr n) = mapMaybe (castToNode @Expr) (nodeChildren n)

pattern TupleExprP :: [Expr] -> TupleExpr
pattern TupleExprP elems <- (tupleExprElems -> elems)

{-# COMPLETE TupleExprP #-}

instance AstNode TupleExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprTuple -> Just (TupleExpr node)
    _ -> Nothing

  syntaxNode (TupleExpr n) = n

newtype ConsExpr = ConsExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE consExprName #-}
consExprName :: ConsExpr -> Maybe Ident
consExprName (ConsExpr n) = findMap (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE consExprArgs #-}
consExprArgs :: ConsExpr -> [Expr]
consExprArgs (ConsExpr n) = mapMaybe (castToNode @Expr) (nodeChildren n)

pattern ConsExprP :: Maybe Ident -> [Expr] -> ConsExpr
pattern ConsExprP name args <-
  (\c -> (consExprName c, consExprArgs c) -> (name, args))

{-# COMPLETE ConsExprP #-}

instance AstNode ConsExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprCons -> Just (ConsExpr node)
    _ -> Nothing

  syntaxNode (ConsExpr n) = n

newtype RecordExpr = RecordExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE recordExprName #-}
recordExprName :: RecordExpr -> Maybe Ident
recordExprName (RecordExpr n) = findMap (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE recordExprFields #-}
recordExprFields :: RecordExpr -> [RecordLitField]
recordExprFields (RecordExpr n) = mapMaybe (castToNode @RecordLitField) (nodeChildren n)

pattern RecordExprP :: Maybe Ident -> [RecordLitField] -> RecordExpr
pattern RecordExprP name fields <-
  (\r -> (recordExprName r, recordExprFields r) -> (name, fields))

{-# COMPLETE RecordExprP #-}

instance AstNode RecordExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprRecord -> Just (RecordExpr node)
    _ -> Nothing

  syntaxNode (RecordExpr n) = n

newtype RecordAccessExpr = RecordAccessExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE recordAccessExprRecord #-}
recordAccessExprRecord :: RecordAccessExpr -> Maybe Expr
recordAccessExprRecord (RecordAccessExpr n) = findMap (castToNode @Expr) (nodeChildren n)

{-# INLINEABLE recordAccessExprField #-}
recordAccessExprField :: RecordAccessExpr -> Maybe Ident
recordAccessExprField (RecordAccessExpr n) = findMap (castToNode @Ident) (nodeChildren n)

pattern RecordAccessExprP :: Maybe Expr -> Maybe Ident -> RecordAccessExpr
pattern RecordAccessExprP record field <-
  (\r -> (recordAccessExprRecord r, recordAccessExprField r) -> (record, field))

{-# COMPLETE RecordAccessExprP #-}

instance AstNode RecordAccessExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprRecordAccess -> Just (RecordAccessExpr node)
    _ -> Nothing

  syntaxNode (RecordAccessExpr n) = n

newtype RecordUpdateExpr = RecordUpdateExpr SyntaxNode deriving (Show, Eq)

{-# INLINEABLE recordUpdateExprRecord #-}
recordUpdateExprRecord :: RecordUpdateExpr -> Maybe Expr
recordUpdateExprRecord (RecordUpdateExpr n) = findMap (castToNode @Expr) (nodeChildren n)

{-# INLINEABLE recordUpdateExprUpdates #-}
recordUpdateExprUpdates :: RecordUpdateExpr -> [RecordLitField]
recordUpdateExprUpdates (RecordUpdateExpr n) = mapMaybe (castToNode @RecordLitField) (nodeChildren n)

pattern RecordUpdateExprP :: Maybe Expr -> [RecordLitField] -> RecordUpdateExpr
pattern RecordUpdateExprP record updates <-
  (\r -> (recordUpdateExprRecord r, recordUpdateExprUpdates r) -> (record, updates))

{-# COMPLETE RecordUpdateExprP #-}

instance AstNode RecordUpdateExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprRecordUpdate -> Just (RecordUpdateExpr node)
    _ -> Nothing

  syntaxNode (RecordUpdateExpr n) = n

newtype RecordLitField = RecordLitField SyntaxNode deriving (Show, Eq)

{-# INLINEABLE recordLitFieldName #-}
recordLitFieldName :: RecordLitField -> Maybe Ident
recordLitFieldName (RecordLitField n) = findMap (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE recordLitFieldValue #-}
recordLitFieldValue :: RecordLitField -> Maybe Expr
recordLitFieldValue (RecordLitField n) = findMap (castToNode @Expr) (nodeChildren n)

pattern RecordLitFieldP :: Maybe Ident -> Maybe Expr -> RecordLitField
pattern RecordLitFieldP name value <-
  (\f -> (recordLitFieldName f, recordLitFieldValue f) -> (name, value))

{-# COMPLETE RecordLitFieldP #-}

instance AstNode RecordLitField where
  castToNode node = case nodeKind node of
    SyntaxKindRecordLitField -> Just (RecordLitField node)
    _ -> Nothing

  syntaxNode (RecordLitField n) = n

newtype UnitExpr = UnitExpr SyntaxNode deriving (Show, Eq)

instance AstNode UnitExpr where
  castToNode node = case nodeKind node of
    SyntaxKindExprUnit -> Just (UnitExpr node)
    _ -> Nothing

  syntaxNode (UnitExpr n) = n

data Bind
  = BindFun FunBind
  | BindPat PatBind
  deriving (Show, Eq)

instance AstNode Bind where
  castToNode node = case nodeKind node of
    SyntaxKindBindFun -> Just (BindFun (FunBind node))
    SyntaxKindBindPat -> Just (BindPat (PatBind node))
    _ -> Nothing

  syntaxNode b = case b of
    BindFun f -> syntaxNode f
    BindPat p -> syntaxNode p

newtype FunBind = FunBind SyntaxNode deriving (Show, Eq)

{-# INLINEABLE funBindName #-}
funBindName :: FunBind -> Maybe Ident
funBindName (FunBind n) = findMap (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE funBindAlts #-}
funBindAlts :: FunBind -> [Alt]
funBindAlts (FunBind n) = mapMaybe (castToNode @Alt) (nodeChildren n)

{-# INLINEABLE funBindWhereDecls #-}
funBindWhereDecls :: FunBind -> [ClassDecl]
funBindWhereDecls (FunBind n) = mapMaybe (castToNode @ClassDecl) (nodeChildren n)

pattern FunBindP :: Maybe Ident -> [Alt] -> [ClassDecl] -> FunBind
pattern FunBindP name alts decls <-
  (\f -> (funBindName f, funBindAlts f, funBindWhereDecls f) -> (name, alts, decls))

{-# COMPLETE FunBindP #-}

instance AstNode FunBind where
  castToNode node = case nodeKind node of
    SyntaxKindBindFun -> Just (FunBind node)
    _ -> Nothing

  syntaxNode (FunBind node) = node

newtype PatBind = PatBind SyntaxNode deriving (Show, Eq)

{-# INLINEABLE patBindPat #-}
patBindPat :: PatBind -> Maybe Pattern
patBindPat (PatBind n) = findMap (castToNode @Pattern) (nodeChildren n)

{-# INLINEABLE patBindRhs #-}
patBindRhs :: PatBind -> Maybe Rhs
patBindRhs (PatBind n) = findMap (castToNode @Rhs) (nodeChildren n)

{-# INLINEABLE patBindWhereDecls #-}
patBindWhereDecls :: PatBind -> [ClassDecl]
patBindWhereDecls (PatBind n) = mapMaybe (castToNode @ClassDecl) (nodeChildren n)

pattern PatBindP :: Maybe Pattern -> Maybe Rhs -> [ClassDecl] -> PatBind
pattern PatBindP pat rhs decls <-
  (\p -> (patBindPat p, patBindRhs p, patBindWhereDecls p) -> (pat, rhs, decls))

{-# COMPLETE PatBindP #-}

instance AstNode PatBind where
  castToNode node = case nodeKind node of
    SyntaxKindBindPat -> Just (PatBind node)
    _ -> Nothing

  syntaxNode (PatBind node) = node

newtype Alt = Alt SyntaxNode deriving (Show, Eq)

{-# INLINEABLE altPatterns #-}
altPatterns :: Alt -> [Pattern]
altPatterns (Alt n) = mapMaybe (castToNode @Pattern) (nodeChildren n)

{-# INLINEABLE altRhs #-}
altRhs :: Alt -> Maybe Rhs
altRhs (Alt n) = findMap (castToNode @Rhs) (nodeChildren n)

pattern AltP :: [Pattern] -> Maybe Rhs -> Alt
pattern AltP pats rhs <-
  (\a -> (altPatterns a, altRhs a) -> (pats, rhs))

{-# COMPLETE AltP #-}

instance AstNode Alt where
  castToNode node = case nodeKind node of
    SyntaxKindAlt -> Just (Alt node)
    _ -> Nothing

  syntaxNode (Alt n) = n

data Rhs
  = RhsExpr !Expr
  | RhsGuard !GuardRhs
  deriving (Show, Eq)

instance AstNode Rhs where
  castToNode node = case nodeKind node of
    SyntaxKindRhsExpr -> RhsExpr <$> castToNode @Expr node
    SyntaxKindRhsGuard -> Just (RhsGuard (GuardRhs node))
    _ -> Nothing

  syntaxNode r = case r of
    RhsExpr e -> syntaxNode e
    RhsGuard g -> syntaxNode g

newtype GuardRhs = GuardRhs SyntaxNode deriving (Show, Eq)

{-# INLINEABLE guardRhsGuards #-}
guardRhsGuards :: GuardRhs -> [Guard]
guardRhsGuards (GuardRhs n) = mapMaybe (castToNode @Guard) (nodeChildren n)

pattern GuardRhsP :: [Guard] -> GuardRhs
pattern GuardRhsP guards <- (guardRhsGuards -> guards)

{-# COMPLETE GuardRhsP #-}

instance AstNode GuardRhs where
  castToNode node = case nodeKind node of
    SyntaxKindRhsGuard -> Just (GuardRhs node)
    _ -> Nothing

  syntaxNode (GuardRhs n) = n

newtype Guard = Guard SyntaxNode deriving (Show, Eq)

{-# INLINEABLE guardPattern #-}
guardPattern :: Guard -> Maybe Pattern
guardPattern (Guard n) = findMap (castToNode @Pattern) (nodeChildren n)

{-# INLINEABLE guardExpr #-}
guardExpr :: Guard -> Maybe Expr
guardExpr (Guard n) = findMap (castToNode @Expr) (nodeChildren n)

pattern GuardP :: Maybe Pattern -> Maybe Expr -> Guard
pattern GuardP pat expr <-
  (\g -> (guardPattern g, guardExpr g) -> (pat, expr))

{-# COMPLETE GuardP #-}

instance AstNode Guard where
  castToNode node = case nodeKind node of
    SyntaxKindGuard -> Just (Guard node)
    _ -> Nothing

  syntaxNode (Guard n) = n

data Anno
  = AnnoVar !VarAnno
  | AnnoIdent !IdentAnno
  | AnnoFun !FunAnno
  | AnnoList !ListAnno
  | AnnoTuple !TupleAnno
  | AnnoUnit !UnitAnno
  deriving (Show, Eq)

instance AstNode Anno where
  castToNode node = case nodeKind node of
    SyntaxKindAnnoVar -> Just (AnnoVar (VarAnno node))
    SyntaxKindAnnoIdent -> Just (AnnoIdent (IdentAnno node))
    SyntaxKindAnnoFun -> Just (AnnoFun (FunAnno node))
    SyntaxKindAnnoList -> Just (AnnoList (ListAnno node))
    SyntaxKindAnnoTuple -> Just (AnnoTuple (TupleAnno node))
    SyntaxKindAnnoUnit -> Just (AnnoUnit (UnitAnno node))
    _ -> Nothing

  syntaxNode a = case a of
    AnnoVar v -> syntaxNode v
    AnnoIdent i -> syntaxNode i
    AnnoFun f -> syntaxNode f
    AnnoList l -> syntaxNode l
    AnnoTuple t -> syntaxNode t
    AnnoUnit u -> syntaxNode u

newtype VarAnno = VarAnno SyntaxNode deriving (Show, Eq)

{-# INLINEABLE varAnnoName #-}
varAnnoName :: VarAnno -> Maybe Ident
varAnnoName (VarAnno n) = findMap (castToNode @Ident) (nodeChildren n)

pattern VarAnnoP :: Maybe Ident -> VarAnno
pattern VarAnnoP name <- (varAnnoName -> name)

{-# COMPLETE VarAnnoP #-}

instance AstNode VarAnno where
  castToNode node = case nodeKind node of
    SyntaxKindAnnoVar -> Just (VarAnno node)
    _ -> Nothing

  syntaxNode (VarAnno n) = n

newtype IdentAnno = IdentAnno SyntaxNode deriving (Show, Eq)

{-# INLINEABLE identAnnoName #-}
identAnnoName :: IdentAnno -> Maybe Ident
identAnnoName (IdentAnno n) = findMap (castToNode @Ident) (nodeChildren n)

pattern IdentAnnoP :: Maybe Ident -> IdentAnno
pattern IdentAnnoP name <- (identAnnoName -> name)

{-# COMPLETE IdentAnnoP #-}

instance AstNode IdentAnno where
  castToNode node = case nodeKind node of
    SyntaxKindAnnoIdent -> Just (IdentAnno node)
    _ -> Nothing

  syntaxNode (IdentAnno n) = n

newtype FunAnno = FunAnno SyntaxNode deriving (Show, Eq)

{-# INLINEABLE funAnnoParamTy #-}
funAnnoParamTy :: FunAnno -> Maybe Anno
funAnnoParamTy (FunAnno n) = findMap (castToNode @Anno) (nodeChildren n)

{-# INLINEABLE funAnnoRetTy #-}
funAnnoRetTy :: FunAnno -> Maybe Anno
funAnnoRetTy (FunAnno n) = findMap (castToNode @Anno) (nodeChildren n)

pattern FunAnnoP :: Maybe Anno -> Maybe Anno -> FunAnno
pattern FunAnnoP param ret <-
  (\f -> (funAnnoParamTy f, funAnnoRetTy f) -> (param, ret))

{-# COMPLETE FunAnnoP #-}

instance AstNode FunAnno where
  castToNode node = case nodeKind node of
    SyntaxKindAnnoFun -> Just (FunAnno node)
    _ -> Nothing

  syntaxNode (FunAnno n) = n

newtype ListAnno = ListAnno SyntaxNode deriving (Show, Eq)

{-# INLINEABLE listAnnoElemTy #-}
listAnnoElemTy :: ListAnno -> Maybe Anno
listAnnoElemTy (ListAnno n) = findMap (castToNode @Anno) (nodeChildren n)

pattern ListAnnoP :: Maybe Anno -> ListAnno
pattern ListAnnoP elemTy <- (listAnnoElemTy -> elemTy)

{-# COMPLETE ListAnnoP #-}

instance AstNode ListAnno where
  castToNode node = case nodeKind node of
    SyntaxKindAnnoList -> Just (ListAnno node)
    _ -> Nothing

  syntaxNode (ListAnno n) = n

newtype TupleAnno = TupleAnno SyntaxNode deriving (Show, Eq)

{-# INLINEABLE tupleAnnoElemTys #-}
tupleAnnoElemTys :: TupleAnno -> [Anno]
tupleAnnoElemTys (TupleAnno n) = mapMaybe (castToNode @Anno) (nodeChildren n)

pattern TupleAnnoP :: [Anno] -> TupleAnno
pattern TupleAnnoP elemTys <- (tupleAnnoElemTys -> elemTys)

{-# COMPLETE TupleAnnoP #-}

instance AstNode TupleAnno where
  castToNode node = case nodeKind node of
    SyntaxKindAnnoTuple -> Just (TupleAnno node)
    _ -> Nothing

  syntaxNode (TupleAnno n) = n

newtype UnitAnno = UnitAnno SyntaxNode deriving (Show, Eq)

instance AstNode UnitAnno where
  castToNode node = case nodeKind node of
    SyntaxKindAnnoUnit -> Just (UnitAnno node)
    _ -> Nothing

  syntaxNode (UnitAnno n) = n

data Pattern
  = PatternWildcard !WildcardPat
  | PatternLiteral !LiteralPat
  | PatternIdent !IdentPat
  | PatternCons !ConsPat
  | PatternAs !AsPat
  | PatternList !ListPat
  | PatternTuple !TuplePat
  | PatternUnit !UnitPat
  deriving (Show, Eq)

instance AstNode Pattern where
  castToNode node = case nodeKind node of
    SyntaxKindPatternWildcard -> Just (PatternWildcard (WildcardPat node))
    SyntaxKindPatternLiteral -> Just (PatternLiteral (LiteralPat node))
    SyntaxKindPatternIdent -> Just (PatternIdent (IdentPat node))
    SyntaxKindPatternCons -> Just (PatternCons (ConsPat node))
    SyntaxKindPatternAs -> Just (PatternAs (AsPat node))
    SyntaxKindPatternList -> Just (PatternList (ListPat node))
    SyntaxKindPatternTuple -> Just (PatternTuple (TuplePat node))
    SyntaxKindPatternUnit -> Just (PatternUnit (UnitPat node))
    _ -> Nothing

  syntaxNode p = case p of
    PatternWildcard w -> syntaxNode w
    PatternLiteral l -> syntaxNode l
    PatternIdent i -> syntaxNode i
    PatternCons c -> syntaxNode c
    PatternAs a -> syntaxNode a
    PatternList l -> syntaxNode l
    PatternTuple t -> syntaxNode t
    PatternUnit u -> syntaxNode u

newtype WildcardPat = WildcardPat SyntaxNode deriving (Show, Eq)

instance AstNode WildcardPat where
  castToNode node = case nodeKind node of
    SyntaxKindUnderscore -> Just (WildcardPat node)
    _ -> Nothing

  syntaxNode (WildcardPat n) = n

newtype LiteralPat = LiteralPat SyntaxNode deriving (Show, Eq)

{-# INLINEABLE literalPatLit #-}
literalPatLit :: LiteralPat -> Maybe Lit
literalPatLit (LiteralPat n) = findMap (castToNode @Lit) (nodeChildren n)

pattern LiteralPatP :: Maybe Lit -> LiteralPat
pattern LiteralPatP lit <- (literalPatLit -> lit)

{-# COMPLETE LiteralPatP #-}

instance AstNode LiteralPat where
  castToNode node = case nodeKind node of
    SyntaxKindPatternLiteral -> Just (LiteralPat node)
    _ -> Nothing

  syntaxNode (LiteralPat n) = n

newtype IdentPat = IdentPat SyntaxNode deriving (Show, Eq)

{-# INLINEABLE identPatName #-}
identPatName :: IdentPat -> Maybe Ident
identPatName (IdentPat n) = findMap (castToNode @Ident) (nodeChildren n)

pattern IdentPatP :: Maybe Ident -> IdentPat
pattern IdentPatP ident <- (identPatName -> ident)

{-# COMPLETE IdentPatP #-}

instance AstNode IdentPat where
  castToNode node = case nodeKind node of
    SyntaxKindPatternIdent -> Just (IdentPat node)
    _ -> Nothing

  syntaxNode (IdentPat n) = n

newtype ConsPat = ConsPat SyntaxNode deriving (Show, Eq)

{-# INLINEABLE consPatName #-}
consPatName :: ConsPat -> Maybe Ident
consPatName (ConsPat n) = findMap (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE consPatParams #-}
consPatParams :: ConsPat -> [Pattern]
consPatParams (ConsPat n) = mapMaybe (castToNode @Pattern) (nodeChildren n)

pattern ConsPatP :: Maybe Ident -> [Pattern] -> ConsPat
pattern ConsPatP name params <-
  (\p -> (consPatName p, consPatParams p) -> (name, params))

{-# COMPLETE ConsPatP #-}

instance AstNode ConsPat where
  castToNode node = case nodeKind node of
    SyntaxKindPatternCons -> Just (ConsPat node)
    _ -> Nothing

  syntaxNode (ConsPat n) = n

newtype AsPat = AsPat SyntaxNode deriving (Show, Eq)

{-# INLINEABLE asPatName #-}
asPatName :: AsPat -> Maybe Ident
asPatName (AsPat n) = findMap (castToNode @Ident) (nodeChildren n)

{-# INLINEABLE asPatPattern #-}
asPatPattern :: AsPat -> Maybe Pattern
asPatPattern (AsPat n) = findMap (castToNode @Pattern) (nodeChildren n)

pattern AsPatP :: Maybe Ident -> Maybe Pattern -> AsPat
pattern AsPatP name pat <-
  (\p -> (asPatName p, asPatPattern p) -> (name, pat))

{-# COMPLETE AsPatP #-}

instance AstNode AsPat where
  castToNode node = case nodeKind node of
    SyntaxKindPatternAs -> Just (AsPat node)
    _ -> Nothing

  syntaxNode (AsPat n) = n

newtype ListPat = ListPat SyntaxNode deriving (Show, Eq)

{-# INLINEABLE listPatElements #-}
listPatElements :: ListPat -> [Pattern]
listPatElements (ListPat n) = mapMaybe (castToNode @Pattern) (nodeChildren n)

pattern ListPatP :: [Pattern] -> ListPat
pattern ListPatP elems <- (listPatElements -> elems)

{-# COMPLETE ListPatP #-}

instance AstNode ListPat where
  castToNode node = case nodeKind node of
    SyntaxKindPatternList -> Just (ListPat node)
    _ -> Nothing

  syntaxNode (ListPat n) = n

newtype TuplePat = TuplePat SyntaxNode deriving (Show, Eq)

{-# INLINEABLE tuplePatElements #-}
tuplePatElements :: TuplePat -> [Pattern]
tuplePatElements (TuplePat n) = mapMaybe (castToNode @Pattern) (nodeChildren n)

pattern TuplePatP :: [Pattern] -> TuplePat
pattern TuplePatP elems <- (tuplePatElements -> elems)

{-# COMPLETE TuplePatP #-}

instance AstNode TuplePat where
  castToNode node = case nodeKind node of
    SyntaxKindPatternTuple -> Just (TuplePat node)
    _ -> Nothing

  syntaxNode (TuplePat n) = n

newtype UnitPat = UnitPat SyntaxNode deriving (Show, Eq)

instance AstNode UnitPat where
  castToNode node = case nodeKind node of
    SyntaxKindPatternUnit -> Just (UnitPat node)
    _ -> Nothing

  syntaxNode (UnitPat n) = n

newtype Ident = Ident SyntaxNode
  deriving (Show, Eq)

instance AstNode Ident where
  castToNode node = case nodeKind node of
    SyntaxKindLowercaseIdent -> Just (Ident node)
    SyntaxKindUppercaseIdent -> Just (Ident node)
    SyntaxKindOpIdent -> Just (Ident node)
    _ -> Nothing

  syntaxNode (Ident n) = n

newtype Lit = Lit SyntaxNode deriving (Show, Eq, Ord)

instance AstNode Lit where
  castToNode node = case nodeKind node of
    SyntaxKindLiteral -> Just (Lit node)
    _ -> Nothing

  syntaxNode (Lit n) = n

-- Helpers
findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = listToMaybe . mapMaybe f