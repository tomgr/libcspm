module CSPM.Symmetry.Detector (
    analyseFile,
    analyseDecl,
    analyseExp,
    analyseAssertion,
) where

import qualified Data.Map as M

import CSPM.Prelude
import CSPM.Syntax.AST
import CSPM.Syntax.Names
import CSPM.Syntax.Types
import CSPM.Symmetry.DependencyGraph
import CSPM.Symmetry.Monad
import Util.Annotated
import Util.Exception

analyseFile :: TCCSPMFile -> SymmetryMonad ()
analyseFile (An _ _ (CSPMFile ds)) = do
    mapM_ (uncurry registerType) (concatMap findTypes ds)
    mds <- registerDeclarations ds
    mapM_ (\ (a, b) -> analyseDecl a b) mds

-- | Extracts all datatype declarations from the given declaration.
findTypes :: TCDecl -> [(Type, [Name])]
findTypes (An _ _ (DataType n cs)) = [(TDatatype n, map extractClause cs)]
    where
        extractClause (An _ _ (DataTypeClause n _ _)) = n
findTypes (An _ _ (Module _ _ priv pub)) = concatMap findTypes (pub++priv)
findTypes (An _ (x, y) (ModuleInstance _ _ _ nameMap (Just target))) =
        map convertDataType (findTypes target)
    where
        convertDataType (TDatatype n, ns) =
            (TDatatype (applyNameMap n), map applyNameMap ns)
        applyNameMap n =
            case M.lookup n nameMap of
                Just n' -> n'
                Nothing -> panic $ "Type name not found "++show n
findTypes (An _ _ (TimedSection _ _ ds)) = concatMap findTypes ds
findTypes (An _ _ (Assert _)) = []
findTypes (An _ _ (Channel _ _ _)) = []
findTypes (An _ _ (External _)) = []
findTypes (An _ _ (FunBind _ _ _)) = []
findTypes (An _ _ (NameType _ _ _)) = []
findTypes (An _ _ (PatBind _ _ _)) = []
findTypes (An _ _ (PrintStatement _)) = []
findTypes (An _ _ (SubType _ _)) = []
findTypes (An _ _ (Transparent _)) = []

extractDatatype :: Type -> Type
extractDatatype (TDotable t1 t2) = extractDatatype t2
extractDatatype t = t

analyseDecl :: TCDecl -> Maybe DependencyGraphNode -> SymmetryMonad ()
analyseDecl (An _ _ (Assert _)) Nothing = return ()
analyseDecl (An _ _ (External _)) Nothing = return ()
analyseDecl (An _ _ (PrintStatement _)) Nothing = return ()
analyseDecl (An _ _ (Transparent _)) Nothing = return ()

analyseDecl (An _ _ (Channel _ me _)) (Just n) = symmetryScope $ do
    analyseMaybeExp me
    recordDeclerationNonSymmetries n
analyseDecl (An _ _ (DataType _ cs)) (Just n) = symmetryScope $ do
    mapM_ analyseDatatypeClause cs
    recordDeclerationNonSymmetries n
analyseDecl (An _ _ (FunBind na ms _)) (Just n) = symmetryScope $ do
    mapM_ analyseMatch ms
    recordDeclerationNonSymmetries n
analyseDecl (An _ _ (Module _ es _ _)) (Just n) = symmetryScope $ do
    mapM_ analysePat es
    recordDeclerationNonSymmetries n
analyseDecl (An _ _ (ModuleInstance _ _ es _ _)) (Just n) = symmetryScope $ do
    mapM_ analyseExp es
    recordDeclerationNonSymmetries n
analyseDecl (An _ _ (PatBind p e _)) (Just n) = symmetryScope $ do
    analysePat p
    analyseExp e
    recordDeclerationNonSymmetries n
analyseDecl (An _ _ (NameType _ e _)) (Just n) = symmetryScope $ do
    analyseExp e
    recordDeclerationNonSymmetries n
analyseDecl (An _ _ (SubType _ cs)) (Just n) = symmetryScope $ do
    mapM_ (\ (An loc (typ, ptyp) (DataTypeClause n me _)) -> do
        let constructorType = extractDatatype typ
        recordNonSymmetryReason constructorType (ExpContainsConstant (An loc (typ, ptyp) (Var n)) n)
        analyseMaybeExp me
        ) cs
    recordDeclerationNonSymmetries n
analyseDecl (An _ _ (TimedSection _ e _)) (Just n) = symmetryScope $ do
    analyseMaybeExp e
    recordDeclerationNonSymmetries n

analyseAssertion :: TCAssertion -> SymmetryMonad ()
analyseAssertion (An _ _ (Refinement s _ i opts)) = do
    analyseExp s
    analyseExp i
    mapM_ analyseModelOption opts
analyseAssertion (An _ _ (PropertyCheck p _ _ opts)) = do
    analyseExp p
    mapM_ analyseModelOption opts
analyseAssertion (An _ _ (SymmetryCheck e _)) =
    analyseExp e
analyseAssertion (An _ _ (ASNot a)) = analyseAssertion a

analyseModelOption :: TCModelOption -> SymmetryMonad ()
analyseModelOption (An _ _ (TauPriority e)) = analyseExp e
analyseModelOption (An _ _ (PartialOrderReduce _)) = return ()
analyseModelOption (An _ _ (SymmetryReduce _)) = return ()

analyseDatatypeClause :: TCDataTypeClause -> SymmetryMonad ()
analyseDatatypeClause (An _ _ (DataTypeClause _ me _)) = analyseMaybeExp me

analyseMatch :: TCMatch -> SymmetryMonad ()
analyseMatch (An _ _ (Match p e)) = do
    mapM_ analysePat (concat p)
    analyseExp e

isNameDataConstant :: Type -> Name -> Bool
isNameDataConstant (TDatatype _) n = isNameDataConstructor n
isNameDataConstant _ _ = False

analysePat :: TCPat -> SymmetryMonad ()
analysePat (An _ _ (PConcat p1 p2)) = do
    analysePat p1
    analysePat p2
analysePat (An _ _ (PDotApp p1 p2)) = do
    analysePat p1
    analysePat p2
analysePat (An _ _ (PDoublePattern p1 p2)) = do
    analysePat p1
    analysePat p2
analysePat (An _ _ (PList ps)) = mapM_ analysePat ps
analysePat (An _ _ (PLit l)) = return ()
analysePat (An _ _ (PParen p)) = analysePat p
analysePat (An _ _ (PSet ps)) = mapM_ analysePat ps
analysePat (An _ _ (PTuple ps)) = mapM_ analysePat ps
analysePat pat@(An _ (typ, _) (PVar n)) | isNameDataConstant typ n = do
    let constructorType = extractDatatype typ
    recordNonSymmetryReason constructorType (PatContainsConstant pat n)
analysePat (An _ _ (PVar n)) = return ()
analysePat (An _ _ PWildCard) = return ()

analyseExp :: TCExp -> SymmetryMonad ()
analyseExp (An _ _ (App (An _ _ (Var n)) args)) | n == builtInName "error" = do
    -- There is no need to analyse the right-hand side of an error call, since
    -- it will always be an error.
    return ()
analyseExp (An _ _ (LocatedApp (An _ _ (Var n)) args)) | n == builtInName "error" = return ()
analyseExp (An _ _ (App fn args)) = do
    analyseExp fn
    mapM_ analyseExp args
analyseExp (An _ _ (LocatedApp fn args)) = do
    analyseExp fn
    mapM_ analyseExp args
analyseExp (An _ _ (BooleanBinaryOp _ e1 e2)) = do
    analyseExp e1
    analyseExp e2
analyseExp (An _ _ (BooleanUnaryOp _ e)) = analyseExp e
analyseExp (An _ _ (Concat e1 e2)) = do
    analyseExp e1
    analyseExp e2
analyseExp (An _ _ (DotApp e1 e2)) = do
    analyseExp e1
    analyseExp e2
analyseExp (An _ _ (If e1 e2 e3)) = do
    analyseExp e1
    analyseExp e2
    analyseExp e3
analyseExp (An _ _ (Lambda p e)) = do
    mapM_ analysePat p
    analyseExp e
analyseExp (An _ _ (Let _ e)) = analyseExp e
analyseExp (An _ _ (Lit l)) = return ()
analyseExp (An _ _ (List items)) = mapM_ analyseExp items
analyseExp (An _ _ (ListComp items stmts)) = do
    mapM_ analyseExp items
    mapM_ analyseStmt stmts
analyseExp (An _ _ (ListEnumFrom lb)) = analyseExp lb
analyseExp (An _ _ (ListEnumFromTo lb ub)) = do
    analyseExp lb
    analyseExp ub
analyseExp (An _ _ (ListEnumFromComp lb stmts)) = do
    analyseExp lb
    mapM_ analyseStmt stmts
analyseExp (An _ _ (ListEnumFromToComp lb ub stmts)) = do
    analyseExp lb
    analyseExp ub
    mapM_ analyseStmt stmts
analyseExp (An _ _ (ListLength e)) = analyseExp e
analyseExp (An _ _ (Map kvs)) = do
    mapM_ (\ (a, b) -> analyseExp a >> analyseExp b) kvs
analyseExp (An _ _ (MathsBinaryOp _ left right)) = do
    analyseExp left
    analyseExp right
analyseExp (An _ _ (MathsUnaryOp _ e)) = analyseExp e
analyseExp (An _ _ (Paren e)) = analyseExp e
analyseExp (An _ _ (Set items)) = do
    mapM_ analyseExp items
analyseExp (An _ _ (SetComp items stmts)) = do
    mapM_ analyseExp items
    mapM_ analyseStmt stmts
analyseExp (An _ _ (SetEnum items)) = do
    mapM_ analyseExp items
analyseExp (An _ _ (SetEnumComp items stmts)) = do
    mapM_ analyseExp items
    mapM_ analyseStmt stmts
analyseExp (An _ _ (SetEnumFrom lb)) = analyseExp lb
analyseExp (An _ _ (SetEnumFromTo lb ub)) = do
    analyseExp lb
    analyseExp ub
analyseExp (An _ _ (SetEnumFromComp lb stmts)) = do
    analyseExp lb
    mapM_ analyseStmt stmts
analyseExp (An _ _ (SetEnumFromToComp lb ub stmts)) = do
    analyseExp lb
    analyseExp ub
    mapM_ analyseStmt stmts
analyseExp (An _ _ (Tuple es)) = mapM_ analyseExp es
analyseExp exp@(An _ (typ, _) (Var n)) | isNameDataConstant typ n = do
    let constructorType = extractDatatype typ
    recordNonSymmetryReason constructorType (ExpContainsConstant exp n)
analyseExp (An loc (typ, _) (Var n)) = analyseTypeVariables n loc typ

analyseExp (An _ _ (AlphaParallel left alphaLeft alphaRight right)) = do
    analyseExp left
    analyseExp alphaLeft
    analyseExp alphaRight
    analyseExp right
analyseExp (An _ _ (Exception left alpha right)) = do
    analyseExp left
    analyseExp alpha
    analyseExp right
analyseExp (An _ _ (ExternalChoice left right)) = do
    analyseExp left
    analyseExp right
analyseExp (An _ _ (GenParallel left alpha right)) = do
    analyseExp left
    analyseExp alpha
    analyseExp right
analyseExp (An _ _ (GuardedExp guard p)) = do
    analyseExp guard
    analyseExp p
analyseExp (An _ _ (Hiding p a)) = do
    analyseExp p
    analyseExp a
analyseExp (An _ _ (InternalChoice left right)) = do
    analyseExp left
    analyseExp right
analyseExp (An _ _ (Interrupt left right)) = do
    analyseExp left
    analyseExp right
analyseExp (An _ _ (Interleave left right)) = do
    analyseExp left
    analyseExp right
analyseExp (An _ _ (LinkParallel left ties stmts right)) = do
    analyseExp left
    mapM_ (\ (l, r) -> analyseExp l >> analyseExp r) ties
    mapM_ analyseStmt stmts
    analyseExp right
analyseExp (An _ _ (Prefix c fs p)) = do
    analyseExp c
    mapM_ analyseField fs
    analyseExp p
analyseExp (An _ _ (Project p a)) = do
    analyseExp p
    analyseExp a
analyseExp (An _ _ (Rename p ties stmts)) = do
    analyseExp p
    mapM_ (\ (l, r) -> analyseExp l >> analyseExp r) ties
    mapM_ analyseStmt stmts
analyseExp (An _ _ (SequentialComp l r)) = do
    analyseExp l
    analyseExp r
analyseExp (An _ _ (SlidingChoice l r)) = do
    analyseExp l
    analyseExp r
analyseExp (An _ _ (SynchronisingExternalChoice l a r)) = do
    analyseExp l
    analyseExp a
    analyseExp r
analyseExp (An _ _ (SynchronisingInterrupt l a r)) = do
    analyseExp l
    analyseExp a
    analyseExp r
analyseExp (An _ _ (ReplicatedAlphaParallel stmts alpha p)) = do
    mapM_ analyseStmt stmts
    analyseExp alpha
    analyseExp p
analyseExp (An _ _ (ReplicatedExternalChoice stmts p)) = do
    mapM_ analyseStmt stmts
    analyseExp p
analyseExp (An _ _ (ReplicatedInterleave stmts p)) = do
    mapM_ analyseStmt stmts
    analyseExp p
analyseExp (An _ _ (ReplicatedInternalChoice stmts e)) = do
    mapM_ analyseStmt stmts
    analyseExp e
analyseExp (An _ _ (ReplicatedLinkParallel ties tieStmts stmts p)) = do
    mapM_ (\ (l, r) -> analyseExp l >> analyseExp r) ties
    mapM_ analyseStmt tieStmts
    mapM_ analyseStmt stmts
    analyseExp p
analyseExp (An _ _ (ReplicatedParallel alpha stmts p)) = do
    analyseExp alpha
    mapM_ analyseStmt stmts
    analyseExp p
analyseExp (An _ _ (ReplicatedSequentialComp stmts e)) = do
    mapM_ analyseStmt stmts
    analyseExp e
analyseExp (An _ _ (ReplicatedSynchronisingExternalChoice alpha stmts e)) = do
    analyseExp alpha
    mapM_ analyseStmt stmts
    analyseExp e

analyseExp e = panic $ "Unexpected expression "++show e

analyseMaybeExp :: Maybe TCExp -> SymmetryMonad ()
analyseMaybeExp (Just e) = analyseExp e
analyseMaybeExp Nothing = return ()

analyseStmt :: TCStmt -> SymmetryMonad ()
analyseStmt (An _ _ (Generator p e)) = do
    analysePat p
    analyseExp e
analyseStmt (An _ _ (Qualifier e)) = analyseExp e

analyseField :: TCField -> SymmetryMonad ()
analyseField (An _ _ (Input p e)) = do
    analysePat p
    analyseMaybeExp e
analyseField (An _ _ (Output e)) = analyseExp e
analyseField (An _ _ (NonDetInput p e)) = do
    analysePat p
    analyseMaybeExp e
    