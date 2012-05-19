{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module CSPM.Operators.Custom.Syntax where

import Control.Monad
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import qualified CSPM.Operators.Custom.OpSemDataStructures as OpSem
import qualified CSPM.Operators.Custom.OpSemPrettyPrinter as OpSem
import CSPM.Desugar
import CSPM.Renamer
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Compressor
import CSPM.TypeChecker.Dependencies
import CSPM.TypeChecker.Expr
import CSPM.TypeChecker.Monad
import Util.Monad
import Util.PrettyPrint hiding (($$))

type CustomModule = PModule Process
type CustomDecl = PDecl Process
type CustomMatch = PMatch Process
type CustomPat = PPat
type CustomExp = PExp Process
type CustomStmt = PStmt Process
type CustomDataTypeClause = PDataTypeClause Process
type CustomAssertion = PAssertion Process
type CustomInteractiveStmt = PInteractiveStmt Process

data CustomParserContext = CustomParserContext {
        uncompiledOperators :: OpSem.OpSemDefinition,
        compiledOperators :: [OpSem.CompiledOp]
    }
    deriving Show

data Process id =
    ReplicatedUserOperator {
        repOperatorName :: Name,
        repOperatorArguments :: [AnExp id Process],
        repOperatorStatments :: [AnStmt id Process],
        operatorDefinition :: CustomParserContext
    }
    | UserOperator {
        userOperatorName :: Name,
        userOperatorArguments :: [AnExp id Process],
        operatorDefinition :: CustomParserContext
    }

instance Eq id => Eq (Process id) where
    (ReplicatedUserOperator op args stmts _) == (ReplicatedUserOperator op' args' stmts' _) =
        op == op' && args == args' && stmts == stmts'
    (UserOperator op args _) == (UserOperator op' args' _) =
        op == op' && args == args'

instance PrettyPrintable id => PrettyPrintable (Process id) where
    prettyPrint (ReplicatedUserOperator id es stmts _) =
        prettyPrint id <> parens (list [angles 
                (prettyPrint e <+> char '|' <+> list (map prettyPrint stmts)) 
            | e <- es])
    prettyPrint (UserOperator id es _) =
        prettyPrint id <>
            case es of
                [] -> empty
                _ -> parens (list (map prettyPrint es))

instance Desugarable (Process Name) where
    desugar (ReplicatedUserOperator id es stmts def) =
        ReplicatedUserOperator id (desugar es) (desugar stmts) def
    desugar (UserOperator id es def) = UserOperator id (desugar es) def

instance Renamable (Process UnRenamedName) (Process Name) where
    rename (ReplicatedUserOperator id es stmts def) = do
        (stmts', es') <- renameStatements stmts (rename es)
        return $ ReplicatedUserOperator id es' stmts' def
    rename (UserOperator id es def) =
        return (UserOperator id) $$ rename es $$ return def

instance Dependencies (Process Name) where
    dependencies' (ReplicatedUserOperator n es stmts _) =
        dependenciesStmts stmts es
    dependencies' (UserOperator n args _) = dependencies' args

instance Compressable (Process Name) where
    mcompress (ReplicatedUserOperator opname es stmts defn) =
        return ReplicatedUserOperator $$ return opname $$ mcompress es $$ 
            mcompress stmts $$ return defn
    mcompress (UserOperator opname es defn) =
        return UserOperator $$ return opname $$ mcompress es $$ return defn

instance TypeCheckable (Process Name) Type where
    errorContext _ = Nothing
    --typeCheck' (ReplicatedUserOperator op es stmts) =
    typeCheck' (UserOperator opname es defn) = do
        let op = head [op | op <- OpSem.operators (uncompiledOperators defn),
                                OpSem.opFriendlyName op == opname]
        ts <- zipWithM (\ e (n, t) -> typeCheckExpect e (opSemTypeToType t)) es (OpSem.opArgs op)
        return $ TProc

opSemTypeToType :: OpSem.Type -> Type
--opSemTypeToType (OpSem.TVar (OpSem.TypeVarRef (TypeVar ix) pt)) =
--    TVar (TypeVarRef tv [] pt)
opSemTypeToType (OpSem.TEvent) = TEvent
--opSemTypeToType OpSem.TProcArg = TEvent
opSemTypeToType (OpSem.TOnProcess st) = TProc
opSemTypeToType (OpSem.TOffProcess st) = TProc
opSemTypeToType (OpSem.TSet t) = TSet (opSemTypeToType t)
--opSemTypeToType (OpSem.TChannel ts) = 
opSemTypeToType (OpSem.TTuple ts) = TTuple (map opSemTypeToType ts)
--opSemTypeToType (OpSem.TOperator ts) =
