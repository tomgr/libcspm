{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module CSPM.Symmetry.Dependency (
    Dependencies(..)
) where

import Control.Monad.State
import Data.List (nubBy, sortBy)

import CSPM.Prelude
import CSPM.Syntax.AST
import CSPM.Syntax.Names
import CSPM.Syntax.Visitor
import Util.Annotated

-- | Computes the dependencies of a name, specialised for symmetry detection.
--
-- The dependencies of a name include the set of all names  something depends on
-- except those within error statements.
class Visitable a (State [(SrcSpan, Name)]) => Dependencies a where
    dependencies :: a -> [(SrcSpan, Name)]
    dependencies a = nubBy eqSnd $ sortBy cmpSnd $ execState (visit visitor a) []
        where
            cmpSnd (_, a) (_, b) = compare a b
            eqSnd (_, a) (_, b) = a == b

            visitExp (An _ _ (App (An _ _ (Var n)) args)) _ | n == builtInName "error" =
                return ()
            visitExp (An loc _ (Var n)) _ = modify (\ st -> (loc, n) : st)
            visitExp _ prog = prog
            visitPat (An loc _ (PVar n)) _ | isNameDataConstructor n =
                modify (\ st -> (loc, n) : st)
            visitPat _ prog = prog

            visitor = defaultVisitor {
                    visitExp = visitExp,
                    visitPat = visitPat
                }

instance Dependencies TCAssertion
instance Dependencies TCCSPMFile
instance Dependencies TCDecl
instance Dependencies TCExp
instance Dependencies [TCExp]
instance Dependencies (Maybe TCExp)
instance Dependencies TCInteractiveStmt
