{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module CSPM.Symmetry.DeclarationFinder (
    FindDeclarations(..),
)
where

import Control.Monad.State
import CSPM.Syntax.AST
import CSPM.Syntax.Visitor

class Visitable a (State [TCDecl]) => FindDeclarations a where
    -- | Finds all declarations contained in the items. This only returns the
    -- top-level declarations in a. For instance, if a is TCDecl, this returns
    -- the declaration itself, and if a is TCExp, it will contain the top-level
    -- declarations in any let expressions.
    findDeclarations :: a -> [TCDecl]
    findDeclarations a = execState (visit visitor a) []
        where
            visitor = defaultVisitor {
                    visitDecl = \ d _ -> modify (\ ds -> d:ds)
                }

instance FindDeclarations a => FindDeclarations [a]
instance FindDeclarations a => FindDeclarations (Maybe a)
instance FindDeclarations TCAssertion
instance FindDeclarations TCCSPMFile
instance FindDeclarations TCDecl
instance FindDeclarations TCDataTypeClause
instance FindDeclarations TCExp
instance FindDeclarations TCInteractiveStmt
instance FindDeclarations TCMatch
