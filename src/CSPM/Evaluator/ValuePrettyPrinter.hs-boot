{-# LANGUAGE FlexibleContexts, FlexibleInstances, IncoherentInstances, 
    MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances #-}
module CSPM.Evaluator.ValuePrettyPrinter () where

import Control.Applicative
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Monad
import CSPM.Evaluator.ProcessValues
import {-# SOURCE #-} CSPM.Evaluator.Values
import {-# SOURCE #-} CSPM.Evaluator.ValueSet
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Util.PrettyPrint
import qualified Util.MonadicPrettyPrint as M

instance PrettyPrintable Event
instance PrettyPrintable (S.Seq Event)
instance PrettyPrintable ProcName
instance PrettyPrintable Value
instance PrettyPrintable ValueSet
instance PrettyPrintable UnCompiledProc
instance PrettyPrintable ProcOperator
instance PrettyPrintable ScopeIdentifier
instance (Applicative m, Monad m, M.MonadicPrettyPrintable m Value) =>
        M.MonadicPrettyPrintable m Event
instance (Applicative m, Monad m) => M.MonadicPrettyPrintable m ProcOperator
instance (F.Foldable f) => M.MonadicPrettyPrintable EvaluationMonad (f Event)
instance (Applicative m, Monad m, M.MonadicPrettyPrintable m Value) =>
        M.MonadicPrettyPrintable m ProcName
instance (Applicative m, F.Foldable seq, Functor seq, Monad m, 
            M.MonadicPrettyPrintable m pn, M.MonadicPrettyPrintable m ev,
            M.MonadicPrettyPrintable m evs) => 
        M.MonadicPrettyPrintable m (Proc seq CSPOperator pn ev evs (seq (ev,ev)))
instance (Applicative m, Monad m,
        M.MonadicPrettyPrintable m TCExp,
        M.MonadicPrettyPrintable m UnCompiledProc,
        M.MonadicPrettyPrintable m ValueSet) =>
        M.MonadicPrettyPrintable m Value where
instance M.MonadicPrettyPrintable EvaluationMonad ValueSet