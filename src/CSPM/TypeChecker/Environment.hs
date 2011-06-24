module CSPM.TypeChecker.Environment (
	module Util.HierarchicalMap,
	Environment
)
where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Types
import Util.HierarchicalMap

type Environment = HierarchicalMap Name TypeScheme
