module CSPM.Evaluator.Environment (
    module Util.HierarchicalMap,
    Environment,
) where

import qualified Data.Map as M
import Prelude

import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.HierarchicalMap

type Environment = HierarchicalMap Name Value
