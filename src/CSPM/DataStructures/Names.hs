{-# LANGUAGE DeriveDataTypeable #-}
module CSPM.DataStructures.Names where
import Data.Typeable

data Name = 
	Name String
	| InternalName String
	deriving (Eq, Ord, Typeable, Show)

isInternal :: Name -> Bool
isInternal (Name _) = False
isInternal (InternalName _) = True

mkInternalName :: String -> Name
mkInternalName = InternalName

data QualifiedName =
	UnQual Name
	deriving (Eq, Show)
