module CSPM.Syntax.Constructors (
    mkApplication, mkLocatedApplication,
    mkLit,
    mkVar,
) where

import CSPM.Syntax.AST
import CSPM.Syntax.Literals
import CSPM.Syntax.Names
import CSPM.Syntax.Types
import Util.Annotated
import Util.Exception

errorThunk = panic "PType evaluated"

mkApplication :: Name -> Type -> [TCExp] -> TCExp
mkApplication fn fnTyp args =
    let TFunction _ tr = fnTyp
    in An Unknown (tr, errorThunk) (App (mkVar fn fnTyp) args)

mkLocatedApplication :: Name -> Type -> [TCExp] -> TCExp
mkLocatedApplication fn fnTyp args =
    let TFunction _ tr = fnTyp
    in An Unknown (tr, errorThunk) (LocatedApp (mkVar fn fnTyp) args)

mkLit :: Literal -> TCExp
mkLit l = An Unknown (typeOfLiteral l, errorThunk) (Lit l)

mkVar :: Name -> Type -> TCExp
mkVar n typ = An Unknown (typ, errorThunk) (Var n)
