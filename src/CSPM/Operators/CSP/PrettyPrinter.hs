module CSPM.Operators.CSP.PrettyPrinter () where

import CSPM.DataStructures.Syntax
import CSPM.Operators.CSP.Syntax
import CSPM.PrettyPrinter
import Util.PrettyPrint

instance PrettyPrintable id => PrettyPrintable (CSPProcess id) where
    -- Processes
    prettyPrint(AlphaParallel e1 a1 a2 e2) =
        ppBinOp (prettyPrint e1) (brackets (prettyPrint a1 <> text "||"
                                    <> prettyPrint a2)) (prettyPrint e2)
    prettyPrint (Exception e1 a e2) =
        ppBinOp (prettyPrint e1) (text "[|" <+> prettyPrint a <+> text "|>") 
                (prettyPrint e2)
    prettyPrint (ExternalChoice e1 e2) = 
        ppBinOp (prettyPrint e1) (text "[]") (prettyPrint e2)
    prettyPrint (GenParallel e1 alpha e2) = 
        ppBinOp (prettyPrint e1) (brackets (bars (prettyPrint alpha))) 
                (prettyPrint e2)
    prettyPrint (GuardedExp e1 e2) = 
        ppBinOp (prettyPrint e1) (char '&') (prettyPrint e2)
    prettyPrint (Hiding e1 e2) = 
        ppBinOp (prettyPrint e1) (char '\\') (prettyPrint e2)
    prettyPrint (InternalChoice e1 e2) =
        ppBinOp (prettyPrint e1) (text "|~|") (prettyPrint e2)
    prettyPrint (Interrupt e1 e2) =
        ppBinOp (prettyPrint e1) (text "/\\") (prettyPrint e2)
    prettyPrint (Interleave e1 e2) =
        ppBinOp (prettyPrint e1) (text "|||") (prettyPrint e2)
    prettyPrint (LinkParallel e1 ties stmts e2) =
        ppBinOp (prettyPrint e1)
                (ppComp' (map ppTie ties) stmts)
                (prettyPrint e2)
    prettyPrint (Prefix ev fs e) =
        ppBinOp (prettyPrint ev <> hcat (map prettyPrint fs)) (text "->")
                (prettyPrint e)
    prettyPrint (Rename e ties stmts) =
        prettyPrint e <+> brackets (brackets (
                ppComp' (map ppRename ties) stmts
            ))
    prettyPrint (SequentialComp e1 e2) =
        ppBinOp (prettyPrint e1) (char ';') (prettyPrint e2)
    prettyPrint (SlidingChoice e1 e2) =
        ppBinOp (prettyPrint e1) (text "[>") (prettyPrint e2)

    prettyPrint (ReplicatedAlphaParallel stmts alpha e) = 
        ppRepOp (text "||") stmts 
                (brackets (prettyPrint alpha) <+> prettyPrint e)
    prettyPrint (ReplicatedExternalChoice stmts e) = 
        ppRepOp (text "[]") stmts (prettyPrint e)
    prettyPrint (ReplicatedInterleave stmts e) = 
        ppRepOp (text "|||") stmts (prettyPrint e)
    prettyPrint (ReplicatedInternalChoice stmts e) = 
        ppRepOp (text "|~|") stmts (prettyPrint e)
    prettyPrint (ReplicatedLinkParallel ties tiesStmts stmts e) =
        ppRepOp (brackets (ppComp' (map ppTie ties) tiesStmts)) 
                stmts (prettyPrint e)
    prettyPrint (ReplicatedParallel alpha stmts e) =
        ppRepOp (brackets (bars (prettyPrint alpha))) stmts (prettyPrint e)
    
instance PrettyPrintable id => PrettyPrintable (CSPField id) where
    prettyPrint (Output exp) = 
        char '!' <> prettyPrint exp
    prettyPrint (Input pat Nothing) =
        char '?' <> prettyPrint pat
    prettyPrint (Input pat (Just exp)) =
        char '?' <> prettyPrint pat <+> colon <+> prettyPrint exp
    prettyPrint (NonDetInput pat Nothing) = 
        char '$' <> prettyPrint pat
    prettyPrint (NonDetInput pat (Just exp)) =
        char '$' <> prettyPrint pat <+> colon <+> prettyPrint exp

ppTie :: (PrettyPrintable id, PrettyPrintable (p id)) => 
    (AnExp id p, AnExp id p) -> Doc
ppTie (l, r) = prettyPrint l <+> text "<->" <+> prettyPrint r

ppRename :: (PrettyPrintable id, PrettyPrintable (p id)) => 
    (AnExp id p, AnExp id p) -> Doc
ppRename (l, r) = prettyPrint l <+> text "<-" <+> prettyPrint r

ppRepOp :: (PrettyPrintable id, PrettyPrintable (p id)) =>
    Doc -> [AnStmt id p] -> Doc -> Doc
ppRepOp op stmts exp =
    hang op tabWidth (
        hang (list (map prettyPrint stmts) <+> char '@')
            tabWidth exp)
