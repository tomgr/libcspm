module CSPM.Operators.CSP.Syntax (
    CSPProcess(..),
    -- ** Fields
    -- | Fields occur within prefix statements. For example, if the prefix
    -- was @c$x?y!z@ then there would be three fields, of type 'NonDetInput',
    -- 'Input' and 'Output' respectively.
    CSPField(..), AnField,
    CSPExp(..),
    PCSPExp, PField,
) where

import CSPM.Compiler.Processes
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Operators.CSP.Processes
import Util.Annotated

type AnField id = Annotated () (CSPField id)
type CSPExp id = AnExp id CSPProcess
type CSPStmt id = AnStmt id CSPProcess

type PCSPExp = CSPExp UnRenamedName
type PField = AnField UnRenamedName

data CSPProcess id = 
    -- | Alphabetised parallel, e.g. @P [A || B] Q@.
    AlphaParallel {
        -- | Process 1.
        alphaParLeftProcess :: CSPExp id,
        -- | Alphabet of process 1.
        alphaParAlphabetLeftProcess :: CSPExp id,
        -- | Alphabet of process 2.
        alphaParAlphabetRightProcess :: CSPExp id,
        -- | Process 2.
        alphaParRightProcess :: CSPExp id
    }
    -- | Exception operator, e.g. @P [| A |> Q@.
    | Exception {
        exceptionLeftProcess :: CSPExp id,
        exceptionAlphabet :: CSPExp id,
        exceptionRightProcess :: CSPExp id
    }
    -- | External choice, e.g. @P [] Q@.
    | ExternalChoice {
        extChoiceLeftProcess :: CSPExp id,
        extChoiceRightOperator :: CSPExp id
    }
    -- | Generalised parallel, e.g. @P [| A |] Q@.
    | GenParallel {
        genParallelLeftProcess :: CSPExp id,
        genParallelAlphabet :: CSPExp id,
        genParallelRightProcess :: CSPExp id
    }
    -- | Guarded expressions, e.g. @b & P@ where @b@ is a boolean expression.
    -- This is equivalent to @if b then P else STOP@.
    | GuardedExp {
        guardedExpCondition :: CSPExp id,
        guardedExpProcess :: CSPExp id
    }
    -- | Hiding of events, e.g. @P \ A@.
    | Hiding {
        -- | The process the hiding is applied to.
        hidingProcess :: CSPExp id,
        -- | The set of events to be hidden.
        hidingAlphabet :: CSPExp id
    }
    -- | Internal choice, e.g. @P |~| Q@.
    | InternalChoice {
        intChoiceLeftProcess :: CSPExp id,
        intChoiceRightProcess :: CSPExp id
    }
    -- | Interrupt (where the left process is turned off once the right process
    -- performs an event), e.g. @P /\ Q@.
    | Interrupt {
        interruptLeftProcess :: CSPExp id,
        interruptRightProcess :: CSPExp id
    }
    -- | Interleaving of processes, e.g. @P ||| Q@.
    | Interleave {
        interleaveLeftProcess :: CSPExp id,
        interleaveRightProcess :: CSPExp id
    }
    -- Linked parallel, e.g. @P [a.x <- b.x | x <- X] Q@.
    | LinkParallel {
        linkParLeftProcess :: CSPExp id,
        linkParTiedEvents :: [(CSPExp id, CSPExp id)],
        linkParTieStatements :: [CSPStmt id],
        linkParRightProcess :: CSPExp id
    }
    -- | Event prefixing, e.g. @c$x?y!z -> P@.
    | Prefix {
        prefixChannel :: CSPExp id,
        prefixFields :: [AnField id],
        prefixProcess :: CSPExp id
    }
    -- | Event renaming, e.g. @P [[ a.x <- b.x | x <- X ]]@.
    | Rename {
        -- | The process that is renamed.
        renameProcess :: CSPExp id,
        -- | The events that are renamed, in the format of @(old, new)@.
        renameTiedEvents :: [(CSPExp id, CSPExp id)],
        -- | The statements for the ties.
        renameTieStatements :: [CSPStmt id]
    }
    -- | Sequential composition, e.g. @P; Q@.
    | SequentialComp {
        seqCompLeftProcess :: CSPExp id,
        seqCompRightProcess :: CSPExp id
    }
    -- | Sliding choice, e.g. @P |> Q@.
    | SlidingChoice {
        slidingChoiceLeftProcess :: CSPExp id,
        slidingChoiceRightProcess :: CSPExp id
    }

    -- Replicated Operators
    -- | Replicated alphabetised parallel, e.g. @|| x : X \@ [| A(x) |] P(x)@.
    | ReplicatedAlphaParallel {
        repAlphaParReplicatedStatements :: [CSPStmt id],
        repAlphaParAlphabet :: CSPExp id,
        repAlphaParProcess :: CSPExp id
    }
    -- | Replicated external choice, e.g. @[] x : X \@ P(x)@.
    | ReplicatedExternalChoice {
        repExtChoiceReplicatedStatements :: [CSPStmt id],
        repExtChoiceProcess :: CSPExp id
    }
    -- | Replicated interleave, e.g. @||| x : X \@ P(x)@.
    | ReplicatedInterleave {
        repInterleaveReplicatedStatements :: [CSPStmt id],
        repInterleaveProcess :: CSPExp id
    }
    -- | Replicated internal choice, e.g. @|~| x : X \@ P(x)@.
    | ReplicatedInternalChoice {
        repIntChoiceReplicatedStatements :: [CSPStmt id],
        repIntChoiceProcess :: CSPExp id
    }
    -- | Replicated link parallel, e.g. 
    -- @[a.x <- b.x | x <- X(y)] y : Y \@ P(y)@.
    | ReplicatedLinkParallel {
        -- | The tied events.
        repLinkParTiedChannels :: [(CSPExp id, CSPExp id)],
        -- | The statements for the ties.
        repLinkParTieStatements :: [CSPStmt id],
        -- | The 'Stmt's - the process (and ties) are evaluated once for each 
        -- value generated by these.
        repLinkParReplicatedStatements :: [CSPStmt id],
        -- | The process
        repLinkParProcess :: CSPExp id
    }
    -- | Replicated parallel, e.g. @[| A |] x : X \@ P(x)@.
    | ReplicatedParallel {
        repParAlphabet :: CSPExp id,
        repParReplicatedStatements :: [CSPStmt id],
        repParProcess :: CSPExp id
    }
        
    deriving (Eq, Show)

data CSPField id = 
    -- | @!x@
    Output (CSPExp id)
    -- | @?x:A@
    | Input (AnPat id) (Maybe (CSPExp id))
    -- | @$x:A@ (see P395 UCS)
    | NonDetInput (AnPat id) (Maybe (CSPExp id))
    deriving (Eq, Show)
