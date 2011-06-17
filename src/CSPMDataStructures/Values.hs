data Value = 
	VInt Integer
	| VBool Boolean
	| VEvent [Value]
	| VTuple [Value]
	| VDot Value Value
	| VList [Value]
	| VSet ValueSet
	| VFunction ([Value] -> M Value)
	| VProc Operator [Value]
	deriving (Eq, Show)
		
data ValueSet = 
	Integers
	| Booleans
	| Processes
	| Events
	
	| ExplicitSet [Value]
	| InfSet Int -- {lb..}
	deriving (Eq, Show)

data Transition = 
	Event Identifier 
data State = 
	State Identifier [Transition]
newtype LTS = LTS ([State])
