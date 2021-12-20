module Turing  
   --  (
   --      TuringMachine,
   --      Instruction,
   --      runMachine,
   --      State
   -- )
where

-- This type represents a state in the machine
data State = Halt | StartState | A | B | C deriving (Eq, Show)

-- This type repsents a symbol on the tape
data Symbol = Start | Zero | One | Blank deriving (Eq, Show)

-- This type repsents the tape
type Tape = [Symbol]

-- This represnets moving the tape
data PositionShift = Backwards | Forwards | Stay deriving (Show)

-- This type represnets an instruction on the turing machine
data Instruction = Instruction { 
    stateToMatch :: State, 
    symbolToMatch :: Symbol, 
    newState :: State, 
    newSymbol :: Symbol, 
    positionShift :: PositionShift 
} deriving (Show)

-- This presents the turing machine itself
data TuringMachine = TuringMachine { 
    states :: [State], 
    currentState :: State, 
    tape :: Tape, 
    currentPosition :: Int, 
    instructions :: [Instruction] 
} deriving (Show)


-- This takes a machiene and list of insturctions and recursively looks for an instruction to apply
findInstructionToApply :: TuringMachine -> [Instruction] -> Maybe Instruction
findInstructionToApply machine instructions
    | null instructions = Nothing
    | shouldApplyInstruction machine (instructions !! 0) =  Just (instructions !! 0)
    | otherwise = findInstructionToApply machine (tail instructions)

-- Gets the current symbol on the tape of the machine
currentSymbol :: TuringMachine -> Symbol
currentSymbol machine = (tape machine) !! (currentPosition machine)

-- This evalutes a machiene and determiens if we shoudl apply a particular instruction
shouldApplyInstruction :: TuringMachine -> Instruction -> Bool
shouldApplyInstruction machine instruction = 
    (currentState machine == stateToMatch instruction) && (currentSymbol machine == symbolToMatch instruction)

-- This takes a position and a shift and returns a new position
getNewPosition :: TuringMachine -> PositionShift -> Int
getNewPosition machine Forwards = (currentPosition machine) + 1
getNewPosition machine Stay = (currentPosition machine)
getNewPosition machine Backwards 
    | (currentPosition machine) == 0 = 0
    | otherwise = (currentPosition machine) - 1


-- Given a tape, a position in the tape to update and a symbol it creates a new tape
newTape :: Tape -> Int -> Symbol -> Tape
newTape tape position newSymbol =
    take (position) tape ++ newSymbol : drop (position+1) tape

-- Takes a machine and an instruction and updates it accordingly
applyInstruction :: TuringMachine -> Instruction -> TuringMachine
applyInstruction machine instruction =
    TuringMachine {
        states =  states machine, 
        currentState = newState instruction, 
        tape = newTape (tape machine) (currentPosition machine) (newSymbol instruction), 
        currentPosition = getNewPosition (machine) (positionShift instruction),
        instructions = instructions machine
    }

-- Halts the machine
haltMachine :: TuringMachine -> TuringMachine
haltMachine machine =
    TuringMachine {
        states = states machine, 
        currentState = Halt, 
        tape = tape machine, 
        currentPosition = currentPosition machine,
        instructions = instructions machine
    }

-- Takes a machine, finds the instruction to apply and applies it
machineCycle :: TuringMachine -> TuringMachine
machineCycle machine = 
    case instructionToApply of 
        Just instruction -> applyInstruction machine instruction
        Nothing -> haltMachine machine
    where instructionToApply = findInstructionToApply machine (instructions machine) -- Find instruction to apply

-- Loops until machine is in halting state
runMachine :: TuringMachine -> TuringMachine
runMachine machine
    | (currentState machine) == Halt = machine
    | otherwise  = runMachine (machineCycle machine)
