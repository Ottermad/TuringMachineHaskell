module Main where

import Turing

main :: IO ()
main = do
    let myMachine = TuringMachine{
        states = [Halt, StartState, A, B, C],
        currentState = StartState,
        tape = [Start, One, Zero, One, Blank], -- Can I use an infinite list of blanks here?
        currentPosition = 0,
        instructions = [
            Instruction{stateToMatch = StartState, symbolToMatch=Start, newState = A, newSymbol = Start, positionShift = Forwards},
            Instruction{stateToMatch = A, symbolToMatch=Zero, newState = A, newSymbol = Blank, positionShift = Forwards},
            Instruction{stateToMatch = A, symbolToMatch=One, newState = A, newSymbol = Blank, positionShift = Forwards},
            Instruction{stateToMatch = A, symbolToMatch=Blank, newState = B, newSymbol = Blank, positionShift = Backwards},
            Instruction{stateToMatch = B, symbolToMatch=Blank, newState = B, newSymbol = Blank, positionShift = Backwards},
            Instruction{stateToMatch = B, symbolToMatch=Start, newState = C, newSymbol = Start, positionShift = Forwards},
            Instruction{stateToMatch = C, symbolToMatch=Blank, newState = Halt, newSymbol = One, positionShift = Stay}
        ]
    }
    print (tape myMachine)
    let outputMachine = runMachine myMachine
    print (tape outputMachine)

