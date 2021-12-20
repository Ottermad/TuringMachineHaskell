    module Main where

import Turing

main :: IO ()
main = do
    let myMachine = newTuringMachine [One, Zero, One] [
            Instruction{stateToMatch = StartState, symbolToMatch=Start, newState = A, newSymbol = Start, positionShift = Forwards},
            Instruction{stateToMatch = A, symbolToMatch=Zero, newState = A, newSymbol = Blank, positionShift = Forwards},
            Instruction{stateToMatch = A, symbolToMatch=One, newState = A, newSymbol = Blank, positionShift = Forwards},
            Instruction{stateToMatch = A, symbolToMatch=Blank, newState = B, newSymbol = Blank, positionShift = Backwards},
            Instruction{stateToMatch = B, symbolToMatch=Blank, newState = B, newSymbol = Blank, positionShift = Backwards},
            Instruction{stateToMatch = B, symbolToMatch=Start, newState = C, newSymbol = Start, positionShift = Forwards},
            Instruction{stateToMatch = C, symbolToMatch=Blank, newState = Halt, newSymbol = One, positionShift = Stay}
            ]
    print (take 10 (tape myMachine))
    let outputMachine = runMachine myMachine
    print (take 10 (tape outputMachine))

