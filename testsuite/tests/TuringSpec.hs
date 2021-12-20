module TuringSpec (spec) where

import Turing
import Test.Hspec

spec :: Spec
spec = do
    describe "Turing Machine" $ do
        it "always writes one" $ do
            tape (runMachine (newTuringMachine [One, Zero, One] [
                    Instruction{stateToMatch = StartState, symbolToMatch=Start, newState = A, newSymbol = Start, positionShift = Forwards},
                    Instruction{stateToMatch = A, symbolToMatch=Zero, newState = A, newSymbol = Blank, positionShift = Forwards},
                    Instruction{stateToMatch = A, symbolToMatch=One, newState = A, newSymbol = Blank, positionShift = Forwards},
                    Instruction{stateToMatch = A, symbolToMatch=Blank, newState = B, newSymbol = Blank, positionShift = Backwards},
                    Instruction{stateToMatch = B, symbolToMatch=Blank, newState = B, newSymbol = Blank, positionShift = Backwards},
                    Instruction{stateToMatch = B, symbolToMatch=Start, newState = C, newSymbol = Start, positionShift = Forwards},
                    Instruction{stateToMatch = C, symbolToMatch=Blank, newState = Halt, newSymbol = One, positionShift = Stay}
                ]
                )) !! 1 == One
