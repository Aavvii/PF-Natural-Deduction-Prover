module BackwardsRulesImplementation where

import Tokenization
import TokenInfoExtraction

import Data.Maybe

--NATURAL DEDUCTION RULES_________________________________________________________________________________________________

----LEFT DEPENDANT RULES____________________________________________________________________________________________________

------INTRODUCTION____________________________________________________________________________________________________________
        --nimic

------ELIMINATION____________________________________________________________________________________________________________

-- ++ implementarea pentru AND ELIMINATON 1
backwardsAndElimination1 :: [Token] -> Int -> [[Token]]
backwardsAndElimination1 l n = ( ( getLeftOfTurnstile l ) ++ (TLParen:[]) ++(getRightOfTurnstile l) ++ (TAnd:[]) ++ (getRightArgument l n) ++ (TRParen:[]) )  : [];

-- implementarea pentru AND ELIMINATON 2
backwardsAndElimination2 :: [Token] -> Int -> [[Token]]
backwardsAndElimination2 l n = ( ( getLeftOfTurnstile l ) ++ (TLParen:[]) ++ (getLeftArgument l n) ++ (TAnd:[]) ++ (getRightOfTurnstile l) ++ (TRParen:[]) ): [];

-- implementarea pentru IMPLICATION ELIMINATON
backwardsImplicationElimination :: [Token] -> Int -> [[Token]]
backwardsImplicationElimination l n = ( (getLeftOfTurnstile l ) ++ (TLParen:[]) ++ (getLeftArgument l n) ++ (TImplies:[]) ++ (getRightOfTurnstile l) ++ (TRParen:[]) ) : ( (getLeftOfTurnstile l) ++ (getLeftArgument l n) ) : []

-- implementarea pentru OR ELIMINATION
backwardsOrElimination :: [Token] -> Int -> [[Token]]
backwardsOrElimination l n = ( (getLeftOfTurnstile l) ++ [TLParen] ++ (getLeftArgument l n) ++ (TOr:[]) ++ (getRightArgument l n) ++ [TRParen] ) : 
 ( (if(checkIfElementExists l (getLeftArgument  l n)) then ( getLeftOfTurnstile l ) else ( (getLeftArgument  l n) ++ (TComma:[]) ++ (getLeftOfTurnstile l) ) ) ++ (getRightOfTurnstile l) ) : 
 ( (if(checkIfElementExists l (getRightArgument l n)) then ( getLeftOfTurnstile l ) else ( (getRightArgument l n) ++ (TComma:[]) ++ (getLeftOfTurnstile l) ) ) ++ (getRightOfTurnstile l) ) : []

-- implementarea pentru NOT ELIMINATION
backwardsNotElimination :: [Token] -> Int -> [[Token]]
backwardsNotElimination l n = ( (getLeftOfTurnstile l) ++ (getRightArgument l n) ) : ( (getLeftOfTurnstile l) ++ (TLParen:[])  ++ (TNot:[]) ++ (getRightArgument l n) ++ (TRParen:[]) ) : []

----RIGHT DEPENDANT RULES__________________________________________________________________________________________________

------INTRODUCTION___________________________________________________________________________________________________________

--implementarea pentru AND INTRODUCTION in backwards reasoning
backwardsAndIntroduction :: [Token] -> Int -> [[Token]]
backwardsAndIntroduction l n | ((getLeftArgument l n) == (getRightArgument l n) ) = (getLeftOfTurnstile l ++ (getRightArgument l n) ) : []
backwardsAndIntroduction l n = ( getLeftOfTurnstile l ++ (getLeftArgument l n) ) : ( getLeftOfTurnstile l ++ (getRightArgument l n) ) : []

--implementarea pentru IMPLICATION INTRODUCTION in backwards reasoning
backwardsImplicationIntroduction :: [Token] -> Int -> [[Token]]
backwardsImplicationIntroduction l n | checkIfElementExists l (getLeftArgument l n) = (getLeftOfTurnstile l ++ getRightArgument l n) : []
backwardsImplicationIntroduction l n | thereIsNothingBeforeTurnstile l = ( getLeftArgument l n    ++ getLeftOfTurnstile l ++ getRightArgument l n ) : []
backwardsImplicationIntroduction l n = ( getLeftArgument l n ++ (TComma:[]) ++ getLeftOfTurnstile l ++ getRightArgument l n ) : []

--implementarea pentru OR INTRODUCTION 1 in backwards reasoning
backwardsOrIntroduction1 :: [Token] -> Int -> [[Token]]
backwardsOrIntroduction1 l n = ( getLeftOfTurnstile l ++ getLeftArgument l n ) : []

--implementarea pentru OR INTRODUCTION 2 in backwards reasoning
backwardsOrIntroduction2 :: [Token] -> Int -> [[Token]]
backwardsOrIntroduction2 l n = ( getLeftOfTurnstile l ++ getRightArgument l n ) : []

--implementarea pentru NOT INTRODUCTION in backwards reasoning
backwardsNotIntroduction :: [Token] -> Int -> [[Token]]
backwardsNotIntroduction l n | thereIsNothingBeforeTurnstile l = ( getRightArgument l n ++ getLeftOfTurnstile l ++ (TBottom:[]) ) : []
backwardsNotIntroduction l n | checkIfElementExists l (getRightArgument l n) = ( getLeftOfTurnstile l ++ (TBottom:[]) ) : []
backwardsNotIntroduction l n = ( getRightArgument l n ++ (TComma:[]) ++ getLeftOfTurnstile l ++ (TBottom:[]) ) : []

------ELIMINATION____________________________________________________________________________________________________________

--implementarea pentru BOTTOM ELIMINATION in backwards reasoning
backwardsBottomElimination :: [Token] -> Int -> [[Token]]
backwardsBottomElimination l _ = ( getLeftOfTurnstile l ++ (TBottom:[]) ) : []

--implementarea pentru NOT NOT ELIMINATION in backwards reasoning
backwardsNotNotElimination :: [Token] -> Int -> [[Token]]
backwardsNotNotElimination l n | isJust(getIndexOfElementAfterTurnistle l) = ( getLeftOfTurnstile l ++ (TLParen:[]) ++ (TNot:[]) ++ (TLParen:[]) ++ (TNot:[]) ++ (getRightArgument l (fromJust(getIndexOfElementAfterTurnistle l))) ++ (TRParen:[]) ++ (TRParen:[]) ) : []
backwardsNotNotElimination _ _ = []

--_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
-- IPOTEZA__________________________________________________________________________________________________________________
applyBackwardsAssumption :: [Token] -> Bool
applyBackwardsAssumption t = checkIfElementExists t (getRightOfTurnstile t)