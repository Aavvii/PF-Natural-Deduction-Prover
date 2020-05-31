-- {-# OPTIONS_GHC -Wall #-}

import Data.Char
--import System.Environment
import Data.List
import Data.Tuple()
import Data.Maybe


data Formula = Var String
 | And Formula Formula
 | Or Formula Formula
 | Implies Formula Formula
 | Not Formula
 deriving Eq

data Token = TVar String | TLParen | TRParen | TAnd | TOr | TNot | TImplies | TTurnstile | TComma | TBottom | TRule String | TNr Int deriving (Eq, Show);

instance Ord Token where
 compare (TNr a) (TNr b) | a < b = LT
 compare (TNr a) (TNr b) | a > b = GT
 compare _ _ = EQ 

fstOfThree :: (a, b, c) -> a
fstOfThree (t,_,_) = t

sndOfThree :: (a, b, c) -> b
sndOfThree (_,t,_) = t

trdOfThree :: (a, b, c) -> c
trdOfThree (_,_,t) = t

{-
instance Show Formula where
 show (Var x) = x;
 show (Not f) = "!" ++ (show f);
 show (And f1 f2) = "(" ++ (show f1) ++ " & " ++ (show f2) ++ ")";
 show (Or f1 f2) = "(" ++ (show f1) ++ " | " ++ (show f2) ++ ")";
 show (Implies f1 f2) = "(" ++ (show f1) ++ " -> " ++ (show f2) ++ ")";
-}

-- De ce ++ si nu : ??
showTokens :: [Token] -> String
showTokens [] = []
showTokens ((TNr x) : tl) = show x ++ ". "++ (showTokens tl)
showTokens ((TRule x) : tl) = "    " ++ x ++ " " ++ (showTokens tl)
showTokens ((TVar x) : tl) = x ++ (showTokens tl)
showTokens (TLParen : tl) = "(" ++ showTokens tl
showTokens (TRParen : tl) = ")" ++ showTokens tl
showTokens (TAnd : tl) = " & " ++ showTokens tl
showTokens (TOr : tl) = " | " ++ showTokens tl
showTokens (TNot : tl) = "!" ++ showTokens tl
showTokens (TImplies : tl) = " -> " ++ showTokens tl
showTokens (TTurnstile : tl) = " |- " ++ showTokens tl
showTokens (TComma : tl) = " , " ++ showTokens tl
showTokens (TBottom : tl) = "_|_" ++ showTokens tl

showTokensArray :: [[Token]] -> String
showTokensArray [] = []
showTokensArray (t : tl) = showTokens t ++ "\n" ++ showTokensArray tl

showMaybeTokensArray :: Maybe [[Token]] -> String
showMaybeTokensArray Nothing = "No proof found."
showMaybeTokensArray (Just []) = []
showMaybeTokensArray (Just t) = showTokensArray  ( t)

showTokensAndRule :: [Token] -> String -> String
showTokensAndRule [] _ = []
showTokensAndRule t r = (showTokens t) ++ "   " ++ r ++ "\n"

stringArrayToString :: [String] -> String
stringArrayToString [] = []
stringArrayToString (h : tl) = h ++ stringArrayToString tl

{-
instance Show Token where
 show (TVar x) = x;
 show TLParen = "(";
 show TRParen = ")";
 show TAnd = "&";
 show TOr = "|";
 show TNot = "!";
 show TImplies = "->";
 show TTurnstile = "|-";
-}

canTokenize :: String -> Bool
canTokenize ('|' : '-' : tl) =       (canTokenize tl)
canTokenize ('(' : tl) =             (canTokenize tl)
canTokenize (')' : tl) =             (canTokenize tl)
canTokenize ('&' : tl) =             (canTokenize tl)
canTokenize ('|' : tl) =             (canTokenize tl) 
canTokenize ('!' : tl) =             (canTokenize tl)
canTokenize ('-' : '>' : tl) =       (canTokenize tl)
canTokenize (',' : tl) =             (canTokenize tl)
canTokenize ('_' : '|' : '_' : tl) = (canTokenize tl)
canTokenize (c : tl) | isAlpha c =   (canTokenize tl)
canTokenize (c : tl) | isSpace c =   (canTokenize tl)
canTokenize [] = True
canTokenize _ = False

isThereATurnistle :: String -> Bool
isThereATurnistle [] = False
isThereATurnistle ('|' : '-' : tl) = True
isThereATurnistle (_:tl) = isThereATurnistle tl

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('|' : '-' : tl) = TTurnstile  : (tokenize tl)
tokenize ('(' : tl) = TLParen : (tokenize tl)
tokenize (')' : tl) = TRParen : (tokenize tl)
tokenize ('&' : tl) = TAnd : (tokenize tl)
tokenize ('|' : tl) = TOr : (tokenize tl) 
tokenize ('!' : tl) = TNot : (tokenize tl)
tokenize ('-' : '>' : tl) = TImplies : (tokenize tl)
tokenize (',' : tl) = TComma : (tokenize tl)
tokenize ('_' : '|' : '_' : tl) = TBottom : (tokenize tl)
tokenize (c : tl) | isAlpha c = (TVar [c] : (tokenize tl))
--tokenize ((c :: Integer) : tl) = (TVar  : (tokenize tl))
tokenize (c : tl) | isSpace c = tokenize tl
tokenize _ = error "No tokenisation."

parcurge :: String -> String
parcurge (a : l) = toUpper a : parcurge l
parcurge [] = []

parcurgeInvers :: String -> String
parcurgeInvers (a : l) = parcurgeInvers l ++ [toLower a]
parcurgeInvers [] = []

--getTokensFromParanthesisToTheLeft :: [Token] -> [Token]
--getTokensFromParanthesisToTheLeft (TLParen : tl) = TLParen
--getTokensFromParanthesisToTheLeft (TRParen : tl) = getTokensFromParanthesisToTheLeft (TRParen : tl)

--Functie care ia partea de DINAINTEA elementului la indexul n, fara el
getLeftArgument :: [Token] -> Int -> [Token]
getLeftArgument xs n = reverse (getLeftArgument' (reverse (fst (splitAt n xs))) 0)

getLeftArgument' :: [Token] -> Int -> [Token]
getLeftArgument' [] _ = [];
getLeftArgument' (TVar x : _) 0 = TVar x : []
getLeftArgument' (TRParen : tl) depth = TRParen : getLeftArgument' tl (depth+1)
getLeftArgument' (TLParen : _) 1 = TLParen : []
getLeftArgument' (TLParen : tl) depth = TLParen : getLeftArgument' tl (depth-1)
getLeftArgument' (t : tl) depth = t : getLeftArgument' tl depth

--Functie care ia partea de DUPA elementul la indexul n, fara el
getRightArgument :: [Token] -> Int -> [Token]
getRightArgument xs n = getRightArgument' (snd (splitAt (n+1) xs)) 0

getRightArgument' :: [Token] -> Int -> [Token]
getRightArgument' [] _ = [];
getRightArgument' (TVar x : _) 0 = TVar x : []
getRightArgument' (TLParen : tl) depth = TLParen : getRightArgument' tl (depth+1)
getRightArgument' (TRParen : _) 1 = TRParen : []
getRightArgument' (TRParen : tl) depth = TRParen : getRightArgument' tl (depth-1)
getRightArgument' (t : tl) depth = t : getRightArgument' tl depth


getThisElementToTheRight :: [Token] -> Int -> [Token]
getThisElementToTheRight xs n = getRightArgument' (snd (splitAt n xs)) 0

getLeftOfTurnstile :: [Token] -> [Token]
getLeftOfTurnstile [] = []
getLeftOfTurnstile (TTurnstile : _) = TTurnstile : []
getLeftOfTurnstile (t : tl) = t : getLeftOfTurnstile tl

-- ++
getRightOfTurnstile :: [Token] -> [Token];
getRightOfTurnstile [] = []
getRightOfTurnstile (TTurnstile : tl) = tl;
getRightOfTurnstile ( _ : tl) = getRightOfTurnstile tl

thereIsNothingBeforeTurnstile :: [Token] -> Bool
thereIsNothingBeforeTurnstile l = if( (head l) == TTurnstile) then True else False

isBinaryOperation :: Token -> Bool
isBinaryOperation c | (c==TAnd) || (c==TOr) || (c==TNot) || (c==TImplies) = True
isBinaryOperation _ = False

getIndexOfElementAfterTurnistle :: [Token] -> Maybe Int
getIndexOfElementAfterTurnistle t = getIndexOfElementAfterTurnistle' t 0

getIndexOfElementAfterTurnistle' :: [Token] -> Int -> Maybe Int
getIndexOfElementAfterTurnistle' [] _ = Nothing
getIndexOfElementAfterTurnistle' (TTurnstile : h : tl) index = Just index
getIndexOfElementAfterTurnistle' (h:tl) index = getIndexOfElementAfterTurnistle' (tl) (index+1)

isThereOnlyOneOrNoNotAfterTurnistle :: [Token] -> Bool
isThereOnlyOneOrNoNotAfterTurnistle t = isThereOnlyOneOrNoNotAfterTurnistle' (getRightOfTurnstile t)

isThereOnlyOneOrNoNotAfterTurnistle' :: [Token] -> Bool
isThereOnlyOneOrNoNotAfterTurnistle' t | (countNots t)>1 = False
isThereOnlyOneOrNoNotAfterTurnistle' t = True

countNots :: [Token] -> Int
countNots (TNot:tl) = (countNots tl) + 1
countNots (TLParen:tl) = (countNots tl)
countNots _ = 0

couldBeAnd :: [Token] -> Int -> Bool
couldBeAnd t goal = couldBeAnd' t goal 0 0

couldBeAnd' :: [Token] -> Int -> Int -> Int -> Bool
couldBeAnd' (h:tl) goal index nots | (goal == index) = if ( ( (h==TAnd) && (nots `mod` 2 == 0)) || ( (h==TOr) && (nots `mod` 2 == 1)) ) then True else False 
couldBeAnd' (h:tl) goal index nots | (h == TNot) = couldBeAnd' tl goal (index+1) (nots+1)
couldBeAnd' (h:tl) goal index nots | (h == TRParen) = couldBeAnd' tl goal (index+1) (nots-1)
couldBeAnd' (h:tl) goal index nots | (h == TComma) = couldBeAnd' tl goal (index+1) 0
couldBeAnd' (h:tl) goal index nots = couldBeAnd' tl goal (index+1) nots
couldBeAnd' _ _ _ _ = False

couldBeOr :: [Token] -> Int -> Bool
couldBeOr t goal = couldBeOr' t goal 0 0

couldBeOr' :: [Token] -> Int -> Int -> Int -> Bool
couldBeOr' (h:tl) goal index nots | (goal == index) = if ( ( (h==TOr) && (nots `mod` 2 == 0)) || ( (h==TAnd) && (nots `mod` 2 == 1)) ) then True else False 
couldBeOr' (h:tl) goal index nots | (h == TNot) = couldBeOr' tl goal (index+1) (nots+1)
couldBeOr' (h:tl) goal index nots | (h == TComma) = couldBeOr' tl goal (index+1) 0
couldBeOr' (h:tl) goal index nots = couldBeOr' tl goal (index+1) nots
couldBeOr' _ _ _ _ = False

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


--_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
-- IPOTEZA__________________________________________________________________________________________________________________
applyBackwardsAssumption :: [Token] -> Bool
applyBackwardsAssumption t = checkIfElementExists t (getRightOfTurnstile t)

checkIfElementExists :: [Token] -> [Token] -> Bool
checkIfElementExists left right = (checkAssumptionHead left right) || (checkAssumptionTail left right)

checkAssumptionHead :: [Token] -> [Token] -> Bool
checkAssumptionHead left right | ((getThisElementToTheRight left 0) == right) = True;
checkAssumptionHead _ _ = False;

checkAssumptionTail :: [Token] -> [Token] -> Bool
checkAssumptionTail [] _ = False
checkAssumptionTail (TComma : tl) right | ((getThisElementToTheRight tl 0) == right) = True;
checkAssumptionTail (TTurnstile : _) _ = False;
checkAssumptionTail (_ : tl) right = checkAssumptionTail tl right

--Aditional functions for aplications________________________________________________________________________________________
countLeftSideTokens :: [Token] -> Int
countLeftSideTokens t = countLeftSideTokens' t 1

countLeftSideTokens' :: [Token] -> Int -> Int
countLeftSideTokens' [] n = n
countLeftSideTokens' (TTurnstile : _) n = n
countLeftSideTokens' (_ : tl) n = countLeftSideTokens' tl (n+1)

findRightSideAplicationPoint :: [Token] -> Token -> Maybe Int
findRightSideAplicationPoint list goal = findRightSideAplicationPoint' (getRightOfTurnstile list) goal (countLeftSideTokens list) 0

findRightSideAplicationPoint' :: [Token] -> Token -> Int -> Int -> Maybe Int
findRightSideAplicationPoint' [] _ _ _ = Nothing
findRightSideAplicationPoint' (TLParen :tl) goal position depth = findRightSideAplicationPoint' tl goal (position + 1) (depth + 1)
findRightSideAplicationPoint' (TRParen :tl) goal position depth = findRightSideAplicationPoint' tl goal (position + 1) (depth - 1)
findRightSideAplicationPoint' (h : _ ) goal position 1 | (h == goal) = Just (position)
findRightSideAplicationPoint' (_ : tl) goal position depth = findRightSideAplicationPoint' tl goal (position + 1) depth

-- APLICATIONS____________________________________________________________________________________________________________

----RIGHT DEPENDANT RULES__________________________________________________________________________________________________
applyBackwardsAndIntroduction :: [[Token]] -> [Token] -> Maybe [[Token]]
applyBackwardsAndIntroduction _ t | ( isJust (findRightSideAplicationPoint t TAnd)) = Just (  (backwardsAndIntroduction t (fromJust (findRightSideAplicationPoint t TAnd))) )
applyBackwardsAndIntroduction _ _ = Nothing

applyBackwardsImplicationIntroduction :: [[Token]] -> [Token] -> Maybe [[Token]]
applyBackwardsImplicationIntroduction _ t | ( isJust (findRightSideAplicationPoint t TImplies)) = Just (  (backwardsImplicationIntroduction t (fromJust (findRightSideAplicationPoint t TImplies)) ) )
applyBackwardsImplicationIntroduction _ _ = Nothing

applyBackwardsOrIntroduction1 :: [[Token]] -> [Token] -> Maybe [[Token]]
applyBackwardsOrIntroduction1 _ t | ( isJust (findRightSideAplicationPoint t TOr)) = Just (  (backwardsOrIntroduction1 t (fromJust (findRightSideAplicationPoint t TOr))) )
applyBackwardsOrIntroduction1 _ _ = Nothing

applyBackwardsOrIntroduction2 :: [[Token]] -> [Token] -> Maybe [[Token]]
applyBackwardsOrIntroduction2 _ t | ( isJust (findRightSideAplicationPoint t TOr)) = Just (  (backwardsOrIntroduction2 t (fromJust (findRightSideAplicationPoint t TOr))) )
applyBackwardsOrIntroduction2 _ _ = Nothing

applyBackwardsNotIntroduction :: [[Token]] -> [Token] -> Maybe [[Token]]
applyBackwardsNotIntroduction history t | (( hasloop history ) == False ) && ( isJust (findRightSideAplicationPoint t TNot)) = Just (  (backwardsNotIntroduction t (fromJust (findRightSideAplicationPoint t TNot))) )
applyBackwardsNotIntroduction _ _ = Nothing

---Functie ajutatoare pentru leadsToSolution
notIntroductionThenNotElimination :: [[Token]] ->[[Token]] -> Bool
--notIntroductionThenNotElimination _ _ = False
--notIntroductionThenNotElimination history (h:tl) | (isJust(applyNotNotElimination history h)) && hasSolution (history ++ (fromJust(applyNotNotElimination history h))) ((fromJust(applyNotNotElimination history h))++tl) = False
notIntroductionThenNotElimination history (h:tl) | ((wasBottomAlreadyAttemptedForThisPremise history h)==False) && (( conclussionIsBottom (h) ) == False) && (isJust(applyBackwardsNotIntroduction history h)) && (fromJust(applyBackwardsNotIntroduction history h) /= []) =
 leadsToSolution (history ++ (fromJust(applyBackwardsNotIntroduction history h))) ((fromJust(applyBackwardsNotIntroduction history h))++tl) (applyNotElimination)
notIntroductionThenNotElimination _ _ = False
-------------------------------------------------------------------

applyBackwardsBottomElimination :: [[Token]] -> [Token] -> Maybe [[Token]]
applyBackwardsBottomElimination history t  | (( conclussionIsBottom t ) == False) = Just ( getWhatIsNotInHistory history (backwardsBottomElimination t 0 ))
applyBackwardsBottomElimination _ _ = Nothing

---Functie ajutatoare pentru leadsToSolution
bottomEliminationThenNotElimination :: [[Token]] ->[[Token]] -> Bool
--bottomEliminationThenNotElimination _ _ = False
bottomEliminationThenNotElimination history (h:tl) | ((wasBottomAlreadyAttemptedForThisPremise history h)==False) && (( conclussionIsBottom (h) ) == False) && (isJust(applyBackwardsBottomElimination history h)) && (fromJust(applyBackwardsBottomElimination history h) /= []) =
 leadsToSolution (history ++ (fromJust(applyBackwardsBottomElimination history h))) ((fromJust(applyBackwardsBottomElimination history h))++tl) (applyNotElimination)
bottomEliminationThenNotElimination _ _ = False

applyNotNotElimination :: [[Token]] -> [Token] -> Maybe [[Token]]
applyNotNotElimination _ t | (( conclussionIsBottom t ) == False) && isJust(getIndexOfElementAfterTurnistle t) && (isThereOnlyOneOrNoNotAfterTurnistle t) = Just (  (backwardsNotNotElimination t 0))
applyNotNotElimination _ _ = Nothing


----LEFT DEPENDANT RULES__________________________________________________________________________________________________
--Functii ajutatoare-----------------
isStillSane :: [Token] -> Bool
--                                              As putea sa pun aici un multiplicator, dar are rost?
--                                                |
--                                                v                                                 Nu.
isStillSane t | (( length (getLeftOfTurnstile t) ) ) > ( length (getRightOfTurnstile t) ) = True
isStillSane _ = False

----------------------------------------------- <<<<<<<<<< NU UITA CA AI STRICAT getWhatIsNotInHistory fiindca parea intuila
--                                                          Motive de a renunta la ea:
--                                                                             -programul aplica not elimination si crede ca e ok
--                                                                             -ma incurca la verificarea de final
getWhatIsNotInHistory :: [[Token]] -> [[Token]] -> [[Token]]
getWhatIsNotInHistory _ t = t 
 {-
getWhatIsNotInHistory _ [] = []
getWhatIsNotInHistory history (h:tl) | ((hasConclussionBeenAlreadyFound history h)==False) || (applyBackwardsAssumption h) = h : (getWhatIsNotInHistory history tl)
getWhatIsNotInHistory history (_:tl) = getWhatIsNotInHistory history tl
-- -}

--AND ELIMINATION 1-------------------------------------------------
applyAndElimination1 :: [[Token]] -> [Token] -> Maybe [[Token]]
applyAndElimination1 history t | (isStillSane t) && (( conclussionIsBottom t ) == False) = applyAndElimination1' history t t 0
applyAndElimination1 _ _ = Nothing
--
applyAndElimination1' :: [[Token]] -> [Token] -> [Token] -> Int -> Maybe [[Token]]
applyAndElimination1' _ [] _ _ = Nothing
applyAndElimination1' _ (TTurnstile : _ ) _ _ = Nothing
applyAndElimination1' history (h : _ ) initialRule index | ( couldBeAnd initialRule index ) && ( (getRightOfTurnstile initialRule) == (getLeftArgument initialRule index) ) && ((getWhatIsNotInHistory history  (backwardsAndElimination1 initialRule index)) /= [] ) && (hasSolution (history ++ (backwardsAndElimination1 initialRule index)) (backwardsAndElimination1 initialRule index)) = Just ( getWhatIsNotInHistory history (backwardsAndElimination1 initialRule index) )
applyAndElimination1' history ( _ : tl) initialRule index = applyAndElimination1' history tl initialRule (index+1)
--AND ELIMINATION 2-------------------------------------------------
applyAndElimination2 :: [[Token]] -> [Token] -> Maybe [[Token]]
applyAndElimination2 history t | (isStillSane t) && (( conclussionIsBottom t ) == False) = applyAndElimination2' history t t 0
applyAndElimination2 _ _ = Nothing
--
applyAndElimination2' :: [[Token]] -> [Token] -> [Token] -> Int -> Maybe [[Token]]
applyAndElimination2' _ [] _ _ = Nothing
applyAndElimination2' _ (TTurnstile : _ ) _ _ = Nothing
applyAndElimination2' history (h : _ ) initialRule index | ( couldBeAnd initialRule index ) && ( (getRightOfTurnstile initialRule) == (getRightArgument initialRule index) ) && ((getWhatIsNotInHistory history  (backwardsAndElimination2 initialRule index)) /= [] ) && (hasSolution (history ++ (backwardsAndElimination2 initialRule index)) (backwardsAndElimination2 initialRule index)) = Just(  getWhatIsNotInHistory history (backwardsAndElimination2 initialRule index) )
applyAndElimination2' history ( _ : tl) initialRule index = applyAndElimination2' history tl initialRule (index+1)

--IMPLICATION ELIMINATION-------------------------------------------------------------------------------
applyImplicationElimination :: [[Token]] -> [Token] -> Maybe [[Token]]
applyImplicationElimination history t | (isStillSane t) && (( conclussionIsBottom t ) == False) = applyImplicationElimination' history t t 0
applyImplicationElimination _ _ = Nothing
--
applyImplicationElimination' :: [[Token]] -> [Token] -> [Token] -> Int -> Maybe [[Token]]
applyImplicationElimination' _ [] _ _ = Nothing
applyImplicationElimination' _ (TTurnstile : _ ) _ _ = Nothing
applyImplicationElimination' history ( h : _ ) initialRule index | (h==TImplies) && ((getWhatIsNotInHistory history  (backwardsImplicationElimination initialRule index)) /= [] ) && ( (getRightArgument initialRule index)==(getRightOfTurnstile initialRule) ) && (areAllResultsProvableWithHistory (history ++ ( getWhatIsNotInHistory history (backwardsImplicationElimination initialRule index))) ( getWhatIsNotInHistory history (backwardsImplicationElimination initialRule index)) ) = Just ( getWhatIsNotInHistory history (backwardsImplicationElimination initialRule index) )
applyImplicationElimination' history ( _ : tl) initialRule index = applyImplicationElimination' history tl initialRule (index+1)

--OR ELIMINATION------------------------------------------------------------------------------------------
applyOrElimination :: [[Token]] -> [Token] -> Maybe [[Token]]
applyOrElimination history t | (isStillSane t) && (( conclussionIsBottom t ) == False) = applyOrElimination' history t t 0
applyOrElimination _ _ = Nothing
--
applyOrElimination' :: [[Token]] -> [Token] -> [Token] -> Int -> Maybe [[Token]]
applyOrElimination' _ [] _ _ = Nothing
applyOrElimination' _ (TTurnstile : _ ) _ _ = Nothing
applyOrElimination' history ( h : _ ) initialRule index | (couldBeOr initialRule index) && ((getWhatIsNotInHistory history  (backwardsOrElimination initialRule index)) /= [] ) && (areAllResultsProvableWithHistory (history ++ ( getWhatIsNotInHistory history (backwardsOrElimination initialRule index))) ( getWhatIsNotInHistory history (backwardsOrElimination initialRule index)) ) = Just ( getWhatIsNotInHistory history (backwardsOrElimination initialRule index) )
applyOrElimination' history ( _ : tl) initialRule index = applyOrElimination' history tl initialRule (index+1)

--NOT ELIMINATION---------------------------------------------------------------------------------------------
applyNotElimination :: [[Token]] -> [Token] -> Maybe [[Token]]
applyNotElimination history t | (isStillSane t) && ( conclussionIsBottom t ) = applyNotElimination' history t t 0
applyNotElimination _ _ = Nothing
--
applyNotElimination' :: [[Token]] -> [Token] -> [Token] -> Int -> Maybe [[Token]]
applyNotElimination' _ [] _ _ = Nothing
applyNotElimination' _ (TTurnstile : _ ) _ _ = Nothing
applyNotElimination' history ( h : tl) initialRule index | (h==TNot) {- && ( (getRightOfTurnstile (h:tl) ) == ([TBottom]) )  &&  ( ((getWhatIsNotInHistory history  (backwardsNotElimination initialRule index)) /= [] ) ) && (areAllResultsProvableWithHistory (history ++ ( getWhatIsNotInHistory history (backwardsNotElimination initialRule index))) ( getWhatIsNotInHistory history (backwardsNotElimination initialRule index)) ) -} = Just ( getWhatIsNotInHistory history (backwardsNotElimination initialRule index) )
applyNotElimination' history ( _ : tl) initialRule index = applyNotElimination' history tl initialRule (index+1)


--FUNCTII AJUTATOARE PENTRU PROVE_________________________________________________________________________________________________

giveNumbers :: Maybe [[Token]] -> Token -> Maybe [[Token]]
giveNumbers Nothing _ = Nothing
giveNumbers (Just t) (TNr i) = Just (giveNumbers' t (i+1))
giveNumbers _ _ = Just []

giveNumbers' :: [[Token]] -> Int -> [[Token]]
giveNumbers' (h:tl) nr = ( [TNr nr] ++ h ) : ( giveNumbers' tl (nr+1) )
giveNumbers' [] _ = []

getNr :: Token -> Int
getNr (TNr i) = i
getNr _ = 0

{-
getMaxLineNr :: [[Token]] -> Token
getMaxLineNr t = TNr (getMaxLineNr' t 0)

getMaxLineNr' :: [[Token]] -> Int -> Int
getMaxLineNr' (h: tl) max = if ( (getNr (head h)) > max ) then getMaxLineNr' tl (getNr (head h)) else getMaxLineNr' tl max
getMaxLineNr' [] max = max
-}
getMaxLineNr :: [Token] -> Token
getMaxLineNr t = TNr (getMaxLineNr' t 0)

getMaxLineNr' :: [Token] -> Int -> Int
getMaxLineNr' (h: tl) max = if ( (getNr h) > max ) then getMaxLineNr' tl (getNr h) else getMaxLineNr' tl max
getMaxLineNr' [] max = max

getMaxNrAsInt :: [[Token]] -> Int
getMaxNrAsInt t = (getMaxLineNr' (getAllHeads t) 0)

getAllHeads :: [[Token]] -> [Token]
getAllHeads (h:tl) = (head h) : (getAllHeads tl)
getAllHeads [] = []

applyAllOnceInefficienty :: [[Token]] -> (Maybe [[Token]] , [Token] , [[Token]])
applyAllOnceInefficienty t = applyAllOnce t [(TNr 1:(head t))] (TNr 1)

applyAllOnce :: [[Token]] -> [[Token]] -> Token -> (Maybe [[Token]] , [Token] , [[Token]])
applyAllOnce history (h : _ ) number | applyBackwardsAssumption (tail h)                                               = (Just ([]), [(TRule("(Assumption)"))], (history ))
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyBackwardsAndIntroduction)         = ( giveNumbers (applyBackwardsAndIntroduction         history (tail h)) number ,         [TRule ("- And Introduction over")]          ++ [(TNr ((getNr number)+2))] ++ [(TNr ((getNr number)+1))],                              (history ++ (fromJust(applyBackwardsAndIntroduction         history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyBackwardsImplicationIntroduction) = ( giveNumbers (applyBackwardsImplicationIntroduction history (tail h)) number ,         [TRule ("- Implication Introduction over")]  ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyBackwardsImplicationIntroduction history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyBackwardsOrIntroduction1)         = ( giveNumbers (applyBackwardsOrIntroduction1         history (tail h)) number ,         [TRule ("- Or Introduction 1 over")]         ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyBackwardsOrIntroduction1         history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyBackwardsOrIntroduction2)         = ( giveNumbers (applyBackwardsOrIntroduction2         history (tail h)) number ,         [TRule ("- Or Introduction 2 over")]         ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyBackwardsOrIntroduction2         history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyAndElimination1)                  = ( giveNumbers (applyAndElimination1                  history (tail h)) number ,         [TRule ("- And Elimination 1 over")]         ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyAndElimination1                  history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyAndElimination2)                  = ( giveNumbers (applyAndElimination2                  history (tail h)) number ,         [TRule ("- And Elimination 2 over")]         ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyAndElimination2                  history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyImplicationElimination)           = ( giveNumbers (applyImplicationElimination           history (tail h)) number ,         [TRule ("- Implication Elimination over")]   ++ [(TNr ((getNr number)+2))] ++ [(TNr ((getNr number)+1))],                              (history ++ (fromJust(applyImplicationElimination           history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyOrElimination)                    = ( giveNumbers (applyOrElimination                    history (tail h)) number ,         [TRule ("- Or Elimination over")]            ++ [(TNr ((getNr number)+3))] ++ [(TNr ((getNr number)+2))] ++ [(TNr ((getNr number)+1))],(history ++ (fromJust(applyOrElimination                    history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyNotNotElimination)                = ( giveNumbers (applyNotNotElimination                history (tail h)) number ,         [TRule ("- Not Not Elimination over")]       ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyNotNotElimination                history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyNotElimination)                   = ( giveNumbers (applyNotElimination                   history (tail h)) number ,         [TRule ("- Not Elimination over")]           ++ [(TNr ((getNr number)+2))] ++ [(TNr ((getNr number)+1))],                              (history ++ (fromJust(applyNotElimination                   history (tail h) ))) )
--Cazuri speciale pentru bottom
applyAllOnce history (h : tl) number | (bottomEliminationThenNotElimination history ((tail h) : tl) )                  = ( giveNumbers (applyBackwardsBottomElimination       history (tail h)) number ,         [TRule ("- Bottom Elimination over")]        ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyBackwardsBottomElimination       history (tail h) ))) )
applyAllOnce history (h : tl) number | (notIntroductionThenNotElimination   history ((tail h) : tl) )                  = ( giveNumbers (applyBackwardsNotIntroduction         history (tail h)) number ,         [TRule ("- Not Introduction over")]          ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyBackwardsNotIntroduction         history (tail h) ))) )
--
--applyAllOnce history (h : tl) | (leadsToSolution history (h : tl) applyBackwardsNotIntroduction)         = (applyBackwardsNotIntroduction history h ,                TRule ("(Not Introduction)"),         (history ++ (fromJust(applyBackwardsNotIntroduction         history h))) )
--applyAllOnce history (h : tl) | (leadsToSolution history (h : tl) applyBackwardsBottomElimination)                   = (applyNotElimination history h ,              TRule ("(Not Elimination)"),          (history ++ (fromJust(applyNotElimination                   history h))) )
applyAllOnce _ _ _ = ( Nothing, [TRule "This message will never be shown anyway, so... How did the scarecrow do in his PhD? - He was outstanding in his field."], []:[])

areAllResultsProvable :: [[Token]] -> Bool
areAllResultsProvable [] = True
areAllResultsProvable (h : tl) = ( startHasSolution (h:tl)) && (areAllResultsProvable tl)

areAllResultsProvableWithHistory :: [[Token]] -> [[Token]] -> Bool
--areAllResultsProvableWithHistory history _ | hasloop history = False;
areAllResultsProvableWithHistory _ [] = True
areAllResultsProvableWithHistory history (h : tl) = ( hasSolution history [h]) && (areAllResultsProvableWithHistory history tl)


doesLineExist :: [[Token]] -> [Token] -> Bool
doesLineExist [] _ = False
doesLineExist (h : _ ) l | ((applyBackwardsAssumption h)==False) && (h == l) = True
doesLineExist (_ : tl) l = doesLineExist tl l

hasConclussionBeenAlreadyFound :: [[Token]] -> [Token] -> Bool
hasConclussionBeenAlreadyFound [] _ = False
hasConclussionBeenAlreadyFound (h : _ ) l | ((getRightOfTurnstile h) == (getRightOfTurnstile l)) && ( (applyBackwardsAssumption h)) = True
hasConclussionBeenAlreadyFound (_ : tl) l = hasConclussionBeenAlreadyFound tl l

wasBottomAlreadyAttemptedForThisPremise :: [[Token]] -> [Token] -> Bool
wasBottomAlreadyAttemptedForThisPremise [] _ = False
wasBottomAlreadyAttemptedForThisPremise (h:tl) t | ( (getLeftOfTurnstile h) == (getLeftOfTurnstile t) ) && (conclussionIsBottom h) = True
wasBottomAlreadyAttemptedForThisPremise (h:tl) t = wasBottomAlreadyAttemptedForThisPremise tl t

hasloop :: [[Token]] -> Bool
hasloop [] = False
hasloop ( h : tl) | (doesLineExist tl h) = True
hasloop ( _ : tl) = hasloop tl

conclussionIsBottom :: [Token] -> Bool
conclussionIsBottom t | ( (getRightOfTurnstile t) == ([TBottom])) = True
conclussionIsBottom _ = False

{-
leadsToSolution :: [[Token]] -> ([Token] -> Maybe [[Token]])-> Bool
leadsToSolution tokens rule = leadsToSolution' tokens tokens rule 
-}

leadsToSolution :: [[Token]] -> [[Token]] -> ( [[Token]] -> [Token] -> Maybe [[Token]])-> Bool
leadsToSolution _ [] _ = False
--leadsToSolution history _ _ | (length history) > 3 = False
--leadsToSolution history _ _  | (hasloop history) = False
leadsToSolution history (h : [] ) _ | (applyBackwardsAssumption h) = True
leadsToSolution history (h : tl ) _ | (applyBackwardsAssumption h) = True  && ( hasSolution history tl )
leadsToSolution history (h : tl) rule | (isJust(rule history h)) && (fromJust(rule history h) /= []) = -- && (areAllResultsProvableWithHistory history (fromJust(rule history h))) =
 leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyBackwardsAndIntroduction) ||
 leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyBackwardsImplicationIntroduction) ||
 leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyBackwardsOrIntroduction1) ||
 leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyBackwardsOrIntroduction2) ||
 leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyAndElimination1) ||
 leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyAndElimination2) ||
 leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyImplicationElimination) ||
 leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyOrElimination) ||
 leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyNotNotElimination) ||
 leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyNotElimination)  ||
-- leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyBackwardsNotIntroduction)  ||
-- Aici vor fi cazurile in care apare bottom
 bottomEliminationThenNotElimination (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) ||
 notIntroductionThenNotElimination   (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) -- ||
--
-- leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) ( applyBackwardsBottomElimination )  ||
-- leadsToSolution (history ++ (fromJust(rule history h))) ((fromJust(rule history h))++tl) (applyBackwardsNotIntroduction) -- ||
leadsToSolution _ _ _ = False;

hasSolution :: [[Token]] -> [[Token]] -> Bool
hasSolution history _ | (hasloop history) = False
hasSolution _ (h : _) | (applyBackwardsAssumption h) = True
hasSolution history t | (leadsToSolution history t applyBackwardsAndIntroduction) = True
hasSolution history t | (leadsToSolution history t applyBackwardsImplicationIntroduction) = True
hasSolution history t | (leadsToSolution history t applyBackwardsOrIntroduction1) = True
hasSolution history t | (leadsToSolution history t applyBackwardsOrIntroduction2) = True
--hasSolution history t | (leadsToSolution history t applyBackwardsNotIntroduction) = True
hasSolution history t | (leadsToSolution history t applyAndElimination1) = True
hasSolution history t | (leadsToSolution history t applyAndElimination2) = True
hasSolution history t | (leadsToSolution history t applyImplicationElimination) = True
hasSolution history t | (leadsToSolution history t applyOrElimination) = True
hasSolution history t | (leadsToSolution history t applyNotNotElimination) = True
hasSolution history t | (leadsToSolution history t applyNotElimination) = True
-- Cazuri speciale pentru bottom
hasSolution history t | (bottomEliminationThenNotElimination history t ) = True
hasSolution history t | (notIntroductionThenNotElimination history t ) = True
--
hasSolution _ _ = False

startHasSolution :: [[Token]] -> Bool
startHasSolution t = hasSolution t t


-- (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

{-
giveRowsNumebrs :: Maybe [[Token]] -> Maybe [[Token]]
giveRowsNumebrs Nothing = Nothing
giveRowsNumebrs t = Just (giveRowsNumebrs' (  reverse(fromJust t)) 1 )

giveRowsNumebrs' :: [[Token]] -> Int -> [[Token]]
giveRowsNumebrs' (h:tl) index = (( TNr index ) : h) : giveRowsNumebrs' tl (index+1)
giveRowsNumebrs' [] _ = []

checkAndIntroduction :: [Token] -> [Token] -> [Token] -> (Bool, [Token] )
checkAndIntroduction (nr1:pr1) (nr2:pr2) (cnr:conclusion) = if ( ( getLeftOfTurnstile pr1 == getLeftOfTurnstile pr2 ) && ( conclusion == ( (getLeftOfTurnstile pr1) ++ [TLParen] ++ (getRightOfTurnstile pr1) ++ [TAnd] ++ (getRightOfTurnstile pr2) ++ [TRParen]) ) ) then (True, [cnr] ++ conclusion ++ [TRule "And introduction"] ++ [nr1] ++ [nr2]) else (False,[])




forwardCheck :: Maybe [[Token]] -> Maybe [[Token]]
forwardCheck t | isNothing t = Nothing
forwardCheck t = Just (forwardCheck' (fromJust t) )

forwardCheck' :: [[Token]] -> [[Token]]
forwardCheck' (h:tl) | fst (checkLine h) = (snd(checkLine h)) : (forwardCheck' tl)
forwardCheck' -}

-- (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

inverseCountOnRow :: [Token] -> Int -> [Token]
inverseCountOnRow [] _ = []
inverseCountOnRow ( (TNr x) : tl ) max =  (TNr ( max -x ) ) : (inverseCountOnRow tl max )
inverseCountOnRow ( h : tl ) max =  ( h ) : (inverseCountOnRow tl max )

inverseCount :: [[Token]] -> Int -> [[Token]]
inverseCount [] _ = []
inverseCount (h:tl) max = (inverseCountOnRow h max) : (inverseCount tl max)

orderRules :: Maybe [[Token]] -> Maybe [[Token]]
orderRules Nothing = Nothing
orderRules (Just t) = Just (  sortOn (head) ( inverseCount t ( (getMaxNrAsInt t) + 1 ) )  )


-- ++ FUNCTIA DE APLICARE A REGULILOR-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

prove :: [Token] -> Maybe [[Token]]
prove [] = Just ([])
prove start  | startHasSolution (start:[])  = Just (prove' ( [start] ) ( [[TNr 1] ++ start] ) [TNr 1] )
prove _ = Nothing

prove' :: [[Token]] -> [[Token]] -> [Token] -> [[Token]]
prove' _       (h : []) _ | (applyBackwardsAssumption (tail h)) = (  h ++ [ (TRule "(Assumption)") ] ) : []
prove' history (h : tl) numbers | isJust (fstOfThree(applyAllOnce history ([h]) (getMaxLineNr (numbers)) )) = (h ++  ( sndOfThree (applyAllOnce history ([h]) (getMaxLineNr (numbers)) ))  ) : ( prove' ( trdOfThree (applyAllOnce history ([h]) (getMaxLineNr (numbers)) )) ((fromJust(fstOfThree( applyAllOnce history ([h]) (getMaxLineNr (numbers)) )) ) ++ tl) (numbers ++ (getAllHeads (h : fromJust (fstOfThree(applyAllOnce history ([h]) (getMaxLineNr (numbers)) )) )) ) )
prove' _ [] _ = []
-- Asta e un fallback. Daca nu intra in nici un caz de mai sus dar are solutie, resetez istoricul
--prove' _ (h : tl) numbers | startHasSolution [h] = (h ++  ( sndOfThree (applyAllOnce [] ([h]) (getMaxLineNr (numbers)) ))  ) : ( prove' ( trdOfThree (applyAllOnce [] ([h]) (getMaxLineNr (numbers)) )) ((fromJust(fstOfThree( applyAllOnce [] ([h]) (getMaxLineNr (numbers)) )) ) ++ tl) (numbers ++ (getAllHeads (h : fromJust (fstOfThree(applyAllOnce [] ([h]) (getMaxLineNr (numbers)) )) )) ) )
prove' history _ _ = [[(TRule "!!!- An Error Occured. Above is history")]] ++ history

main :: IO ()
main = do 
       putStrLn "\nScrieti o formula de logica propozitionala sau \"exit\" pentru a opri programul."
       inFormula <- getLine
       if ( inFormula /= "exit" ) then
        do
        if ( (canTokenize inFormula) && (isThereATurnistle inFormula) ) then
         do
         putStrLn ( "\n" ++ showMaybeTokensArray (   orderRules  ( prove (tokenize (inFormula))) )  )
         main
        else
         do
         putStrLn "Formula data nu este valida."
         main
       else putStrLn ""

