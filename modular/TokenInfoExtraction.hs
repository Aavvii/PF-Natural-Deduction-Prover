module TokenInfoExtraction where

import Tokenization


isThereATurnistle :: String -> Bool
isThereATurnistle [] = False
isThereATurnistle ('|' : '-' : tl) = True
isThereATurnistle (_:tl) = isThereATurnistle tl

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

conclussionIsBottom :: [Token] -> Bool
conclussionIsBottom t | ( (getRightOfTurnstile t) == ([TBottom])) = True
conclussionIsBottom _ = False

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