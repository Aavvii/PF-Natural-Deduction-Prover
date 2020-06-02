module ForwardFinalCheck where

import Tokenization
import TokenInfoExtraction

import Data.Maybe

--Functii ajutatoare pentru forward checking
getJustConclusion :: [Token] -> [Token]
getJustConclusion t = getJustConclusion' (tail t)

getJustConclusion' :: [Token] -> [Token]
getJustConclusion' ((TRule _):_) = [] 
getJustConclusion' (h:tl) = h : (getJustConclusion' tl)
getJustConclusion' _ = []

getJustConclusions :: [[Token]] -> [[Token]]
getJustConclusions (h:tl) = ( getJustConclusion h ) : ( getJustConclusions tl )
getJustConclusions _ = []

getJustRule :: [Token] -> String
getJustRule ((TRule x):_) = x
getJustRule (h:tl) = getJustRule tl
getJustRule _ = []

getJustLineNrs :: [Token] -> [Token]
getJustLineNrs ((TRule x):tl) = tl
getJustLineNrs ( h : tl ) = getJustLineNrs tl
getJustLineNrs _ = []

numberIsInList :: Token -> [Token] -> Bool
numberIsInList nr (h : _ ) | nr == h = True
numberIsInList nr (h : tl) = numberIsInList nr tl
numberIsInList nr [] = False

getLinesWithTheseNumbers :: [[Token]] -> [Token] -> [[Token]]
getLinesWithTheseNumbers [] numbers     = []
getLinesWithTheseNumbers (h:tl) numbers | ( numberIsInList (head h) numbers ) = h : ( getLinesWithTheseNumbers tl numbers )
getLinesWithTheseNumbers (h:tl) numbers = ( getLinesWithTheseNumbers tl numbers )

getPremise :: [[Token]] -> [Token] -> [[Token]]
getPremise all line =  getJustConclusions ( (getLinesWithTheseNumbers all (getJustLineNrs line) ) )

getIndexOfOperationAfterTurniste :: [Token] -> Token -> Int
getIndexOfOperationAfterTurniste [] goal = 0
getIndexOfOperationAfterTurniste (TTurnstile : tl) goal =  (getIndexOfOperationAfterTurniste' tl goal 1)
getIndexOfOperationAfterTurniste (h : tl) goal           = (getIndexOfOperationAfterTurniste tl goal)+1

getIndexOfOperationAfterTurniste' :: [Token] -> Token -> Int -> Int
getIndexOfOperationAfterTurniste' (h:_) goal index | h == goal = index
getIndexOfOperationAfterTurniste' (h : tl) goal index = (getIndexOfOperationAfterTurniste' tl) goal (index+1)
getIndexOfOperationAfterTurniste' _ _ _ = 0

whatRuleToApply :: String -> ( [Token] -> [[Token]] -> Bool )
whatRuleToApply "And Introduction"         = checkAndIntroduction
whatRuleToApply "And Elimination 1"        = checkAndElimination1
whatRuleToApply "And Elimination 2"        = checkAndElimination2
whatRuleToApply "Implication Elimination"  = checkImplicationElimination
whatRuleToApply "Implication Introduction" = checkImplicationIntroduction
whatRuleToApply "Or Introduction 1"        = checkOrIntroduction1
whatRuleToApply "Or Introduction 2"        = checkOrIntroduction2
whatRuleToApply "Or Elimination"           = checkOrElimination
whatRuleToApply "Not Elimination"          = checkNotElimination
whatRuleToApply "Not Introduction"         = checkNotIntroduction
whatRuleToApply "Bottom Elimination"       = checkBottomElimination
whatRuleToApply "Not Not Elimination"      = checkNotNotIntroduction
whatRuleToApply _ = (\x y -> False )

-----------------------------------------------

forwardCheck :: Maybe [[Token]] -> Maybe [[Token]]
--forwardCheck t = t
forwardCheck t | isNothing t = Nothing
forwardCheck t | isValidProof ( fromJust t ) ( fromJust t ) = t
forwardCheck _ = Nothing -- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< AICI

isValidProof :: [[Token]] -> [[Token]] -> Bool
isValidProof [] _ = True
isValidProof (h:tl) all | ( isValidLine h all ) = isValidProof tl all
isValidProof _ _ = False

isValidLine :: [Token] -> [[Token]] -> Bool
isValidLine line _ | (getJustRule line) == "Assumption" = True
isValidLine line all = (whatRuleToApply (getJustRule line) ) (getJustConclusion line) (getPremise all line)

--Aici am functiile de verificare forward pentru fiecare regula

checkAndIntroduction :: [Token] -> [[Token]] -> Bool
checkAndIntroduction conc (p2:p1:[]) = if ( ( (getLeftOfTurnstile p1) == (getLeftOfTurnstile p2) )  && ( conc == ( (getLeftOfTurnstile p1) ++ [TLParen] ++ (getRightOfTurnstile p1) ++ [TAnd] ++ ( getRightOfTurnstile p2 ) ++ [TRParen] ) )  ) then True else False
checkAndIntroduction _ _ = False

checkAndElimination1 :: [Token] -> [[Token]] -> Bool
checkAndElimination1 conc (p:[]) | (isJust ( findRightSideAplicationPoint p TAnd) )= if ( conc == ( (getLeftOfTurnstile p) ++ ( getLeftArgument p (fromJust ( findRightSideAplicationPoint p TAnd) ) ) ) ) then True else False
checkAndElimination1 _ _ = False

checkAndElimination2 :: [Token] -> [[Token]] -> Bool
checkAndElimination2 conc (p:[]) | (isJust ( findRightSideAplicationPoint p TAnd) )= if ( conc == ( (getLeftOfTurnstile p) ++ ( getRightArgument p (fromJust ( findRightSideAplicationPoint p TAnd) ) ) ) ) then True else False
checkAndElimination2 _ _ = False

checkImplicationElimination :: [Token] -> [[Token]] -> Bool
checkImplicationElimination conc (p2:p1:[]) | (isJust ( findRightSideAplicationPoint p1 TImplies) ) = if ( ( p2 == ((getLeftOfTurnstile p1) ++ ( getLeftArgument p1 (fromJust ( findRightSideAplicationPoint p1 TImplies) ) ) ) ) && ( (getLeftOfTurnstile p1) == (getLeftOfTurnstile p2) ) && (conc == ( (getLeftOfTurnstile p1) ++ ( getRightArgument p1 (fromJust ( findRightSideAplicationPoint p1 TImplies) ) ) ) ) ) then True else False
checkImplicationElimination _ _ = False

checkImplicationIntroduction :: [Token] -> [[Token]] -> Bool
checkImplicationIntroduction conc (p:[]) | (isJust ( findRightSideAplicationPoint conc TImplies) ) = if ( (checkIfElementExists p (getLeftArgument conc (fromJust ( findRightSideAplicationPoint conc TImplies) )) ) && ((getRightOfTurnstile p) == (getRightArgument conc (fromJust ( findRightSideAplicationPoint conc TImplies) )) ) ) then True else False
checkImplicationIntroduction _ _ = False

checkOrIntroduction1 :: [Token] -> [[Token]] -> Bool
checkOrIntroduction1 conc (p:[]) | (isJust ( findRightSideAplicationPoint conc TOr) )= if ( (getRightOfTurnstile p) == (getLeftArgument conc (fromJust ( findRightSideAplicationPoint conc TOr) )) ) then True else False
checkOrIntroduction1 _ _ = False

checkOrIntroduction2 :: [Token] -> [[Token]] -> Bool
checkOrIntroduction2 conc (p:[]) | (isJust ( findRightSideAplicationPoint conc TOr) )= if ( (getRightOfTurnstile p) == (getRightArgument conc (fromJust ( findRightSideAplicationPoint conc TOr) )) ) then True else False
checkOrIntroduction2 _ _ = False

checkOrElimination :: [Token] -> [[Token]] -> Bool
checkOrElimination conc (p3:p2:p1:[]) | (isJust ( findRightSideAplicationPoint p1 TOr) ) = if ( (checkIfElementExists (getLeftOfTurnstile p2) ( getLeftArgument p1 (fromJust ( findRightSideAplicationPoint p1 TOr) ) )) && (checkIfElementExists (getLeftOfTurnstile p3) ( getRightArgument p1 (fromJust ( findRightSideAplicationPoint p1 TOr) ) )) && ( (getRightOfTurnstile conc) == (getRightOfTurnstile p2) ) && ( (getRightOfTurnstile conc) == (getRightOfTurnstile p3) ) ) then True else False
checkOrElimination _ _ = False

checkNotElimination :: [Token] -> [[Token]] -> Bool
checkNotElimination conc (p2:p1:[]) = if ( ( (getRightOfTurnstile conc) == [TBottom] ) && ( (getRightOfTurnstile p2) == ( [TLParen] ++ [TNot] ++ (getRightOfTurnstile p1) ++ [TRParen] )) ) then True else False
checkNotElimination _ _ = False

checkNotIntroduction :: [Token] -> [[Token]] -> Bool
checkNotIntroduction conc (p:[]) | (isJust ( findRightSideAplicationPoint conc TNot) ) = if ( checkIfElementExists p (getRightArgument conc (fromJust ( findRightSideAplicationPoint conc TNot) )) ) then True else False
checkNotIntroduction _ _ = False

checkBottomElimination :: [Token] -> [[Token]] -> Bool
checkBottomElimination conc (p:[]) = if ( (getRightOfTurnstile p) == [TBottom] ) then True else False
checkBottomElimination _ _ = False

checkNotNotIntroduction :: [Token] -> [[Token]] -> Bool
checkNotNotIntroduction conc (p:[]) = if ( (getRightOfTurnstile p) == ( [TLParen] ++ [TNot] ++ [TLParen] ++ [TNot] ++ (getRightOfTurnstile conc) ++ [TRParen] ++ [TRParen] ) ) then True else False
checkNotNotIntroduction _ _ = False
