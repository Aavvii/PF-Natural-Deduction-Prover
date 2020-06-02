module RulesAplicationsAndFindSolution where

import Tokenization
import TokenInfoExtraction
import BackwardsRulesImplementation

import Data.Maybe


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

-------------------------------------- <<<<<<<<<< NU UITA CA AI STRICAT getWhatIsNotInHistory fiindca parea intuila
--                                                    Motive de a renunta la ea:
--                                                       -programul aplica not elimination si crede ca e ok
--                                                       -ma incurca la verificarea de final
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
applyImplicationElimination' history ( h : _ ) initialRule index | (h==TImplies) && ((getWhatIsNotInHistory history  (backwardsImplicationElimination initialRule index)) /= [] ) && ( (getRightArgument initialRule index)==(getRightOfTurnstile initialRule) )  && (areAllResultsProvableWithHistory (history ++ ( getWhatIsNotInHistory history (backwardsImplicationElimination initialRule index))) ( getWhatIsNotInHistory history (backwardsImplicationElimination initialRule index)) ) = Just ( getWhatIsNotInHistory history (backwardsImplicationElimination initialRule index) )
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



-- ======================================================================================================================================
{-
  _____   ___               _     ____            _           _     _                       
 |  ___| |_ _|  _ __     __| |   / ___|    ___   | |  _   _  | |_  (_)   ___    _ __    ___ 
 | |_     | |  | '_ \   / _` |   \___ \   / _ \  | | | | | | | __| | |  / _ \  | '_ \  / __|
 |  _|    | |  | | | | | (_| |    ___) | | (_) | | | | |_| | | |_  | | | (_) | | | | | \__ \
 |_|     |___| |_| |_|  \__,_|   |____/   \___/  |_|  \__,_|  \__| |_|  \___/  |_| |_| |___/
-}

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
-------------------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------------------

applyAllOnce :: [[Token]] -> [[Token]] -> Token -> (Maybe [[Token]] , [Token] , [[Token]])
applyAllOnce history (h : _ ) number | applyBackwardsAssumption (tail h)                                               = (Just ([]), [(TRule("Assumption"))], (history ))
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyBackwardsAndIntroduction)         = ( giveNumbers (applyBackwardsAndIntroduction         history (tail h)) number ,         [TRule ("And Introduction")]          ++ [(TNr ((getNr number)+1))] ++ [(TNr ((getNr number)+2))],                              (history ++ (fromJust(applyBackwardsAndIntroduction         history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyBackwardsImplicationIntroduction) = ( giveNumbers (applyBackwardsImplicationIntroduction history (tail h)) number ,         [TRule ("Implication Introduction")]  ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyBackwardsImplicationIntroduction history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyBackwardsOrIntroduction1)         = ( giveNumbers (applyBackwardsOrIntroduction1         history (tail h)) number ,         [TRule ("Or Introduction 1")]         ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyBackwardsOrIntroduction1         history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyBackwardsOrIntroduction2)         = ( giveNumbers (applyBackwardsOrIntroduction2         history (tail h)) number ,         [TRule ("Or Introduction 2")]         ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyBackwardsOrIntroduction2         history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyAndElimination1)                  = ( giveNumbers (applyAndElimination1                  history (tail h)) number ,         [TRule ("And Elimination 1")]         ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyAndElimination1                  history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyAndElimination2)                  = ( giveNumbers (applyAndElimination2                  history (tail h)) number ,         [TRule ("And Elimination 2")]         ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyAndElimination2                  history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyImplicationElimination)           = ( giveNumbers (applyImplicationElimination           history (tail h)) number ,         [TRule ("Implication Elimination")]   ++ [(TNr ((getNr number)+1))] ++ [(TNr ((getNr number)+2))],                              (history ++ (fromJust(applyImplicationElimination           history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyOrElimination)                    = ( giveNumbers (applyOrElimination                    history (tail h)) number ,         [TRule ("Or Elimination")]            ++ [(TNr ((getNr number)+1))] ++ [(TNr ((getNr number)+2))] ++ [(TNr ((getNr number)+3))],(history ++ (fromJust(applyOrElimination                    history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyNotNotElimination)                = ( giveNumbers (applyNotNotElimination                history (tail h)) number ,         [TRule ("Not Not Elimination")]       ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyNotNotElimination                history (tail h) ))) )
applyAllOnce history (h : tl) number | (leadsToSolution history ((tail h) : tl) applyNotElimination)                   = ( giveNumbers (applyNotElimination                   history (tail h)) number ,         [TRule ("Not Elimination")]           ++ [(TNr ((getNr number)+1))] ++ [(TNr ((getNr number)+2))],                              (history ++ (fromJust(applyNotElimination                   history (tail h) ))) )
--Cazuri speciale pentru bottom
applyAllOnce history (h : tl) number | (bottomEliminationThenNotElimination history ((tail h) : tl) )                  = ( giveNumbers (applyBackwardsBottomElimination       history (tail h)) number ,         [TRule ("Bottom Elimination")]        ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyBackwardsBottomElimination       history (tail h) ))) )
applyAllOnce history (h : tl) number | (notIntroductionThenNotElimination   history ((tail h) : tl) )                  = ( giveNumbers (applyBackwardsNotIntroduction         history (tail h)) number ,         [TRule ("Not Introduction")]          ++ [(TNr ((getNr number)+1))],                                                            (history ++ (fromJust(applyBackwardsNotIntroduction         history (tail h) ))) )
--Caz de eroare
applyAllOnce _ _ _ = ( Nothing, [TRule "This message will never be shown anyway, so... How did the scarecrow do in his PhD? - He was outstanding in his field."], []:[])

leadsToSolution :: [[Token]] -> [[Token]] -> ( [[Token]] -> [Token] -> Maybe [[Token]])-> Bool
leadsToSolution _ [] _ = False
--leadsToSolution history _ _ | (length history) > 3 = False
--leadsToSolution history _ _  | (hasloop history) = False
leadsToSolution history (h : [] ) _ | (applyBackwardsAssumption h) = True
leadsToSolution history (h : tl ) _ | (applyBackwardsAssumption h) = True && ( hasSolution history tl )
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
