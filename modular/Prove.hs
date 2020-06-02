module Prove where

import Tokenization
import TokenInfoExtraction
import BackwardsRulesImplementation
import RulesAplicationsAndFindSolution

import Data.Maybe

fstOfThree :: (a, b, c) -> a
fstOfThree (t,_,_) = t

sndOfThree :: (a, b, c) -> b
sndOfThree (_,t,_) = t

trdOfThree :: (a, b, c) -> c
trdOfThree (_,_,t) = t

-- ++ FUNCTIA DE APLICARE A REGULILOR-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

prove :: [Token] -> Maybe [[Token]]
prove [] = Just ([])
prove start  | startHasSolution (start:[])  = Just (prove' ( [start] ) ( [[TNr 1] ++ start] ) [TNr 1] )
prove _ = Nothing

prove' :: [[Token]] -> [[Token]] -> [Token] -> [[Token]]
prove' _       (h : []) _ | (applyBackwardsAssumption (tail h)) = (  h ++ [ (TRule "Assumption") ] ) : []
prove' history (h : tl) numbers | isJust (fstOfThree(applyAllOnce history ([h]) (getMaxLineNr (numbers)) )) = (h ++  ( sndOfThree (applyAllOnce history ([h]) (getMaxLineNr (numbers)) ))  ) : ( prove' ( trdOfThree (applyAllOnce history ([h]) (getMaxLineNr (numbers)) )) ((fromJust(fstOfThree( applyAllOnce history ([h]) (getMaxLineNr (numbers)) )) ) ++ tl) (numbers ++ (getAllHeads (h : fromJust (fstOfThree(applyAllOnce history ([h]) (getMaxLineNr (numbers)) )) )) ) )
prove' _ [] _ = []
-- Asta e un fallback. Daca nu intra in nici un caz de mai sus dar are solutie, resetez istoricul
--prove' _ (h : tl) numbers | startHasSolution [h] = (h ++  ( sndOfThree (applyAllOnce [] ([h]) (getMaxLineNr (numbers)) ))  ) : ( prove' ( trdOfThree (applyAllOnce [] ([h]) (getMaxLineNr (numbers)) )) ((fromJust(fstOfThree( applyAllOnce [] ([h]) (getMaxLineNr (numbers)) )) ) ++ tl) (numbers ++ (getAllHeads (h : fromJust (fstOfThree(applyAllOnce [] ([h]) (getMaxLineNr (numbers)) )) )) ) )
prove' history _ _ = [[(TRule "!!!- An Error Occured. Above is history")]] ++ history
