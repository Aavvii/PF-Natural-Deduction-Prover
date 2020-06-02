--{-# OPTIONS_GHC -Wall #-}

import Data.Maybe

import Tokenization
import TokenInfoExtraction
import BackwardsRulesImplementation
import RulesAplicationsAndFindSolution
import OrderAplications
import ForwardFinalCheck
import Prove

main :: IO ()
main = do 
       putStrLn "\nScrieti o formula de logica propozitionala sau \"exit\" pentru a opri programul."
       inFormula <- getLine
       if ( inFormula /= "exit" ) then
          do
          if ( (canTokenize inFormula) && (isThereATurnistle inFormula) ) then
            do
            if ( (isNothing (prove (tokenize (inFormula))) )  ) then
               do
               putStrLn "Nu a fost gasita o demonstratie."
               main
            else
               if ( (isNothing( forwardCheck ( orderRules  ( prove ( tokenize inFormula )) ) ))  ) then
                  putStrLn "O demonstratie a fost gasita dar verificarea finala a esuat."
               else
                  do
                  putStrLn ( "\n" ++ showMaybeTokensArray ( forwardCheck ( orderRules  ( prove (tokenize (inFormula))) ) ) )
                  main
           else
               do
               putStrLn "Formula data nu este valida."
               main
       else putStrLn ""