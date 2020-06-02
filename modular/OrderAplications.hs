module OrderAplications where

import Tokenization
import RulesAplicationsAndFindSolution

import Data.List

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

