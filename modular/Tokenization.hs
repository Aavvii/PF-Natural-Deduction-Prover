module Tokenization where

import Data.Char

data Token = TVar String | TLParen | TRParen | TAnd | TOr | TNot | TImplies | TTurnstile | TComma | TBottom | TRule String | TNr Int deriving (Eq, Show);

instance Ord Token where
 compare (TNr a) (TNr b) | a < b = LT
 compare (TNr a) (TNr b) | a > b = GT
 compare _ _ = EQ

showTokens :: [Token] -> String
showTokens [] = []
showTokens ((TNr x) : tl) = show x ++ ". "++ (showTokens tl)
showTokens ((TRule "Assumption") : tl) = "    -Assumption " ++ (showTokens tl)
showTokens ((TRule x) : tl) = "    -" ++ x ++ ", " ++ (showTokens tl)
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
tokenize (c : tl) | isSpace c = tokenize tl
tokenize _ = error "No tokenisation."