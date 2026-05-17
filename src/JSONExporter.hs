module JSONExporter
    ( exprToJSON
    , ruleToJSON
    , grammarToJSON
    ) where

import AST

joinWithComma :: [String] -> String
joinWithComma [] = ""
joinWithComma [x] = x
joinWithComma (x:xs) = x ++ "," ++ joinWithComma xs

exprToJSON :: Expr -> String

exprToJSON (Literal s) =
    "{\"type\":\"literal\",\"value\":\"" ++ escapeJSON s ++ "\"}"

exprToJSON (NonTerminal ntName) =
    "{\"type\":\"nonterminal\",\"name\":\"" ++ escapeJSON ntName ++ "\"}"

exprToJSON (Sequence exprs) =
    "{\"type\":\"sequence\",\"elements\":[" ++
    joinWithComma (map exprToJSON exprs) ++
    "]}"

exprToJSON (Choice exprs) =
    "{\"type\":\"choice\",\"options\":[" ++
    joinWithComma (map exprToJSON exprs) ++
    "]}"

exprToJSON (ZeroOrMore e) =
    "{\"type\":\"zero_or_more\",\"expr\":" ++ exprToJSON e ++ "}"

exprToJSON (OneOrMore e) =
    "{\"type\":\"one_or_more\",\"expr\":" ++ exprToJSON e ++ "}"

exprToJSON (Optional e) =
    "{\"type\":\"optional\",\"expr\":" ++ exprToJSON e ++ "}"

exprToJSON (And e) =
    "{\"type\":\"and\",\"expr\":" ++ exprToJSON e ++ "}"

exprToJSON (Not e) =
    "{\"type\":\"not\",\"expr\":" ++ exprToJSON e ++ "}"

exprToJSON (CharClass s) =
    "{\"type\":\"class\",\"value\":\"" ++ escapeJSON s ++ "\"}"

exprToJSON AnyChar =
    "{\"type\":\"any\"}"

ruleToJSON :: Rule -> String
ruleToJSON (Rule name e) =
    "{\"NT\":\"" ++ escapeJSON name ++ "\",\"expr\":" ++ exprToJSON e ++ "}"

grammarToJSON :: [Rule] -> String
grammarToJSON rules =
    "[" ++ joinWithComma (map ruleToJSON rules) ++ "]"

escapeJSON :: String -> String
escapeJSON = concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar c    = [c]