module Main where

import PEGParser
import AST
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    putStrLn "\n=== Testing PEG Parser ===\n"
    
    let positiveTests = 
            [ ("Literal", "A = \"hello\"", [Rule "A" (Literal "hello")])
            , ("Literal with \\n", "A = \"hello\\nworld\"", [Rule "A" (Literal "hello\nworld")])
            , ("Literal with \\\"", "A = \"\\\"quote\\\"\"", [Rule "A" (Literal "\"quote\"")])
            , ("Nonterminal", "A = B", [Rule "A" (NonTerminal "B")])
            , ("Dot", "A = .", [Rule "A" AnyChar])
            , ("Star", "A = \"a\"*", [Rule "A" (ZeroOrMore (Literal "a"))])
            , ("Plus", "A = \"a\"+", [Rule "A" (OneOrMore (Literal "a"))])
            , ("Question", "A = \"a\"?", [Rule "A" (Optional (Literal "a"))])
            , ("And predicate", "A = &\"a\"", [Rule "A" (And (Literal "a"))])
            , ("Not predicate", "A = !\"a\"", [Rule "A" (Not (Literal "a"))])
            , ("Sequence", "A = \"a\" \"b\"", [Rule "A" (Sequence [Literal "a", Literal "b"])])
            , ("Choice", "A = \"a\" / \"b\"", [Rule "A" (Choice [Literal "a", Literal "b"])])
            , ("Char class", "A = [abc]", [Rule "A" (CharClass "abc")])
            , ("Parentheses", "A = (\"a\" \"b\")", [Rule "A" (Sequence [Literal "a", Literal "b"])])
            , ("Recursion", "A = \"a\" A / \"b\"", [Rule "A" (Choice [Sequence [Literal "a", NonTerminal "A"], Literal "b"])])
            , ("Nested sequence", "A = \"a\" (\"b\" \"c\") \"d\"", 
               [Rule "A" (Sequence [Literal "a", Sequence [Literal "b", Literal "c"], Literal "d"])])
            , ("Complex char class", "A = [0-9a-zA-Z_]", 
               [Rule "A" (CharClass "0-9a-zA-Z_")])
            , ("Multiple predicates", "A = &\"a\" !\"b\" \"c\"", 
               [Rule "A" (Sequence [And (Literal "a"), Not (Literal "b"), Literal "c"])])
            , ("Multiple quantifiers", "A = \"a\"* \"b\"+ \"c\"?", 
               [Rule "A" (Sequence [ZeroOrMore (Literal "a"), OneOrMore (Literal "b"), Optional (Literal "c")])])
            , ("Choice of 4", "A = \"a\" / \"b\" / \"c\" / \"d\"", 
               [Rule "A" (Choice [Literal "a", Literal "b", Literal "c", Literal "d"])])
            ]
    
    let negativeTests =
            [ ("Empty grammar", "")
            , ("Unclosed string", "A = \"abc")
            , ("Unclosed parenthesis", "A = (\"a\"")
            , ("Unclosed class", "A = [abc")
            , ("Invalid name", "123 = \"a\"")
            , ("Unknown operator", "A = \"a\" % \"b\"")
            , ("Missing =", "A \"a\"")
            , ("Empty rule", "A = ")
            ]
    
    passed1 <- runPositiveTests positiveTests 0
    passed2 <- runNegativeTests negativeTests 0
    
    let total = length positiveTests + length negativeTests
    let passed = passed1 + passed2
    
    putStrLn $ "\n=== Result: " ++ show passed ++ "/" ++ show total ++ " tests passed ===\n"
    
    if passed == total
        then exitSuccess
        else exitFailure

runPositiveTests :: [(String, String, [Rule])] -> Int -> IO Int
runPositiveTests [] passed = return passed
runPositiveTests ((name, input, expected):rest) passed = do
    putStr $ "  " ++ name ++ " ... "
    case parsePEG input of
        Right actual ->
            if actual == expected
                then do
                    putStrLn "OK"
                    runPositiveTests rest (passed + 1)
                else do
                    putStrLn "FAIL"
                    runPositiveTests rest passed
        Left _ -> do
            putStrLn "FAIL"
            runPositiveTests rest passed

runNegativeTests :: [(String, String)] -> Int -> IO Int
runNegativeTests [] passed = return passed
runNegativeTests ((name, input):rest) passed = do
    putStr $ "  " ++ name ++ " ... "
    case parsePEG input of
        Left _ -> do
            putStrLn "OK"
            runNegativeTests rest (passed + 1)
        Right _ -> do
            putStrLn "FAIL"
            runNegativeTests rest passed