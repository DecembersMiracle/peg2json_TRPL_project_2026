module PEGParser (parsePEG) where

import Text.Parsec
import Text.Parsec.String (Parser)
import AST

ws :: Parser ()
ws = skipMany (oneOf " \t\r")

endOfRule :: Parser ()
endOfRule = do
    skipMany (oneOf " \t\r")
    skipMany1 (char '\n')
    skipMany (oneOf " \t\r")

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    ws
    return x

symbol :: Char -> Parser Char
symbol c = lexeme (char c)

identifier :: Parser String
identifier = lexeme $ do
    first <- letter <|> char '_'
    rest <- many (alphaNum <|> char '_')
    return (first : rest)

literal :: Parser Expr
literal = lexeme $ do
    char '"'
    content <- many literalChar
    char '"'
    return (Literal content)

literalChar :: Parser Char
literalChar =
        try escapedChar
    <|> noneOf "\""

escapedChar :: Parser Char
escapedChar = do
    char '\\'
    c <- oneOf "\"\\nt"
    return $
        case c of
            '"'  -> '"'
            '\\' -> '\\'
            'n'  -> '\n'
            't'  -> '\t'
            _    -> c

charClass :: Parser Expr
charClass = lexeme $ do
    char '['
    content <- many (noneOf "]")
    char ']'
    return (CharClass content)

primary :: Parser Expr
primary =
        try literal
    <|> try charClass
    <|> try (NonTerminal <$> identifier)
    <|> try (do
            symbol '('
            e <- expression
            symbol ')'
            return e)
    <|> do
            char '.'
            ws
            return AnyChar

suffix :: Parser Expr
suffix = do
    p <- primary
    option p $ do
        op <- lexeme (oneOf "*+?")
        return $
            case op of
                '*' -> ZeroOrMore p
                '+' -> OneOrMore p
                '?' -> Optional p
                _   -> p

prefix :: Parser Expr
prefix =
        try (do
            symbol '!'
            Not <$> suffix)
    <|> try (do
            symbol '&'
            And <$> suffix)
    <|> suffix

sequenceExpr :: Parser Expr
sequenceExpr = do
    first <- prefix
    rest <- many (try (ws >> notFollowedBy (char '\n') >> prefix))

    let exprs = first : rest

    return $
        case exprs of
            [single] -> single
            _        -> Sequence exprs

expression :: Parser Expr
expression = do
    first <- sequenceExpr
    rest <- many $ do
        ws
        char '/'
        ws
        sequenceExpr

    return $
        case rest of
            [] -> first
            _  -> Choice (first : rest)

rule :: Parser Rule
rule = do
    name <- identifier
    symbol '='
    e <- expression
    return (Rule name e)

grammar :: Parser [Rule]
grammar = do
    ws
    firstRule <- rule
    restRules <- many (try (endOfRule >> rule))
    ws
    eof
    return (firstRule : restRules)

parsePEG :: String -> Either ParseError [Rule]
parsePEG = parse grammar ""