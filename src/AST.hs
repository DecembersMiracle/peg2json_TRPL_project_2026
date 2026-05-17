module AST where

data Expr
    = Literal String
    | NonTerminal String
    | Sequence [Expr]
    | Choice [Expr]
    | ZeroOrMore Expr
    | OneOrMore Expr
    | Optional Expr
    | And Expr
    | Not Expr
    | CharClass String
    | AnyChar
    deriving (Show, Eq)

data Rule = Rule
    { nt   :: String
    , expr :: Expr
    } deriving (Show, Eq)