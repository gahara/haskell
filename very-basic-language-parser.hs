module Main where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as Lexer

data BinExpr
  = BoolConst Bool
  | Not BinExpr
  | BoolBinary BoolBinOp BinExpr BinExpr
  | RelBinary RelBinOp ArExpr ArExpr
  deriving (Show)

data BoolBinOp
  = And
  | Or
  deriving (Show)

data RelBinOp
  = Greater
  | Less
  deriving (Show)

data ArExpr
  = Var String
  | IntConst Integer
  | Neg ArExpr
  | ArBinary ArBinOp ArExpr ArExpr
  deriving (Show)

data ArBinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data Stmnt
  = Seq [Stmnt]
  | Assign String ArExpr
  | If BinExpr Stmnt Stmnt
  | While BinExpr Stmnt
  | Skip
  deriving (Show)

type Parser = Parsec Void String

spaceconsumer :: Parser ()
spaceconsumer = Lexer.space space1 singleCom blockCom
  where
    singleCom  = Lexer.skipLineComment "//"
    blockCom = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceconsumer

symbol :: String -> Parser String
symbol = Lexer.symbol spaceconsumer

parseparenthesis :: Parser a -> Parser a
parseparenthesis = between (symbol "(") (symbol ")")

parseint :: Parser Integer
parseint = lexeme Lexer.decimal

parsesemi :: Parser String
parsesemi = symbol ";"

resword :: String -> Parser ()
resword w = lexeme (string w *> notFollowedBy alphaNumChar)

reswords :: [String] 
reswords = [
  "true",
  "false",
  "not",
  "and",
  "or",
  "if",
  "then",
  "else",
  "while",
  "do",
  "skip"
  ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reswords
                then fail $ "Stop! " ++ show x ++ " is a reserved word and can not be an identifier"
                else return x

whileParser :: Parser Stmt
whileParser = between spaceconsumer eof stmnt

stmnt :: Parser Stmnt
stmnt = f <$> sepBy1 stmnt' parsesemi
  where
    f l = if length l == 1 then head l else Seq l

stmnt' :: Parser Stmt
stmnt' = ifStmnt
  <|> whileStmnt
  <|> skipStmnt
  <|> assignStmnt
  <|> parseparenthesis stmnt

ifStmt :: Parser Stmt
ifStmt = do
  resword "if"
  cond  <- bExpr
  resword "then"
  stmnt1 <- stmnt
  resword "else"
  stmnt2 <- stmnt
  return (If cond stmnt1 stmnt2)

whileStmnt :: Parser Stmt
whileStmt = do
  resword "while"
  cond <- bExpr
  resword "do"
  stmnt1 <- stmnt
  return (While cond stmnt1)

assignStmt :: Parser Stmnt
assignStmnt = do
  var  <- identifier
  void (symbol ":=")
  expr <- aExpr
  return (Assign var expr)

skipStmnt :: Parser Stmnt
skipStmnt = Skip <$ resword "skip"

arExpr :: Parser ArExpr
arExpr = makeExprParser arTerm arOperators

binExpr :: Parser BinExpr
binExpr = makeExprParser binTerm binOperators

arOperators :: [[Operator Parser ArExpr]]
arOperators =
  [ 
    [ Prefix (Neg <$ symbol "-") ], 
    [ InfixL (ArBinary Multiply <$ symbol "*"), InfixL (ArBinary Divide   <$ symbol "/") ], 
    [ InfixL (ArBinary Add      <$ symbol "+"), InfixL (ArBinary Subtract <$ symbol "-") ]
  ]

binOperators :: [[Operator Parser BExpr]]
binOperators =
  [ 
    [ Prefix (Not <$ resword "not") ], 
    [ InfixL (BoolBinary And <$ resword "and"), InfixL (BoolBinary Or <$ resword "or") ]
  ]

arTerm :: Parser ArExpr
arTerm = parseparenthesis arExpr
  <|> Var      <$> identifier
  <|> IntConst <$> parseint

binTerm :: Parser BinExpr
binTerm =  parseparenthesis binExpr
  <|> (BoolConst True  <$ resword "true")
  <|> (BoolConst False <$ resword "false")
  <|> relExpr

relExpr :: Parser BExpr
rExpr = do
  a1 <- arExpr
  op <- relation
  a2 <- arExpr
  return (RBinary op a1 a2)

relation :: Parser RinBinOp
relation = (symbol ">" *> pure Greater)
  <|> (symbol "<" *> pure Less)

main :: IO ()
main = return ()
