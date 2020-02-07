{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module ToDelPrj.MiniC_lib2 where


import           Control.Applicative     hiding ( some
                                                , many
                                                )
import           Control.Monad.Identity
import           Control.Monad

import           Data.Void                      ( Void )
import           Text.Megaparsec         hiding ( Label
                                                , State
                                                ) --( parseTest )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex
import qualified Control.Monad.Combinators.Expr
                                               as ExprParser
import           Text.Megaparsec.Error   hiding ( Label )
import           Text.Megaparsec.Error.Builder
import           Text.Megaparsec.Internal
import           Text.Megaparsec.Pos
import           Text.Megaparsec.Stream

import qualified Data.Text                     as T


            {-
              WHILE Language Grammar
              a   ::= x | n | - a | a opa a
              b   ::= true | false | not b | b opb b | a opr a
              opa ::= + | - | * | / |  % | ...
              opb ::= and | or | ...
              opr ::= > | < |  <= | >= | == | != | ....
              S   ::= x := a|R := (a 1 ,a 2 ); | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S
            -}

            -- in haskell we represent the abstract syntax tree
            -- or BNF grammar as algebraic data types

           -- stack exec ghcid -- -T":MiniC_lib"

        -- type MyParsec e s a = ParsecT e s Identity a
type Parser = Parsec Void String

wordP :: Parser String
wordP = some alphaNumChar


  --- parseTest (satisfy (== 'a') :: Parser Char) "a"
-- parseTest (char 'a' :: Parser Char) "b"
-- parseTest (string "foo" :: Parser String) "foo"

mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)
--- parseTest mySequence "abc"

myArray :: Parser (Char, Char, Char)
myArray = (,,) <$> char '[' <*> char 'b' <*> char ']'
--- parseTest mySequence2 "abc"

-- parseTest (many (char 'a') :: Parser [Char]) "aaabbb"
-- parseTest (many (char 'a') <* eof :: Parser [Char]) "aabbb"



    --  parseTest (opExpr <* eof) "(a < 3) &&  (b == 2)"
--  parseTest (opExpr <* eof) "(a < 3) &&  (A[3] == 2)"

data RecordIndexing = First Int
                      | Second Int
                      deriving (Eq,Ord, Show)

newtype IdentifierName = IdentifierName String
                       deriving (Eq,Ord, Show)

makeone :: Integer -> Integer
makeone n = n

newtype Label2 = Label2 {getLabel2::Integer}
                  deriving (Eq,Ord, Show)

data Label3 =  Label3 Int
              | LabelNull
              deriving (Eq,Ord, Show)

newtype Label = Label (Maybe Int)
                deriving (Eq,Ord,Show)

data Program =  Dec Declaration
                | St Statement
                | ProgramSeq [Program]
                deriving (Eq,Ord,Show)

data Statement a   where
    StmtSeq      ::Statement LabelNull   -> Statement LabelNull  -> Statement LabelNull
    AssLit    ::LiteralExpr -> AExpr Int -> Statement LabelNull
    AssRecord ::IdentifierName ->AExpr Int  ->AExpr Int->Statement LabelNull
    IfElse ::Statement Bool-> Statement LabelNull ->Statement LabelNull->Statement LabelNull
    If ::Statement Bool-> Statement LabelNull->Statement LabelNull
    While ::Statement Bool ->Statement LabelNull->Statement LabelNull
    Skip ::Label ->Statement LabelNull
    Read ::LiteralExpr->Statement LabelNull
    Write ::AExpr Int->Statement LabelNull
    BoolConst ::Label  ->Bool -> Statement Bool
    Not    ::Label ->Statement Bool-> Statement Bool
    And::Label  ->Statement Bool ->   Statement Bool -> Statement Bool
    Or::Label  ->Statement Bool ->   Statement Bool -> Statement Bool
    Greater       ::Label -> AExpr Int ->  AExpr Int -> Statement Bool
    Less  ::Label -> AExpr Int ->  AExpr Int -> Statement Bool
    GreaterOrEqual ::Label -> AExpr Int ->  AExpr Int -> Statement Bool
    LessOrEqual  ::Label -> AExpr Int ->  AExpr Int -> Statement Bool
    Equality  ::Label -> AExpr Int ->  AExpr Int -> Statement Bool
    Different ::Label -> AExpr Int ->  AExpr Int -> Statement Bool

data AExpr a where
    AInt      ::Int  -> AExpr Int
    ALExpr    ::LiteralExpr -> AExpr LiteralExpr
    Sum       ::AExpr Int -> AExpr Int -> AExpr Int
    Subtract  ::AExpr Int -> AExpr Int -> AExpr Int
    Multiply  ::AExpr Int -> AExpr Int -> AExpr Int
    Divide    ::AExpr Int -> AExpr Int -> AExpr Int
    Modulus   ::AExpr Int -> AExpr Int -> AExpr Int

-- used to flatten parsed list of statements into a sequence of statements instead of nested list of lists               
flatListStatements :: [Statement] -> Statement
flatListStatements [s1     ] = s1
flatListStatements (s1 : xs) = StmtSeq s1 (flatListStatements xs)


-- Expressions
data LiteralExpr =  LitVar IdentifierName
                    | LitRecord IdentifierName RecordIndexing
                    | LitArray IdentifierName (AExpr Int)
                     deriving (Eq,Ord, Show)


-- Declarations
newtype Size = Size Int
                deriving (Eq,Ord,Show)

data Declaration =    DeclVar Label LiteralExpr --Int type
                    | DeclArray Label IdentifierName Size  --Int type
                    | DeclRecord Label IdentifierName RecordIndexing RecordIndexing
                    | Empty
                    | DeclSeq Declaration Declaration
                   -- | DeclSeq [Declaration]
                    deriving (Eq,Ord,Show)
-- Declarations
-- used to flatten parsed list of Declarations into a sequence of Declarations instead of nested list of lists               
flatListDeclarations :: [Declaration] -> Declaration
flatListDeclarations []        = Empty
flatListDeclarations [d1     ] = d1
flatListDeclarations (d1 : xs) = DeclSeq d1 (flatListDeclarations xs)



declaration :: Parser Declaration
declaration =
  try (parens declaration)
    <|> try (braces declaration)
    <|> try (braces sequenceOfDecl)
    <|> try sequenceOfDecl


sequenceOfDecl :: Parser Declaration
sequenceOfDecl = do
  list <-
    try (sepEndBy declaration' semicolon)
    <|> try (sepEndBy declaration' spaceConsumer)
    <|> try (endBy declaration' eof)
  return $ if length list == 1 then head list else flatListDeclarations list
  --list <- sepBy1 statement' whiteSpace
-- If there's only one statement return it without using Seq.
  --return $ if length list == 1 then head list else DeclSeq list

declaration' :: Parser Declaration
declaration' = try pDeclVariable <|> try pDeclArray <|> try pDeclRecord

-- Operators


-- Operators

spaceConsumer :: Parser ()
spaceConsumer = Lex.space space1                         -- (2)
                          (Lex.skipLineComment "//")       -- (3)
                          (Lex.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = Lex.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

angles = between (symbol "<") (symbol ">")
brackets = between (symbol "[") (symbol "]")
semicolon = symbol ";"
comma = symbol ","
colon = symbol ":"
dot = symbol "."

integer :: Parser Int
integer = lexeme Lex.decimal

signedInteger :: Parser Int
signedInteger = Lex.signed spaceConsumer integer

pKeyword :: String -> Parser String
pKeyword keyword = lexeme (string' keyword <* notFollowedBy alphaNumChar)

divOrMod2 = pKeyword "div" <|> pKeyword "mod"
divOrMod = lexeme (string' "div" Text.Megaparsec.<|> string' "mod")

reserved :: String -> Parser ()
reserved keyword =
  (lexeme . try) (string' keyword *> notFollowedBy alphaNumChar)

isVariable :: Parser String
isVariable = (lexeme . try) (parseStr >>= check) <?> "identifier"
 where
  parseStr = (:) <$> letterChar <*> many alphaNumChar
  check str = if str `elem` reservedWords
    then fail $ "keyword " ++ show str ++ " cannot be an identifier"
    else return str
  reservedWords =
    [ "if"
    , "else"
    , "while"
    , "skip"
    , "read"
    , "write"
    , "fst"
    , "snd"
    , "break"
    , "int"
    , "true"
    , "false"
    , "not"
    , "and"
    , "or"
    ]

pAInteger :: Parser AExpr
pAInteger = AInt <$> integer
--  parseTest (pInteger :: Parser Expr) "AAAA[3]"

pLitVariable :: Parser LiteralExpr
pLitVariable = do
  var <- isVariable
  return (LitVar (IdentifierName var))
{-
pLitVariable = LitVar . IdentifierName <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable parsing")
-}

pAVariable :: Parser AExpr
pAVariable = ALExpr <$> pLitVariable

pDeclVariable :: Parser Declaration
pDeclVariable = DeclVar (Label Nothing) <$> (symbol "int" *> pLitVariable)
-- parseTest ( spaceConsumer *> pDeclVariable <* eof) "int x"

pLitArray :: Parser LiteralExpr
pLitArray = do
  v <- (:) <$> letterChar <*> many letterChar <* lookAhead "["
  d <- brackets aOpExpression --wordP
  return (LitArray (IdentifierName v) d)

pAArray :: Parser AExpr
pAArray = ALExpr <$> pLitArray
-- parseTest (pAArray <* eof) "(A[3])"
{-
pAArray = do
  v <- pLitArray
  return (AExpr v)
  -}
--  parseTest (pAArray :: Parser AExpr) "AAAA[3]"

pDeclArray :: Parser Declaration
pDeclArray = do
  symbol "int"
  size <- brackets integer
  var  <- isVariable
  return (DeclArray (Label Nothing) (IdentifierName var) (Size size))
-- parseTest ( spaceConsumer *> pDeclArray <* eof) " int[123] A"



 {-
        xRecordAccess :: Parser XExpr
        xRecordAccess = VarX . RecordAccess <$> lexeme
          ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
        
        xArraySubscrpting :: Parser XExpr
        xArraySubscrpting = ArraySubscript <$> lexeme
          (   (:)
          <$> letterChar
          <*> many alphaNumChar
          <*  lookAhead brackets integer
          <?> "variable"
          )
        
        
        xFunctionCall :: Parser XExpr
        xFunctionCall = VarX . FunctionCall <$> lexeme
          ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
        
        -}

pLitRecord :: Parser LiteralExpr
pLitRecord = do
  varRec <- isVariable
  symbol "."
  index <- wordP --getLexKeyword
  case index of
    "fst" -> return (LitRecord (IdentifierName varRec) (First 0))
    "snd" -> return (LitRecord (IdentifierName varRec) (Second 0))

pARecord :: Parser AExpr
pARecord = ALExpr <$> pLitRecord
-- parseTest (pARecord <* eof) "R.fst"

pDeclRecord :: Parser Declaration
pDeclRecord = do
  symbol "{"
  symbol "int"
  var1 <- wordP
  symbol ";"
  symbol "int"
  var2 <- wordP
  symbol "}"
  var <- isVariable -- pLitVariable
  return (DeclRecord (Label Nothing) (IdentifierName var) (First 0) (Second 0))
-- parseTest ( spaceConsumer *> pDeclRecord <* eof) " {int fst;int snd} R"


aTerm :: Parser AExpr
aTerm = choice
  [parens aOpExpression, try pAArray, try pARecord, pAVariable, pAInteger]

aOpExpression :: Parser AExpr
aOpExpression = ExprParser.makeExprParser aTerm aOperatorTable
-- parseTest (aOpExpression <* eof) "f*2 + 7+ A[5]*4"
aOperatorTable :: [[ExprParser.Operator Parser AExpr]]
aOperatorTable =
  [ [ --ExprParser.Prefix (Negation <$ symbol "-")
     ExprParser.Prefix (id <$ symbol "+")]
  , [ ExprParser.InfixL (ABinary Multiply <$ symbol "*")
    , ExprParser.InfixL (ABinary Divide <$ symbol "/")
    , ExprParser.InfixL (ABinary Modulus <$ symbol "%")
    ]
  , [ ExprParser.InfixL (ABinary Sum <$ symbol "+")
    , ExprParser.InfixL (ABinary Subtract <$ symbol "-")
    ]
  ]

relation =
  (symbol ">" >> return Greater)
    <|> (symbol "<" >> return Less)
    <|> (symbol "<=" >> return LessOrEqual)
    <|> (symbol ">=" >> return GreaterOrEqual)
    <|> (symbol "==" >> return Equality)
    <|> (symbol "!=" >> return Different)

rExpression :: Parser Statement
rExpression = do
  a1 <- aOpExpression
  op <- relation
  a2 <- aOpExpression
  return $ RBinary (Label Nothing) op a1 a2

bTerm :: Parser Statement
bTerm = choice
  [ try (parens bOpExpression)
  , try rExpression
  , reserved "true" >> return (BoolConst (Label Nothing) True)
  , reserved "false" >> return (BoolConst (Label Nothing) False)
  ]

{-
bTerm :: Parser BExpr
bTerm =
  parens bOpExpression
    <|> (reserved "true" >> return (BoolConst True))
    <|> (reserved "false" >> return (BoolConst False))
    <|> rExpression
    -- <|> parens aOpExpression
-}
bOpExpression :: Parser Statement
bOpExpression = ExprParser.makeExprParser bTerm bOperatorTable
{-
bOperatorTable :: [[ExprParser.Operator Parser BExpr]]
bOperatorTable =
  [ [ ExprParser.InfixL (RBinary Greater <$ symbol ">")
    , ExprParser.InfixL (RBinary Less <$ symbol "<")
    , ExprParser.InfixL (RBinary LessOrEqual <$ symbol "<=")
    , ExprParser.InfixL (RBinary GreaterOrEqual <$ symbol ">=")
    ]
  , [ ExprParser.InfixL (RBinary Equality <$ symbol "==")
    , ExprParser.InfixL (RBinary Different <$ symbol "!=")
    ]
  , [ExprParser.Prefix (Not <$ symbol "!")]
  , [ExprParser.InfixL (BBinary And <$ symbol "&&")]
  , [ExprParser.InfixL (BBinary Or <$ symbol "||")]
  ]
-}

bOperatorTable :: [[ExprParser.Operator Parser Statement]]
bOperatorTable =
  [ [ExprParser.Prefix (Not (Label Nothing) <$ symbol "!")]
  , [ExprParser.InfixL (BBinary (Label Nothing) And <$ symbol "&&")]
  , [ExprParser.InfixL (BBinary (Label Nothing) Or <$ symbol "||")]
  ]
--  parseTest ( spaceConsumer *> bOpExpression <* eof) "a&&b"
--  parseTest (bOpExpression <* eof) "a&&b||(4<7)"


whileParser :: Parser Program
whileParser = do
  spaceConsumer
  d <- declaration
  s <- between space eof statement
  return (ProgramSeq [Dec d, St s])
-- parseTest (spaceConsumer *> whileParser <* eof) "int y;int x; if (3<4) {y:= x ;} else { z:= z*2;}; while (1<2) {z:= z*2;}"


whileParser123 :: Parser Statement
whileParser123 = between space eof statement
-- parseTest ( spaceConsumer *> whileParser  <* eof)  "if (true && false) {y:= x }"
--  parseTest ( spaceConsumer *> whileParser <* eof)  "if (3<4) {y:= x; } else { z:= z*2;}"
-- parseTest ( spaceConsumer *> whileParser <* eof) "while (1<2) {z:= z*2;}"
-- parseTest ( spaceConsumer *> whileParser <* eof) "y:= x; if (3<4) {y:= x ;} else { z:= z*2;}; while (1<2) {z:= z*2;}"

statement :: Parser Statement
statement =
  try (parens statement)
    <|> try (braces statement)
    <|> try (braces sequenceOfStmt)
    <|> try sequenceOfStmt

sequenceOfStmt :: Parser Statement
sequenceOfStmt = do
  list <-
    try (sepEndBy statement' semicolon)
    <|> try (sepEndBy statement' spaceConsumer)
    <|> try (endBy statement' eof)
  --list <- sepBy1 statement' whiteSpace
-- If there's only one statement return it without using Seq.
  return $ if length list == 1 then head list else flatListStatements list
-- return $ if length list == 1 then head list else StmtSeq list

statement' :: Parser Statement
statement' =
  try ifElseStmt
    <|> try ifStmt
    <|> try whileStmt
    <|> try skipStmt
    <|> try recordStmt
    <|> try readStmt
    <|> try writeStmt
    <|> try assignStatement

ifStmt :: Parser Statement
ifStmt = do
  reserved "if"
  condition <- bOpExpression
  stmt1     <- statement
  return (If condition stmt1)
 -- parseTest (spaceConsumer *> ifStmt <* eof) "if(x<2){b:=7;}"
 -- parseTest ( spaceConsumer *> ifStmt <* eof)  "if (true && false) {y:= x; }"
ifElseStmt :: Parser Statement
ifElseStmt = do
  reserved "if"
  cond  <- bOpExpression
  stmt1 <- statement
  reserved "else"
  stmt2 <- statement
  return $ IfElse cond stmt1 stmt2
--  parseTest ( spaceConsumer *> ifElseStmt <* eof)  "if (3<4) {y:= x; } else { z:= z*2;}"

whileStmt :: Parser Statement
whileStmt = do
  reserved "while"
  cond  <- bOpExpression
  stmt1 <- statement
  return (While cond stmt1)
 -- parseTest ( spaceConsumer *> whileStmt <* eof) "while (1<2) {z:= z*2;}"
{-
assignLitVar :: Parser Statement
assignLitVar =
  AssLit
    <$> LitVar
    <$> IdentifierName
    <$> (isVariable <* symbol ":=" *> aOpExpression)
-}
assignLitVar = do
  var <- pLitVariable
  symbol ":="
  expr <- aOpExpression
  return (AssLit (Label Nothing) var expr)

-- (AssLit VarName var ArithBinary expr)
{-
assignLitArr :: Parser Statement
assignLitArr = AssLit <$> pLitArray <* symbol ":=" *> aOpExpression
-}

assignLitArr :: Parser Statement
assignLitArr = do
  varArr <- pLitArray
  symbol ":="
  aExpr <- aOpExpression
  return (AssLit (Label Nothing) varArr aExpr)

assignLitRecord :: Parser Statement
-- assignLitRecord = AssLit <$> pLitRecord <* symbol ":=" *> aOpExpression
assignLitRecord = do
  varRec <- pLitRecord
  symbol ":="
  aExpr <- aOpExpression
  symbol ";"
  return (AssLit (Label Nothing) varRec aExpr)

assignStatement :: Parser Statement
assignStatement = assignLitVar <|> assignLitArr <|> assignLitRecord
 -- parseTest ( spaceConsumer *> assignStatement <* eof)  "y:= x"

recordStmt :: Parser Statement
recordStmt = do
  var <- isVariable
  symbol ":="
  symbol "("
  expr1 <- aOpExpression
  symbol ","
  expr2 <- aOpExpression
  symbol ")"
  return (AssRecord (Label Nothing) (IdentifierName var) expr1 expr2)
-- parseTest ( spaceConsumer *> recordStmt <* eof) "R:=(1+5,4+7)"
readStmt :: Parser Statement
readStmt =
  Read (Label Nothing)
    <$> (reserved "read" *> pLitVariable <|> pLitArray <|> pLitRecord)

readStmt2 = do
  reserved "read"
  var <- pLitVariable <|> pLitArray <|> pLitRecord
  return (Read (Label Nothing) var)

writeStmt :: Parser Statement
writeStmt = Write (Label Nothing) <$> (reserved "write" *> aOpExpression)
{- 
  writeStmt = do 
  reserved "write"
  var <- aOpExpression --evaluate
  return (Write var)
-}
skipStmt :: Parser Statement
skipStmt = Skip (Label Nothing) <$ reserved "skip"

parseString :: String -> Statement
parseString str = case parse whileParser123 "" str of
  Left  e -> error $ show e
  Right r -> r

parseFile :: String -> IO Statement
parseFile file = do
  program <- readFile file
  case parse whileParser123 "" program of
    Left  e -> print e >> fail "parse error"
    Right r -> return r

-- ast <- parseFile "/mnt/c/Users/androgo/Downloads/DTU autumn 2019/02242 Program Analysis E19/other/assignments_course/analysis-project/program1.mc"
-- parseString "y:= x; if (3<4) {y:= x ;} else { z:= z*2;} while (b) {z:= z*2;}"
--  parseString "y:= x; if (3<4) {y:= x }"
  --  parseString "y:= x; if (true && false) {y:= x }"
 -- parseString "y:= x; if (3<4) {y:= x ;} else { z:= z*2;}if (m) {n:= n*9;}"



--  parseTest ( spaceConsumer *> bOpExpression <* eof) "a&&b"
--  parseTest (spaceConsumer *> bOpExpression <* eof) "(a<b)||(4<7)"

--- parseTest ( spaceConsumer *> bOpExpression <* eof) "!(!true ## false )"
--  parseTest ( spaceConsumer *> bOpExpression <* eof) "!((x<y) ## false )"
-- parseTest ( spaceConsumer *> bOpExpression <* eof) "!((A[x]<y) &&  false )"
-- parseTest ( spaceConsumer *> bOpExpression <* eof) "!((A[x]<R.fst) && false )"
-- parseTest ( spaceConsumer *> bOpExpression <* eof)  "y:= x; if (true && false) {y:= x }"
  -- parseTest ( spaceConsumer *> bOpExpression <* eof) "y:= x; if (3<4) {y:= x ;} else { z:= z*2;} while (b) {z:= z*2;}"
