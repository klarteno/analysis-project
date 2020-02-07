{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module ToDelPrj.MiniC_lib where


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

data RecordIndexing = First
                      | Second
                      deriving (Eq,Ord, Show)

newtype IdentifierName = IdentifierName String
                       deriving (Eq,Ord, Show)

makeone :: Int -> Int
makeone n = n


newtype Label3 = Label3 {getLabel::Integer}
                  deriving (Eq,Ord, Show)

data Label =  Label Int
              |LabelNull
              deriving (Eq,Ord, Show)

data Label2 a where
  GetLabel2 ::Int   -> Label2 Int
  SetLabel  ::Int -> Label2 Int

deriving instance Eq a => Eq (Label2 a)
deriving instance Ord a => Ord (Label2 a)
deriving instance Show a => Show (Label2 a)


data Foo = forall a. MkFoo  (a -> a)
                    | forall a. Nil a

data Program =  Dec Declaration
                | St Statement
                | ProgramSeq [Program]



data Statement   where
  StmtSeq      ::Statement   -> Statement  -> Statement
  AssLit    ::LiteralExpr -> AExpr Int -> Statement
  AssRecord ::IdentifierName ->AExpr Int  ->AExpr Int->Statement
  IfElse ::BExpr Bool-> Statement ->Statement->Statement
  If ::BExpr Bool-> Statement->Statement
  While ::BExpr Bool ->Statement->Statement
  Skip ::Label->Statement
  Read ::LiteralExpr->Statement
  Write ::AExpr Int->Statement


-- Expressions
data LiteralExpr  =  LitVar Label  IdentifierName
                    | LitRecord Label  IdentifierName RecordIndexing
                    | LitArray Label  IdentifierName (AExpr Int)
                     deriving (Eq,Ord, Show)
{-
data LiteralExpr2 = forall a.  Show a => LitVar2 Label a  IdentifierName
                    | forall a. Eq a => LitRecord2 Label (a -> a) IdentifierName RecordIndexing
                    | forall a. Eq a => LitArray2 Label (a -> a) IdentifierName (AExpr Int)
-}


deriving instance Eq a => Eq (AExpr a)
deriving instance Ord a => Ord (AExpr a)
deriving instance Show a => Show (AExpr a)

data BExpr a where
  BoolConst      ::Label  ->Bool ->   BExpr Bool
  Not      ::Label ->BExpr Bool->   BExpr Bool
  And::Label  ->BExpr Bool ->   BExpr Bool ->   BExpr Bool
  Or::Label  ->BExpr Bool ->   BExpr Bool ->   BExpr Bool
  Greater       ::Label -> AExpr Int ->  AExpr Int -> BExpr Bool
  Less  ::Label -> AExpr Int ->  AExpr Int -> BExpr Bool
  GreaterOrEqual  ::Label -> AExpr Int ->  AExpr Int -> BExpr Bool
  LessOrEqual    ::Label -> AExpr Int ->  AExpr Int -> BExpr Bool
  Equality   ::Label -> AExpr Int ->  AExpr Int -> BExpr Bool
  Different        ::Label -> AExpr Int ->  AExpr Int -> BExpr Bool

deriving instance Eq a => Eq (BExpr a)
deriving instance Ord a => Ord (BExpr a)
deriving instance Show a => Show (BExpr a)

data AExpr a where
  AInt      ::Int  -> AExpr Int
  ALExpr    ::LiteralExpr -> AExpr LiteralExpr
  Sum       ::AExpr Int -> AExpr Int -> AExpr Int
  Subtract  ::AExpr Int -> AExpr Int -> AExpr Int
  Multiply  ::AExpr Int -> AExpr Int -> AExpr Int
  Divide    ::AExpr Int -> AExpr Int -> AExpr Int
  Modulus   ::AExpr Int -> AExpr Int -> AExpr Int



-- Expressions

-- Declarations
newtype Size = Size Int
                deriving (Eq,Ord,Show)

data Declaration = DeclVar LiteralExpr --Int type
                    | DeclArray IdentifierName Size  --Int type
                    | DeclRecord IdentifierName RecordIndexing RecordIndexing
                    | Empty
                    | DeclSeq [Declaration]
                    deriving (Eq,Ord,Show)
-- Declarations

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
  --list <- sepBy1 statement' whiteSpace
-- If there's only one statement return it without using Seq.
  return $ if length list == 1 then head list else DeclSeq list

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

pAInteger :: Parser (AExpr Int)
pAInteger = AInt <$> integer
--  parseTest (pInteger :: Parser Expr) "AAAA[3]"
pLitVariable :: Parser LiteralExpr
pLitVariable = do
  var <- isVariable
  return (LitVar LabelNull (IdentifierName var))
--pLitVariable2 = (LitVar . LabelNull) <$> (IdentifierName <$> isVariable)
{-
pLitVariable = LitVar . IdentifierName <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable parsing")
-}

pAVariable :: Parser (AExpr LiteralExpr)
pAVariable = ALExpr <$> pLitVariable

pDeclVariable :: Parser Declaration
pDeclVariable = DeclVar <$> (symbol "int" *> pLitVariable)
-- parseTest ( spaceConsumer *> pDeclVariable <* eof) "int x"

pLitArray :: Parser LiteralExpr
pLitArray = do
  v <- (:) <$> letterChar <*> many letterChar <* lookAhead "["
  d <- brackets aOpExpression --wordP
  return (LitArray LabelNull (IdentifierName v) (AInt d))

pAArray :: Parser (AExpr LiteralExpr)
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
  return (DeclArray (IdentifierName var) (Size size))
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
    "fst" -> return (LitRecord LabelNull (IdentifierName varRec) First)
    "snd" -> return (LitRecord LabelNull (IdentifierName varRec) Second)

pARecord :: Parser (AExpr LiteralExpr)
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
  return (DeclRecord (IdentifierName var) First Second)
-- parseTest ( spaceConsumer *> pDeclRecord <* eof) " {int fst;int snd} R"
nOrSexpr :: Parser (Either (Expr Double) (Expr String))
nOrSexpr = (Left <$> nexpr) <|> (Right <$> sexpr)

aTerm :: Parser AExpr2
aTerm = choice
  [ parens aOpExpression
  , Wrap2 <$> try pAArray
  , Wrap2 <$> try pARecord
  , Wrap2 <$> pAVariable
  ]

data AExpr2 =  Wrap1 (AExpr Int)
              | Wrap2 (AExpr LiteralExpr)
--newtype AExpr3 =  AExpr3 (AExpr a)

{- |
--aTerm2 :: AExpr a -> Parser (AExpr a)
aTerm2 (parens aOpExpression) = parens aOpExpression
eval (Succ   t  ) = 1 + eval t
eval (IsZero t  ) = eval t == 0
eval (If b e1 e2) = if eval b then eval e1 else eval e2
eval (Pair e1 e2) = (eval e1, eval e2)
-}


aOpExpression :: Parser AExpr2
aOpExpression = ExprParser.makeExprParser aTerm aOperatorTable
-- parseTest (aOpExpression <* eof) "f*2 + 7+ A[5]*4"
aOperatorTable :: [[ExprParser.Operator Parser AExpr2]]
aOperatorTable =
  [ [ --ExprParser.Prefix (Negation <$ symbol "-")
     ExprParser.Prefix (id <$ symbol "+")]
  , [ ExprParser.InfixL (Wrap1 (Multiply <$ symbol "*"))
    , ExprParser.InfixL (Divide <$ symbol "/")
    , ExprParser.InfixL (Modulus <$ symbol "%")
    ]
  , [ ExprParser.InfixL (Sum <$ symbol "+")
    , ExprParser.InfixL (Subtract <$ symbol "-")
    ]
  ]

relation =
  (symbol ">" >> return Greater)
    <|> (symbol "<" >> return Less)
    <|> (symbol "<=" >> return LessOrEqual)
    <|> (symbol ">=" >> return GreaterOrEqual)
    <|> (symbol "==" >> return Equality)
    <|> (symbol "!=" >> return Different)

rExpression :: Parser (BExpr Bool)
rExpression = do
  a1 <- aOpExpression
  op <- relation
  a2 <- aOpExpression
  return $ op LabelNull a1 a2

bTerm :: Parser (BExpr Bool)
bTerm = choice
  [ try (parens bOpExpression)
  , try rExpression
  , reserved "true" >> return (BoolConst LabelNull True)
  , reserved "false" >> return (BoolConst LabelNull False)
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
bOpExpression :: Parser (BExpr Bool)
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

bOperatorTable :: [[ExprParser.Operator Parser (BExpr Bool)]]
bOperatorTable =
  [ [ExprParser.Prefix (Not LabelNull <$ symbol "!")]
  , [ExprParser.InfixL (And LabelNull <$ symbol "&&")]
  , [ExprParser.InfixL (Or LabelNull <$ symbol "||")]
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
  return $ if length list == 1 then head list else StmtSeq list

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
  var <- isVariable
  symbol ":="
  expr <- aOpExpression
  return (AssLit LitVar (LabelNull (IdentifierName var)) expr)

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
  return (AssLit varArr aExpr)

assignLitRecord :: Parser Statement
-- assignLitRecord = AssLit <$> pLitRecord <* symbol ":=" *> aOpExpression
assignLitRecord = do
  varRec <- pLitRecord
  symbol ":="
  aExpr <- aOpExpression
  symbol ";"
  return (AssLit varRec aExpr)

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
  return (AssRecord (IdentifierName var) expr1 expr2)
-- parseTest ( spaceConsumer *> recordStmt <* eof) "R:=(1+5,4+7)"
readStmt :: Parser Statement
readStmt =
  Read <$> (reserved "read" *> pLitVariable <|> pLitArray <|> pLitRecord)
readStmt2 = do
  reserved "read"
  var <- pLitVariable <|> pLitArray <|> pLitRecord
  return (Read var)

writeStmt :: Parser Statement
writeStmt = Write <$> (reserved "write" *> aOpExpression)
{- 
  writeStmt = do 
  reserved "write"
  var <- aOpExpression --evaluate
  return (Write var)
-}
skipStmt :: Parser Statement
skipStmt = Skip <$ reserved "skip"


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
