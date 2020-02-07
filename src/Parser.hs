module Parser where

import           Ast                            ( Annotation(..)
                                                , Document(..)
                                                , Expr(..)
                                                , Path(..)
                                                , PathElement(..)
                                                , Spec(..)
                                                , Symbol(..)
                                                , Term(..)
                                                )
import           Control.Monad                  ( void )
import           Data.Functor                   ( ($>) )
import qualified Data.Set                      as Set
import           Language                       ( lexer )
import           ParserHelper                   ( many1Till
                                                , skipSpaces
                                                )
import           RIO                     hiding ( try
                                                , (<|>)
                                                )
import           RIO.Text                       ( pack
                                                , strip
                                                , unpack
                                                )
import           Text.Parsec                    ( Parsec
                                                , anyChar
                                                , char
                                                , eof
                                                , manyTill
                                                , optionMaybe
                                                , string
                                                , try
                                                , (<|>)
                                                )
import           Text.Parsec.Expr               ( Assoc(AssocRight)
                                                , Operator(Infix)
                                                , buildExpressionParser
                                                )
import           Text.Parsec.Token              ( GenTokenParser
                                                    ( braces
                                                    , commaSep
                                                    , identifier
                                                    , natural
                                                    , parens
                                                    , stringLiteral
                                                    , symbol
                                                    )
                                                , reserved
                                                )

newline :: Parsec String () ()
newline = lf <|> crlf <|> eof
  where
    lf   = void (string "\n")
    crlf = void (string "\r\n")

document :: Parsec String () Document
document = do
    reserved lexer "spec"
    name <- identifier lexer
    skipSpaces
    terms <- many term
    skipSpaces
    specs <- many1Till (skipSpaces *> api) eof
    pure $ Document { docName = pack name, docTerms = terms, docSpecs = specs }

annotation :: Parsec String () Annotation
annotation = do
    h <- line
    t <- many line
    pure $ Annotation ((pack . unlines) (h : t))
  where
    line :: Parsec String () String
    line = char '#' *> (manyTill anyChar newline >>= \x -> pure (trim x))

    trim :: String -> String
    trim = unpack . strip . pack

term :: Parsec String () Term
term = do
    string "term" *> skipSpaces
    name <- skipSpaces *> stringLiteral lexer
    string "description" *> skipSpaces
    desc <- skipSpaces *> stringLiteral lexer
    string "synonyms" *> skipSpaces
    synonyms <- (braces lexer . commaSep lexer . stringLiteral) lexer
    pure $ Term { termName     = pack name
                , termDesc     = pack desc
                , termSynonyms = fmap pack synonyms
                }

expr :: Parsec String () Expr
expr = try binop <|> value
  where
    binop :: Parsec String () Expr
    binop = buildExpressionParser [[Infix impl AssocRight]] value

    impl :: Parsec String () (Expr -> Expr -> Expr)
    impl = do
        skipSpaces
        op <-
            string "=>"
            <|> string "="
            <|> string "::"
            <|> string ">"
            <|> string "<"
            <|> string ">="
            <|> string "<="
        skipSpaces
        pure $ BinOp (pack op)

value :: Parsec String () Expr
value =
    boolean
        <|> string
        <|> number
        <|> try pair
        <|> set
        <|> try call
        <|> lambda
        <|> parens lexer expr
  where
    boolean :: Parsec String () Expr
    boolean = t <|> f
      where
        t = symbol lexer "true" $> VBool True
        f = symbol lexer "false" $> VBool False

    string :: Parsec String () Expr
    string = do
        s <- stringLiteral lexer
        pure $ VText (pack s)

    number :: Parsec String () Expr
    number = do
        n <- natural lexer
        pure $ VNat n

    pair :: Parsec String () Expr
    pair = do
        k <- (identifier lexer) >>= \x -> pure (Symbol (pack x))
        char ':' *> skipSpaces
        v <- expr
        pure $ VPair k v

    set :: Parsec String () Expr
    set = do
        xs <- braces lexer (commaSep lexer expr)
        pure $ VSet (Set.fromList xs)

    call :: Parsec String () Expr
    call = do
        name <- identifier lexer
        skipSpaces
        args <- many value
        pure $ Call (pack name) args

    lambda :: Parsec String () Expr
    lambda = do
        symbol lexer "\\" *> skipSpaces
        n <- identifier lexer
        symbol lexer "->" *> skipSpaces
        e <- expr
        pure $ Lambda (Symbol (pack n)) e

path :: Parsec String () Path
path = do
    skipSpaces
    h <- element
    t <- many element
    pure $ Path (h : t)
  where
    element :: Parsec String () PathElement
    element = do
        _ <- symbol lexer "/"
        static <|> assignable

    assignable :: Parsec String () PathElement
    assignable = do
        char ':' *> skipSpaces
        name <- identifier lexer >>= \x -> pure (Symbol (pack x))
        pure $ PAssignable name

    static :: Parsec String () PathElement
    static = do
        name <- identifier lexer
        pure $ PStatic (pack name)

api :: Parsec String () Spec
api = do
    a     <- optionMaybe (skipSpaces *> annotation)
    m     <- skipSpaces *> method
    p     <- skipSpaces *> path
    query <- skipSpaces *> string "query" *> skipSpaces *> expr
    pre   <- skipSpaces *> string "pre" *> skipSpaces *> expr
    post  <- skipSpaces *> string "post" *> skipSpaces *> expr
    pure Api { apiAnnotation = a
             , apiMethod     = pack m
             , apiPath       = p
             , apiQuery      = query
             , apiPre        = pre
             , apiPost       = post
             }
  where
    method =
        string "get" <|> string "post" <|> string "put" <|> string "delete"
