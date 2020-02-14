module Language
    ( lexer
    )
where

import           Data.Char                      ( isAlpha )
import           Text.Parsec                    ( oneOf
                                                , (<|>)
                                                )
import           Text.Parsec.Char               ( alphaNum
                                                , satisfy
                                                )
import           Text.Parsec.Token              ( GenLanguageDef(..)
                                                , LanguageDef
                                                , TokenParser
                                                , makeTokenParser
                                                )

golazoStyle :: LanguageDef ()
golazoStyle = LanguageDef
    { commentStart    = ""
    , commentEnd      = ""
    , commentLine     = ""
    , nestedComments  = False
    , identStart      = satisfy isAlpha
    , identLetter     = alphaNum <|> oneOf ['_']
    , opStart         = oneOf opChars
    , opLetter        = oneOf opChars
    , reservedNames   = ["spec", "using", "term", "description", "synonyms"]
    , reservedOpNames = fmap charToString opChars <> ["=>"]
    , caseSensitive   = True
    }
  where
    opChars      = ['+', '-', '*', '/', '=', '>']
    charToString = (: [])

lexer :: TokenParser ()
lexer = makeTokenParser golazoStyle
