module Ast where

import Data.Set (Set)
import Data.Text (Text)

newtype Symbol = Symbol Text deriving (Show, Eq, Ord)

data Document = Document
    { docName :: Text
    , docSpecs :: [Spec]
    , docTerms :: [Term]
    }
    deriving (Show)

newtype Annotation = Annotation Text deriving (Show)

data Term = Term
    { termName :: Text
    , termDesc :: Text
    , termSynonyms :: [Text]
    }
    deriving (Show)

data Expr
    = VBool Bool
    | VText Text
    | VNat Integer
    | VPair Symbol Expr
    | VSet (Set Expr)
    | Call Text [Expr]
    | BinOp Text Expr Expr
    | Lambda Symbol Expr
    deriving (Show, Eq, Ord)

data PathElement = PStatic Text | PAssignable Symbol deriving (Show)

newtype Path = Path [PathElement] deriving (Show)

data Spec = Api
    { apiAnnotation :: Maybe Annotation
    , apiMethod :: Text
    , apiPath :: Path
    , apiQuery :: Expr
    , apiPre :: Expr
    , apiPost :: Expr
    }
    deriving (Show)
