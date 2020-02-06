module ParserHelper where

import           RIO
import           Control.Monad                  ( void )
import           Text.Parsec                    ( manyTill )
import           Text.Parsec.Combinator         ( notFollowedBy )
import           Text.Parsec.Prim               ( Stream
                                                , ParsecT
                                                )
import           Text.Parsec.Char               ( satisfy )

-- |
many1Till
    :: forall s u m a t end
     . (Stream s m t, Show end)
    => ParsecT s u m a
    -> ParsecT s u m end
    -> ParsecT s u m [a]
many1Till p end = do
    notFollowedBy end
    first <- p
    rest  <- manyTill p end
    pure (first : rest)

-- | Match zero or more whitespace characters.
whiteSpace :: forall m . Monad m => ParsecT String () m String
whiteSpace =
    many $ satisfy (\c -> c == '\n' || c == '\r' || c == ' ' || c == '\t')

-- | Skip whitespace characters.
skipSpaces :: forall m . Monad m => ParsecT String () m ()
skipSpaces = void whiteSpace
