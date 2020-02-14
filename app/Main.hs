module Main
    ( main
    )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.List.Safe                 ( head )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Parser                         ( document )
import           Prelude                 hiding ( head )
import           System.Environment             ( getArgs )
import           System.IO                      ( readFile )
import           Text.Parsec                    ( runParser )

main :: IO ()
main = do
    args <- liftIO getArgs
    case head args of
        Just f -> do
            x <- readFile f
            print (exec (pack x))
        Nothing -> print "no file"
  where
    exec :: Text -> Text
    exec code = case runParser document () "" (unpack code) of
        Left  e -> pack (show e)
        Right s -> pack (show s)
