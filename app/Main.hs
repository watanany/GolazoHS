module Main where

import           Data.List.Safe                 ( head )
import           Parser                         ( document )
import           RIO
import           RIO.Text                       ( unpack )
import           System.Environment             ( getArgs )
import           Text.Parsec                    ( runParser )

main :: IO ()
main = runSimpleApp app

app :: RIO SimpleApp ()
app = do
    args <- liftIO getArgs
    case head args of
        Just f -> do
            x <- readFileUtf8 f
            logInfo (display (exec x))
        Nothing -> logInfo "no file"
  where
    exec :: Text -> Text
    exec code = case runParser document () "" (unpack code) of
        Left  e -> tshow e
        Right s -> tshow s
