module Main where

import Data.Char (isPrint)
import Data.Text as T (Text, filter, length, pack)
import Data.Text.Encoding as T (decodeUtf8, encodeUtf8)

countNonPrintableCharacters :: String -> Int
countNonPrintableCharacters = Prelude.length . Prelude.filter (not . Data.Char.isPrint)

countNonPrintableCharactersInText :: Text -> Int
countNonPrintableCharactersInText = T.length . T.filter (not . Data.Char.isPrint) . T.decodeUtf8 . T.encodeUtf8

countNonPrintableCharactersStringAndText :: String -> (Int, Int)
countNonPrintableCharactersStringAndText input =
  (countNonPrintableCharacters input, countNonPrintableCharactersInText $ pack input)

main :: IO ()
main = do
  print $ countNonPrintableCharactersStringAndText "\v\t\aHello\r\n"