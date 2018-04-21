module JSONParser where

import Control.Applicative (Alternative, (<|>), empty, many)
import Control.Monad (join)
import Data.Char (isControl)

import Parser

jsonChar :: (Alternative m, Monad m) => ParserT String m Char
jsonChar = satisfy (\c -> not $ isControl c || (c == '\\') || (c == '"'))

hexDigit :: (Alternative m, Monad m) => ParserT String m Char
hexDigit = digit <|> foldl (<|>) empty (char <$> ['A'..'F'])

jsonControlChar :: (Alternative m, Monad m) => ParserT String m String
jsonControlChar = do
    slash <- char '\\'
    let chars = foldl (<|>) empty (string <$> ["\"", "\\", "/", "b", "f", "n", "r", "t"])
        unicodeCodePoint = do
            u <- char 'u'
            res <- repeatN 4 hexDigit
            pure $ u : res
    match <- chars <|> unicodeCodePoint
    pure $ slash : match

jsonString :: (Alternative m, Monad m) => ParserT String m String
jsonString = within (string "\"") (string "\"") $ join <$> many (string "" <|> fmap pure jsonChar <|> jsonControlChar)