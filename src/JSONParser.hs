module JSONParser where

import Control.Applicative (Alternative, (<|>), empty, many, some)
import Control.Monad (join)
import Data.Char (isControl, digitToInt)
import Data.Functor ((<$))

import Parser

data Json
    = JsonString String
    | JsonNumber Int
    | JsonArray [Json]
    | JsonObject [(String, Json)]
    | JsonNull
    | JsonBool Bool
    deriving (Show)

class FromJson a where
    fromJson :: (Monad m) => String -> ParserT String m a

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

jsonString :: (Alternative m, Monad m) => ParserT String m Json
jsonString = JsonString <$> (within (string "\"") (string "\"") $ join <$> many (fmap pure jsonChar <|> jsonControlChar))

jsonNumber :: (Alternative m, Monad m) => ParserT String m Json
jsonNumber = JsonNumber .read <$> some digit

jsonArray :: (Alternative m, Monad m) => ParserT String m Json
jsonArray = JsonArray <$> (within (string "[" >> space) (space >> string "]") $ json `separatedBy` (space >> string "," >> space))

jsonObject :: (Alternative m, Monad m) => ParserT String m Json
jsonObject = within (string "{" >> space) (space >> string "}") $ JsonObject <$> keyValuePair `separatedBy` (space >> string "," >> space)
                where
                    keyValuePair = do
                        JsonString key <- jsonString
                        space
                        string ":"
                        space
                        (,) <$> pure key <*> json

jsonBool :: (Alternative m, Monad m) => ParserT String m Json
jsonBool = JsonBool . boolFromString <$> (string "true" <|> string "false")
                where
                    boolFromString "true"  = True
                    boolFromString "false" = False
                    boolFromString _       = error "Bad Boolean!!!"

jsonNull :: (Alternative m, Monad m) => ParserT String m Json
jsonNull = JsonNull <$ string "null"

json :: (Alternative m, Monad m) => ParserT String m Json
json =  jsonObject
    <|> jsonArray
    <|> jsonString
    <|> jsonNumber
    <|> jsonBool
    <|> jsonNull
