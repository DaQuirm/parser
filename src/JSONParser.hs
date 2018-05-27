module JSONParser where

import Control.Applicative (Alternative, (<|>), empty, many)
import Control.Monad (join)
import Data.Char (isControl, digitToInt)
import Data.Functor ((<$))

import Parser

data Json
    = JString String
    | JNumber Int
    | JArray [Json]
    | JObject [(String, Json)]
    | JNull
    | JBool Bool
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
jsonString = JString <$> (within (string "\"") (string "\"") $ join <$> many (string "" <|> fmap pure jsonChar <|> jsonControlChar))

jsonNumber :: (Alternative m, Monad m) => ParserT String m Json
jsonNumber = JNumber . digitToInt <$> digit

jsonArray :: (Alternative m, Monad m) => ParserT String m Json
jsonArray = JArray <$> (within (string "[") (string "]") $ separatedBy jsonValue (string ","))

jsonObject :: (Alternative m, Monad m) => ParserT String m Json
jsonObject = within (string "{") (string "}") $ JObject <$> separatedBy parseKeyValue (string ",")
                where
                    parseKeyValue = do
                        key <- many jsonChar
                        string ":"
                        (,) <$> pure key <*> jsonValue

jsonBool :: (Alternative m, Monad m) => ParserT String m Json
jsonBool = JBool . boolFromString <$> (string "true" <|> string "false")
                where
                    boolFromString "true"  = True
                    boolFromString "false" = False
                    boolFromString _       = error "Bad Boolean!!!"

jsonNull :: (Alternative m, Monad m) => ParserT String m Json
jsonNull = JNull <$ string "null"

jsonValue :: (Alternative m, Monad m) => ParserT String m Json
jsonValue
      = jsonString
    <|> jsonNumber
    <|> jsonObject
    <|> jsonArray
    <|> jsonBool
    <|> jsonNull


jsonParser :: (Alternative m, Monad m) => ParserT String m Json
jsonParser = jsonArray <|> jsonObject