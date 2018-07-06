module Main where

import Parser (runParser)
import JSONParser (json)

main :: IO ()
main = do
  zips <- readFile "./json/zips.json"
  zipsJson <- runParser zips json
  print zipsJson
