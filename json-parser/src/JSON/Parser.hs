module JSON.Parser(parseJSON) where
import Text.ParserCombinators.Parsec
import JSON.Type
import Control.Monad
import Numeric (readHex, readFloat, readSigned)
import Control.Applicative((*>), (<$>), (<$))



parseJSON :: String -> Either ParseError JValue
parseJSON = parse aJSONFile "JSONParseError:"

aJSONFile :: GenParser Char () JValue
aJSONFile = do spaces
               result <- JObject <$> jsobject <|> JArray <$> jslist
               spaces
               return result

jslist::GenParser Char () [JValue]
jslist = surroundedBy ('[',',',']') jsvalue

jsbool :: GenParser Char () Bool
jsbool = false <|> true
  where false = string "false" >> return False
        true  = string "true" >> return True

jsobject::GenParser Char () [(String, JValue)]
jsobject = surroundedBy ('{',',','}') keyvalue
  where
      keyvalue = do key <- quotedString
                    spaces
                    dropchar ':'
                    spaces
                    value <- jsvalue
                    return (key,value)

jsvalue::GenParser Char () JValue
jsvalue = jsnull <|>
          JBool <$> jsbool <|>
          JNumber <$> jsnumber <|>
          JString <$> quotedString <|>
          JObject <$> jsobject <|>
          JArray <$> jslist

jsnumber :: GenParser Char () Double
jsnumber = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> mzero

jsnull::GenParser Char () JValue
jsnull = do void $ string "null"
            return JNull

quotedString::GenParser Char () String
quotedString = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> (escape <|> unicode)
              <|> noneOf "\"\\"
          escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
               where decode c r = r <$ char c

unicode :: GenParser Char () Char
unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = readHex x

dropchar:: Char -> GenParser Char () ()
dropchar c = void $ char c

surroundedBy :: (Char,Char,Char) -> GenParser Char () a -> GenParser Char () [a]
surroundedBy (openBracket, seperator, closeBracket) item = do
              dropchar openBracket
              spaces
              list <- manySeperatedBy seperator item <|> return []
              spaces
              dropchar closeBracket
              return list

manySeperatedBy::Char -> GenParser Char () a -> GenParser Char () [a]
manySeperatedBy seperator item = do
     parsedItem <- item
     spaces
     (parsedItem:) `liftM` many ( do dropchar seperator
                                     spaces
                                     result <- item
                                     spaces
                                     return result)
