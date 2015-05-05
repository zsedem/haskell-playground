module JSON.Prettify(toString, prettyJSON) where

import JSON.Type

toString :: JValue -> String
toString (JObject obj) = showListLike ('{','}') obj $ \(key, value) ->'"':key ++ "\":" ++ toString value
toString (JArray array) = showListLike ('[',']') array toString
toString (JString string) = show string
toString (JBool True) = "\"true\""
toString (JBool False) = "\"false\""
toString (JNumber num) = properShow num
toString JNull = "null"

showListLike :: (Char, Char) -> [a] -> (a -> String) -> String
showListLike (open, close) list itemToString = open :showListLike' list ++ [close]
    where showListLike' [] = ""
          showListLike' (firstitem:restitems) =
                itemToString firstitem ++
                foldr (\item result ->"," ++ itemToString item ++ result) "" restitems

prettyJSON :: JValue -> String
prettyJSON = prettyJSON' 0

prettyJSON' :: Int -> JValue -> String
prettyJSON' k (JArray x)  = "[\n"
                            ++ foldl (+|+) "" (map prettyJSONvalue x) ++ "\n"
                            ++ space k ++ "]"
  where prettyJSONvalue value = space (k + 4)  ++ prettyJSON' (k + 4) value
prettyJSON' k (JObject x)  = "{\n"
                             ++ foldl (+|+) "" (map prettyJSONkeyvalue x) ++ "\n"
                             ++ space k ++ "}"
  where prettyJSONkeyvalue (key, value) = space (k + 4) ++ show key ++ ": " ++ prettyJSON' (k + 4) value

prettyJSON' _ (JString value)  = show value
prettyJSON' _ (JBool value)  = show value
prettyJSON' _ (JNumber value)  = properShow value
prettyJSON' _ JNull  = "null"

properShow :: Double -> String
properShow value = if fraction == 0 then show k else show value
    where  k::Integer
           (k, fraction) = properFraction value

(+|+):: String -> String -> String
"" +|+ b = b
a +|+ b = a ++ ",\n" ++ b

space :: Int -> String
space k = replicate k ' '
