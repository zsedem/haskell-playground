{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module JSON.Type(JValue(..), JSONSerializable(..), isNull, dumpJSON) where
import Data.Map(Map, fromList, toList)
import Control.Monad(liftM)
import GHC.Float

data JValue = JString (String)
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Show)

isNull::JValue -> Bool
isNull JNull = True
isNull _ = False

class JSONSerializable t where
  toJSON::t -> JValue
  loadJSON::JValue -> Either String t

instance JSONSerializable JValue where
  toJSON = id
  loadJSON = return


instance JSONSerializable Integer where
  toJSON k = JNumber $ fromIntegral k
  loadJSON = getInt
    where getInt (JNumber n) = Right (truncate n)
          getInt s           = Left $ "Could Not convert " ++ show s ++ " to Int"

instance JSONSerializable Int where
  toJSON k = JNumber $ fromIntegral k
  loadJSON = getInt
    where getInt (JNumber n) = Right (truncate n)
          getInt s           = Left $ "Could Not convert " ++ show s ++ " to Int"


instance JSONSerializable Float where
  toJSON k = JNumber $ float2Double k
  loadJSON (JNumber n) = Right (double2Float n)
  loadJSON s           = Left $ "Could Not convert " ++ show s ++ " to Int"

instance JSONSerializable Double where
  toJSON = JNumber
  loadJSON (JNumber n) = Right n
  loadJSON s           = Left $ "Could Not convert " ++ show s ++ " to Double"

instance JSONSerializable Char where
  toJSON k = JString [k]
  loadJSON (JString [c]) = return c
  loadJSON s = Left $ "Could Not convert " ++ show s ++ " to Char"


instance JSONSerializable String where
  toJSON   = JString
  loadJSON (JString s) = Right s
  loadJSON s           = Left $ "Could Not convert " ++ show s ++ " to String"


instance JSONSerializable Bool where
  toJSON = JBool
  loadJSON = getBool
    where getBool (JBool b) = Right b
          getBool s         = Left $ "Could Not convert " ++ show s ++ " to Bool"

instance JSONSerializable a => JSONSerializable [(String, a)] where
  toJSON keyValueMap = JObject $ map sndToJSON keyValueMap
    where sndToJSON (k,s) = (k,toJSON s)
  loadJSON jObject = do
      jsonDict <- getObject jObject
      eitherMap sndLoadJSON jsonDict
      where sndLoadJSON (k,s) = do s' <- loadJSON s
                                   return (k, s')

            getObject (JObject o) = Right o
            getObject s           = Left $ "Could Not convert " ++ show s ++ "to [String, a]"

instance JSONSerializable a => JSONSerializable (Maybe a) where
  toJSON (Just value) = toJSON value
  toJSON Nothing = JNull
  loadJSON JNull = return Nothing
  loadJSON jValue = Just `liftM` loadJSON jValue

instance JSONSerializable a => JSONSerializable (Map String a) where
  toJSON keyValueMap = JObject $ map sndToJSON $ toList keyValueMap
    where sndToJSON (k,s) = (k,toJSON s)
  loadJSON jObject = do
      jsonDict <- getObject jObject
      pairlist <- eitherMap sndLoadJSON jsonDict
      return $ fromList pairlist
      where sndLoadJSON (k,s) = do s' <- loadJSON s
                                   return (k, s')

            getObject (JObject o) = Right o
            getObject s           = Left $ "Could Not convert " ++ show s ++ "to Map String a"

instance (JSONSerializable a) => JSONSerializable [a] where
  toJSON l = JArray $ map toJSON l
  loadJSON l = do list <- getArray l
                  eitherMap loadJSON list
    where getArray (JArray a) = Right a
          getArray s          = Left $ "Could Not convert " ++ show s ++ "to [a]"

dumpJSON::JValue -> String
dumpJSON (JString a) = show a
dumpJSON (JNumber a) = show a
dumpJSON JNull = "null"
dumpJSON (JBool a) = show a
dumpJSON (JArray x) = "[" ++ foldl (+|+) "" (map dumpJSON x) ++ "]"
dumpJSON (JObject x) = "{" ++ foldl (+|+) "" (map dumpkeyvalue x) ++ "}"
  where dumpkeyvalue (key, value) = show key ++ ":" ++ dumpJSON value

(+|+):: String -> String -> String
"" +|+ b = b
a +|+ b = a ++ "," ++ b


eitherMap:: (a->Either x b) -> [a] -> Either x [b]
eitherMap f (x:xs) = do x' <- f x
                        liftM (x':) $ eitherMap f xs
eitherMap _ [] = return []
