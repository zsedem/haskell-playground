module JSON(JValue(..), JSONSerializable(..), isNull, prettifiedJSONString) where
import JSON.Type(JValue(..), JSONSerializable(..), isNull)
import JSON.Parser
import JSON.Prettify

prettifiedJSONString :: String -> String
prettifiedJSONString str = either show prettyJSON $ parseJSON str
