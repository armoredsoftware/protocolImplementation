module ByteStringJSON where

import Data.Aeson
import qualified Data.Aeson as DA
import qualified Data.ByteString as B (ByteString)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import Control.Applicative
import Data.ByteString.Lazy(ByteString, empty, append, pack, toStrict, fromStrict)

instance ToJSON B.ByteString where
	toJSON = DA.String . encodeToText
instance FromJSON B.ByteString where
	parseJSON (DA.String str) = pure $ decodeFromText str	
				 		         
encodeToText :: B.ByteString -> T.Text
encodeToText = TE.decodeUtf8 . Base64.encode

decodeFromText :: T.Text -> B.ByteString
decodeFromText = {-either fail return .-} Base64.decodeLenient . TE.encodeUtf8

decodeFromTextL :: (Monad m) => T.Text -> m ByteString
decodeFromTextL x = let bs = decodeFromText x in
		       return (fromStrict bs)  

decodeFromTextLStayStrict :: (Monad m) => T.Text -> m B.ByteString
decodeFromTextLStayStrict x = let bs = decodeFromText x in
		       return (bs)  


decodeFromTextL' :: T.Text -> ByteString
decodeFromTextL' x = let bs = decodeFromText x in
		       fromStrict bs  