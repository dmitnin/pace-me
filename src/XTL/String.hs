module XTL.String (packString, unpackString) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

packString :: String -> BS.ByteString
packString = TE.encodeUtf8 . T.pack

unpackString :: BS.ByteString -> String
unpackString = T.unpack . TE.decodeUtf8
