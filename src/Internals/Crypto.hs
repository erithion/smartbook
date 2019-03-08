{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}
module Internals.Crypto
( encrypt
, decrypt
)
where

import Data.Aeson
import Crypto.Cipher.AES
import Data.Tuple.Extra                     ( (&&&) )
import Control.Applicative                  ( (<$>), (<*>), (<|>) )
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Base64     as B64

-- They say that Java's Cipher.getInstance("AES") by default uses Cipher.getInstance("AES/ECB/PKCS5Padding");
aes :: AES
aes = initAES ("sbsecretsbsecret" :: BS.ByteString)

decrypt :: FromJSON a => BS.ByteString -> Either String a
decrypt b64 = do
    plain <- B64.decode b64
    let dec = removePadding . decryptECB aes $ plain
    eitherDecodeStrict dec
    where removePadding :: BS.ByteString -> BS.ByteString
          removePadding bs
            | BS.null bs        = bs
            | BS.last bs <= 16  = let count = fromIntegral . BS.last $ bs
                                      suffix = BS.replicate count (toEnum count) 
                                  in case BS.stripSuffix suffix bs of
                                            -- Padding found
                                            Just xs -> xs
                                            -- There were no repeating bytes hence no padding
                                            Nothing -> bs
            | otherwise         = bs

encrypt :: ToJSON a => a -> BS.ByteString          
encrypt ob = B64.encode . encryptECB aes $
                    (uncurry BS.append . (id &&& paddingArray . BS.length) . BSL.toStrict . encode $ ob)
    where 
        -- for AES ECB the data must be of the multiple 16 size
        paddingArray :: Int -> BS.ByteString
        paddingArray size = 
            let count = case mod size 16 of 
                                0 -> 16
                                x -> fromIntegral $ 16 - x
            --  Padding: The value of each added byte is the number of bytes that are added, for example 03 03 03 or 04 04 04 04
            in BS.replicate count (toEnum count)