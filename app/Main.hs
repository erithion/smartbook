{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Smartbook
import System.Environment (getArgs)
import System.IO
import qualified Data.Text.Lazy.IO          as TexL
import qualified Data.Text.Lazy             as TexL
import qualified Data.Text.Lazy.Encoding    as TexL
import qualified Data.ByteString            as BS
import GHC.IO.Encoding                      ( utf8, setLocaleEncoding )
import qualified Data.Aeson.Text            as Aeson 

{-
dispatch :: [String] -> IO ()
dispatch ("encrypt":fileIn:fileOut:[]) = encryptWrap fileIn fileOut
dispatch ("decrypt":fileIn:fileOut:[]) = decryptWrap fileIn fileOut
dispatch ("bask":[]) = baskwrap
dispatch _ = putStrLn "Wrong arguments!"


decryptWrap :: String -> String -> IO ()
decryptWrap fileIn fileOut = do
    input <- BS.readFile fileIn
    let decrypted :: Either String Book = decrypt input
    flip (either (\x-> System.IO.putStrLn ("Base64.decode failed with " ++ show x) )) decrypted $ \dt-> 
        do
            setLocaleEncoding utf8
            withBinaryFile fileOut WriteMode $ \handle ->
                             TexL.hPutStr handle (Aeson.encodeToLazyText dt)
            return ()        
    
encryptWrap :: String -> String -> IO ()
encryptWrap fileIn fileOut = 
      do
        setLocaleEncoding utf8
        input <- TexL.readFile fileIn
        let base64Content = encrypt input

        withBinaryFile fileOut WriteMode $ \handle ->
                                BS.hPutStr handle base64Content
        return ()        

    
--enFile = "F:/tem1.txt"
--noFile = "F:/tem2.txt"

noFile = "F:/The Hound of the Baskervilles2.txt"
enFile = "F:/Conan Doyle, Arthur - Hunden fra Baskerville2.txt"




baskwrap = do
    setLocaleEncoding utf8
    en <- TexL.readFile enFile
    ru <- TexL.readFile noFile
    let name = "a9.sb"
    let enc = encrypt $ build en ru name
    withBinaryFile ("./" ++ TexL.unpack name) WriteMode $ \handle ->
                                BS.hPutStr handle enc
    
        
       



main :: IO ()
main = do
    args <- getArgs
    dispatch args

-}
main = return ()
