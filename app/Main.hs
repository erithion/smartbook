{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TemplateHaskell #-}
module Main where

import Prelude                       hiding (readFile)
import Data.List
import Data.Monoid
import Options.Applicative
import System.IO                            (withBinaryFile, IOMode(..))
import Data.Text.Lazy.IO as T               (readFile)
import GHC.IO.Encoding                      (utf8, setLocaleEncoding)
import Data.Aeson                           (eitherDecode, encode)
import Data.ByteString.Lazy as BL           (toStrict, readFile, hPutStr)
import Data.ByteString as B                 (hPutStr, ByteString, readFile)

import Smartbook

data AppOptions
  = Build { inputFile :: String
          , title :: String
          , author :: String
          , doEncypt :: Bool
          , outputFile :: String }
  | Encrypt { inputFile :: String 
            , outputFile :: String }
  | Decrypt { inputFile :: String 
            , outputFile :: String }
  deriving (Eq, Show)

buildOptions :: Parser AppOptions
buildOptions = Build 
    <$> strOption 
         (  long "file"
         <> short 'f'
         <> metavar "FILE"
         <> help "Bilingual book in txt format" )
    <*> strOption
         (  long "title"
         <> short 't'
         <> metavar "TITLE"
         <> help "Book title" )
    <*> strOption
         (  long "author"
         <> short 'a'
         <> metavar "AUTHOR"
         <> help "Book author" )
    <*> option auto
         ( long "encrypt"
         <> short 'e'
         <> help "Encrypt the resulting SB file"
         <> showDefault
         <> value True
         <> metavar "BOOL" )
    <*> strOption
         (  long "out"
         <> short 'o'
         <> metavar "FILE"
         <> value "out.sb"
         <> help "Resulting SB book" )

cipherOptions :: Parser (String -> String -> AppOptions) -> Parser AppOptions
cipherOptions v = v 
    <*> strOption 
         (  long "file"
         <> short 'f'
         <> metavar "FILE"
         <> help "Input file to process" )
    <*> strOption
         (  long "out"
         <> short 'o'
         <> metavar "FILE"
         <> help "Resulting file" )

appOptions :: Parser AppOptions
appOptions = subparser
       ( command "build"
         (info buildOptions
               (progDesc "Create a smartbook from a bilingual txt file"))
      <> command "encrypt"
         (info (cipherOptions $ pure Encrypt)
               (progDesc "Encrypts an SB JSON file using Smartbook's encryption"))
      <> command "decrypt"
         (info (cipherOptions $ pure Decrypt)
               (progDesc "Decrypts a file using Smartbook's decryption"))
       )

run :: AppOptions -> IO ()
run v@(Build _ _ _ _ _) = do
    setLocaleEncoding utf8
    input <- T.readFile $ inputFile v
    let book = exampleComposeBook (enruBoilerplate (outputFile v) (title v) (author v)) 
                                            (plainChapters ["kapitel", "chapter"] input)
    let bytes :: ByteString = 
            if (doEncypt v) 
            then encrypt book
            else toStrict . encode $ book
    withBinaryFile (outputFile v) WriteMode $ 
            \handle -> B.hPutStr handle bytes

run (Encrypt inFile outFile) = do
    input <- BL.readFile inFile
    case (eitherDecode input :: Either String Book) of
        Right book -> do
                let bytes = encrypt book
                withBinaryFile outFile WriteMode $ 
                        \handle -> B.hPutStr handle bytes
        Left s -> putStrLn s

run (Decrypt inFile outFile) = do
    input <- B.readFile inFile
    case (decrypt input :: Either String Book) of
        Right book  -> withBinaryFile outFile WriteMode $ 
                            \handle -> BL.hPutStr handle $ encode book
        Left s -> putStrLn s
            
opts :: ParserInfo AppOptions
opts = info (appOptions <**> helper) idm

main :: IO ()
main = execParser opts >>= run