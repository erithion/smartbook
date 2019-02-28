{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Prelude                                       hiding (readFile)
import           Paths_smartbook                            (getDataFileName)
import           Data.Aeson                                 (eitherDecode, encode)
import           Test.Hspec
import           Data.Text.Lazy.IO                          (readFile)
import           Data.Text.Lazy                             (Text)
import           Data.Text.Lazy.Encoding                    (encodeUtf8)
import           System.FilePath                            ((</>))
import           GHC.IO.Encoding                            (utf8, setLocaleEncoding)
import           Data.Map                                   (fromList)

import           Smartbook

dataFile :: String -> IO Text
dataFile name = do
    setLocaleEncoding utf8
    fileIn <- getDataFileName ("test" </> name)
    readFile fileIn

main :: IO ()
main = do
    bilData <- dataFile "bkBilingual.txt"
    let book = Book { _bookLang = "en"
                            , _bookName = ""
                            , _bookRusName = "" 
                            , _bookAuthor = "" 
                            , _bookHash = ""
                            , _bookThumbnail = ""
                            , _bookFilename = "file" 
                            , _bookSize = 0 
                            , _bookLangs = 
                                [ Version { _versionName = "title"
                                          , _versionLang = "en"
                                          , _versionTranslation = Nothing
                                          , _versionTranslationSize = Nothing
                                          , _versionAuthor = Just "author"
                                          },
                                  Version {_versionName = ""
                                          , _versionLang = "ru"
                                          , _versionTranslation = Just ""
                                          , _versionTranslationSize = Just 0
                                          , _versionAuthor = Nothing
                                          }
                                ]
                            , _bookChapters = 
                                [Leaf      { chapterName = ""
                                           , chapterDescription = Nothing
                                           , paragraphs = 
                                                [fromList [("en",""),("ru","")]]
                                           }
                                ,Leaf      { chapterName = "Chapter 1"
                                           , chapterDescription = Nothing
                                           , paragraphs = 
                                                [fromList [("en","Chapter 1"),("ru","Første kapitel.")]
                                                ,fromList [("en","Mr. Sherlock Holmes"),("ru","Sherlock Holmes.")]
                                                ,fromList [("en","Mr. Sherlock Holmes, who was usually very late in the mornings, save upon those not infrequent occasions when he was up all night, was seated at the breakfast table. I stood upon the hearth-rug and picked up the stick which our visitor had left behind him the night before. It was a fine, thick piece of wood, bulbous-headed, of the sort which is known as a \"Penang lawyer.\" Just under the head was a broad silver band nearly an inch across. \"To James Mortimer, M.R.C.S., from his friends of the C.C.H.,\" was engraved upon it, with the date \"1884.\" It was just such a stick as the old-fashioned family practitioner used to carry—dignified, solid, and reassuring."),("ru","HOLMES, som vanligvis var meget sent oppe om morgenen, untatt i de ikke sjeldne tilfellene da han var oppe hele natten — satt ved frokostbordet.Jeg stod på kaminteppet og tok opp stokken som vår gjest den foregående aften hadde etterlatt seg. Den var forarbeidet av vakkert, fast tre, og hadde et løkformet hode. Like under håndtaket gikk et nesten tommebredt sølvbånd.")]
                                                ,fromList [("en","..."),("ru","...")]]
                                           }
                                ,Leaf      { chapterName = "Chapter 2"
                                           , chapterDescription = Nothing
                                           , paragraphs = 
                                                [fromList [("en","Chapter 2"),("ru","Andet kapitel.")]
                                                ,fromList [("en","The Curse of the Baskervilles"),("ru","Baskervilles forbandelse")]
                                                ,fromList [("en","\"I have in my pocket a manuscript,\" said Dr. James Mortimer."),("ru","“JEG har et manuskript i lommen,” sa doktor James Mortimer.")]
                                                ,fromList [("en","\"I observed it as you entered the room,\" said Holmes."),("ru","“Det la jeg merke til da De kom inn,” svarte Holmes.")]
                                                ,fromList [("en","\"It is an old manuscript.\""),("ru","“Det er et gammelt manuskript —”")]
                                                ,fromList [("en","\"Early eighteenth century, unless it is a forgery.\""),("ru","“Fra begynnelsen av det attende århundrede, hvis det ikke er et falskneri.”")]
                                                ,fromList [("en","\"How can you say that, sir?\""),("ru","“Hvordan kan De vite det?”")]]
                                           }
                                ]
                            }

    hspec $ do
        describe "Smartbook.Crypto" $ do
            let rehearse :: Text -> Either String Text = decrypt . encrypt
            it "data == decrypt . encrypt $ data - padding needed" $ do
                rehearse "1234567890abcdef12" `shouldBe` Right "1234567890abcdef12"
        
            it "data == decrypt . encrypt $ data - padding NOT needed" $
                rehearse "1234567890abcdef" `shouldBe` Right "1234567890abcdef"
        
            it "data == decrypt . encrypt $ data - padding FAKE" $
                rehearse "1234567890abcde\3" `shouldBe` Right "1234567890abcde\3"
                
        describe "Smartbook.Book" $ do
            let result = exampleComposeBook (enruBoilerplate "file" "title" "author") 
                                            (plainChapters ["kapitel", "chapter"] bilData)
            it "simple book creation - with non-ascii letters and two chapters" $
                result `shouldBe` book
                
            
        describe "Smartbook.JSON" $ do
            it "to and from json conversion check" $
                (eitherDecode . encode $ book) `shouldBe` Right book
