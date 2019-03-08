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
    illData <- dataFile "bkIllFormed.txt" -- the file contains odd number of lines, i.e. the first book is bigger than the second 
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
    let illBook = Book { _bookLang = "en"       -- by virtue of zipWith Sherlock Holmes line must be absent in a resulting json
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
                                                [fromList [("en","Chapter 1"),("ru","Første kapitel.")]]
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
                
            it "padding test encrypt - block % 16 /= 0" $ do
                encrypt ("1234567890a"::Text) `shouldBe` "yvjcZdXUCgBGUVcEctaCQg=="

            it "padding test decrypt - block % 16 /= 0" $ do
                decrypt ("yvjcZdXUCgBGUVcEctaCQg==") `shouldBe` (Right "1234567890a"::Either String Text)
                
            it "padding test - block % 16 == 0" $ do
                encrypt ("1234567890abcdef"::Text) `shouldBe` "qdWoox9ENOESXXiKihnsl1j1Fg85ZisQAqTeNhe32s4="
                
            it "padding test decrypt - block % 16 == 0" $ do
                decrypt "qdWoox9ENOESXXiKihnsl1j1Fg85ZisQAqTeNhe32s4=" `shouldBe` (Right "1234567890abcdef"::Either String Text)
                
        describe "Smartbook.Book" $ do
            let bilResult = exampleComposeBook (enruBoilerplate "file" "title" "author") 
                                            (plainChapters ["kapitel", "chapter"] bilData)
                
            let illResult = exampleComposeBook (enruBoilerplate "file" "title" "author") 
                                            (plainChapters ["kapitel", "chapter"] illData)
            let winPathResult = enruBoilerplate "c:\\fld1\\fld2\\file.txt" "" ""
            let linPathResult = enruBoilerplate "/fld1/fld2/file" "" ""
            it "simple book creation - ill formed book with odd lines" $
                illResult `shouldBe` illBook
            it "simple book creation - with non-ascii letters and two chapters" $
                bilResult `shouldBe` book
            it "simple book creation - windows file path test" $
                (_bookFilename winPathResult) `shouldBe` "file.txt"
            it "simple book creation - linux file path test" $
                (_bookFilename linPathResult) `shouldBe` "file"
            
        describe "Smartbook.JSON" $ do
            it "to and from json conversion check" $
                (eitherDecode . encode $ book) `shouldBe` Right book
                
        describe "Smartbook Integration Test" $ do
            let b1 = exampleComposeBook (enruBoilerplate "file" "title" "author") 
                                        (plainChapters ["kapitel", "chapter"] bilData)
            let b2 = eitherDecode . encode $ b1
            it "bilingual txt-file -> json book -> sb-book -> json book 'shouldBe' original json" $
                b2 `shouldBe` Right book
