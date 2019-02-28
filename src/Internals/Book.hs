{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, DeriveAnyClass, TemplateHaskell #-}

module Internals.Book
where

import Prelude                                   hiding (lines, isInfixOf)
import Data.Map                                         (Map, fromList)
import Data.Text.Lazy                                   (Text, empty, lines, toLower, isInfixOf)
import Control.Exception                                (assert)

import GHC.Generics
import Data.Default
import Control.Lens
import Data.Text.Lazy.Lens
import Data.List.Split

instance Default Text where
  def = empty
  
data Version = Version
    { _versionName :: Text
    , _versionLang :: Text
    , _versionTranslation :: Maybe Text
    , _versionTranslationSize :: Maybe Int
    , _versionAuthor :: Maybe Text
    } deriving (Generic, Default, Show, Eq)
makeLenses ''Version
        
data Chapter 
    = Chapter {chapterName :: Text, chapterDescription :: Maybe Text, chapters :: [Chapter]}
    | Leaf {chapterName :: Text, chapterDescription :: Maybe Text, paragraphs :: [Map Text Text]}
    deriving (Generic, Show, Eq)
instance Default Chapter where
  def = Leaf def def []

data Book = Book 
    { _bookLang :: Text
    , _bookName :: Text
    , _bookRusName :: Text
    , _bookAuthor :: Text
    , _bookHash :: Text
    , _bookThumbnail :: Text
    , _bookFilename :: Text
    , _bookSize :: Int
    , _bookLangs :: [ Version ]
    , _bookChapters :: [Chapter]
    } deriving (Generic, Default, Show, Eq)
makeLenses ''Book    

-- Only en-ru direction lets you see author's translation in Smartbook app    
enruBoilerplate fileName title author = def 
    & (bookLang .~ "en"^.packed) 
    . (bookFilename .~ fileName^.packed)
    . (bookLangs .~ [enVersion title author, ruVersion])
    where 
        enVersion title author = def 
            & (versionName .~ title^.packed) 
            . (versionLang .~ "en"^.packed) 
            . (versionAuthor .~ Just (author^.packed))
        ruVersion = def 
            & (versionName .~ empty) 
            . (versionLang .~ "ru"^.packed) 
            . (versionTranslation .~ Just empty)
            . (versionTranslationSize .~ Just 0)

-- TODO: correctly handle ill-formatted texts, i.e. when linesBook1 != linesBook2            
-- plainChapters means that nested chapters will be at the same first level as their parent chapters
-- bilingualText : 1st row - lang 1, 2nd row - lang 2, 3rd row - lang 1, 4th row - lang 2, ...
-- firstBookChapterMarkers - a list of possible chapter words to break upon encountering; case is irrelevant
plainChapters :: [Text] -> Text -> [Chapter]
plainChapters firstBookChapterMarkers bilingualText = 
    let 
        indexed = zip [0..] (lines bilingualText)
        lsFst = map snd $ filter (even . fst) indexed -- indicies simply to filter out first and second books properly
        lsSnd = map snd $ filter (odd . fst) indexed
        lsFstSplit = split (dropBlanks . condense . keepDelimsR $ whenElt isNoMarkers) lsFst -- chapter, if found, goes first in the sublist
        lsSndSplit = splitPlaces (map length lsFstSplit) lsSnd -- mirrors the split of the first
    in zipWith chapter lsFstSplit lsSndSplit
    where 
        -- TODO: shouldn't happen, yet add logs here just in case
        chapter [] _ = assert False undefined
        chapter first@(x:xs) second = def {chapterName = x, paragraphs = zipWith bilingualElem first second}
        bilingualElem en ru = fromList [("en", en), ("ru", ru)]
        markers = map toLower firstBookChapterMarkers
        isNoMarkers x = {-Data.List.-}all (not . flip isInfixOf (toLower x)) markers
          
exampleComposeBook bookBoilerplate chapters = 
    bookBoilerplate & (bookChapters .~ chapters)
