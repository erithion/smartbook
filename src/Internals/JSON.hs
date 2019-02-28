{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Internals.JSON
where

import Data.Aeson.Types
import Control.Applicative                  ( (<$>), (<*>), (<|>) )
import Control.Lens                         (view)

import Internals.Book

instance FromJSON Version where
    parseJSON = withObject "Version" $ -- name is required for failure in case parseJSON got anything but an Object
        \v -> Version 
                <$> v .: "name" 
                <*> v .: "lang" 
                <*> v .:? "translation" 
                <*> v .:? "translationSize" 
                <*> v .:? "author"

instance FromJSON Chapter where
    parseJSON = withObject "Chapter" $ 
            \v -> Chapter 
                    <$> v .: "chapterName" 
                    <*> v .:? "chapterDescription" 
                    <*> v .: "chapters" 
            <|>   Leaf 
                    <$> v .: "chapterName" 
                    <*> v .:? "chapterDescription" 
                    <*> v .: "paragraphs"
            <|>   fail "Object is neither Chapter nor Leaf"

instance FromJSON Book where
    parseJSON = withObject "Book" $
        \v -> Book 
                <$> v .: "lang"
                <*> v .: "name"
                <*> v .: "rusName"
                <*> v .: "author"
                <*> v .: "hash"
                <*> v .: "thumbnail"
                <*> v .: "filename"
                <*> v .: "size"
                <*> v .: "langs"
                <*> v .: "chapters"

instance ToJSON Version where
    toJSON v = object [ "name" .= view versionName v
                      , "lang" .= view versionLang v
                      , "translation" .= view versionTranslation v
                      , "translationSize" .= view versionTranslationSize v
                      , "author" .= view versionAuthor v
                      ]

instance ToJSON Chapter where
    toJSON v@(Chapter _ _ _) = 
        object [ "chapterName" .= chapterName v
               , "chapterDescription" .= chapterDescription v
               , "chapters" .= chapters v
               ]
    toJSON v@(Leaf _ _ _) = 
        object [ "chapterName" .= chapterName v
               , "chapterDescription" .= chapterDescription v
               , "paragraphs" .= paragraphs v
               ]
               
instance ToJSON Book where
    toJSON v = object [ "lang" .= view bookLang v
                      , "name" .= view bookName v
                      , "rusName" .= view bookRusName v
                      , "author" .= view bookAuthor v
                      , "hash" .= view bookHash v
                      , "thumbnail" .= view bookThumbnail v
                      , "filename" .= view bookFilename v
                      , "size" .= view bookSize v
                      , "langs" .= view bookLangs v
                      , "chapters" .= view bookChapters v
                      ]