{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, DeriveAnyClass, TemplateHaskell #-}

module Internals.JSON
where

import Prelude                              hiding (lines)
import Data.Text.Lazy.Lens
import GHC.Generics
import Data.Aeson.Types
import Control.Applicative                  ( (<$>), (<*>), (<|>) )
import qualified Data.Aeson.Text            as Aeson 
import Data.Text.Lazy                                   (Text, empty, pack, lines)

import Internals.Book
