{-# LANGUAGE TemplateHaskell #-}
module Smartbook
( module Internals.Crypto
, module Internals.Book
, module Internals.JSON
, libraryGitInfo
) where

import Internals.Crypto
import Internals.Book
import Internals.JSON
import Development.GitRev

--gitLibraryInfo :: a
libraryGitInfo = concat [ $(gitBranch), "@", $(gitHash) ]

