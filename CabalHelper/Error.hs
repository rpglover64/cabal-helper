-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric  #-}
module CabalHelper.ErrorTypes where

import Control.Monad
import Data.List
import Data.Version
import Data.Data
import GHC.Generics
import Text.Printf

data ChError = ChErrorSetupConfigHeader FilePath
             | ChErrorGhcVersion Version Version
             | ChErrorInstallCabalLibrary Version
             | ChProcess String String [String] Int
 deriving (Show, Eq, Ord, Data, Generic, Typeable)

data ChInfo = ChInstallingCabalLibrary ChLocation ChCabalVersion

data ChLocation = ChPrivate
                | ChUserPackageDb
 deriving (Show, Eq, Ord, Enum, Data, Generic, Typeable)

data ChSuggestion = ChReconfigure
                  | ChInstallCabalLibrary ChLocation ChCabalVersion
                  | ChUpdateCabalInstall ChCabalVersion
                  | ChSugConditional ChCond ChSuggestion
                  | ChSugSequence [ChSuggestion]
 deriving (Show, Eq, Ord, Data, Generic, Typeable)

data ChCabalVersion = ChCVSameMajorVersion Version
                    | ChCVLaterThanOrEqual Version
                    | ChCVExactVersion Version
                    | ChCVAnyAvailableVersion
 deriving (Show, Eq, Ord, Data, Generic, Typeable)

data ChCond = ChBuildType ChBuildType
            | ChCabalLibraryNotAvailable
 deriving (Show, Eq, Ord, Data, Generic, Typeable)

data ChBuildType = Simple | Custom | BuildTypeOther String
 deriving (Show, Eq, Ord, Data, Generic, Typeable)
