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

{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
module CabalHelper.Types where

import GHC.Generics
import Data.Version

newtype ChModuleName = ChModuleName String
    deriving (Eq, Ord, Read, Show, Generic)

data ChComponentName = ChSetupHsName
                     | ChLibName
                     | ChExeName String
                     | ChTestName String
                     | ChBenchName String
  deriving (Eq, Ord, Read, Show, Generic)

data ChResponse
    = ChResponseStrings    [(ChComponentName, [String])]
    | ChResponseEntrypoints [(ChComponentName, ChEntrypoint)]
    | ChResponseLbi String
    | ChResponseVersion String Version
  deriving (Eq, Ord, Read, Show, Generic)

data ChEntrypoint = ChSetupEntrypoint -- ^ Almost like 'ChExeEntrypoint' but
                                      -- @main-is@ could either be @"Setup.hs"@
                                      -- or @"Setup.lhs"@. Since we don't know
                                      -- where the source directory is you have
                                      -- to find these files.
                  | ChLibEntrypoint { chExposedModules :: [ChModuleName]
                                    , chOtherModules   :: [ChModuleName]
                                    }
                  | ChExeEntrypoint { chMainIs         :: FilePath
                                    , chOtherModules   :: [ChModuleName]
                                    } deriving (Eq, Ord, Read, Show, Generic)
