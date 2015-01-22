{-
Copyright (C) 2014 Richard Larocque <richard.larocque@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{-# LANGUAGE OverloadedStrings #-}

module Wiki.Latin.Definitions(getDefinitions) where

import qualified Data.Text as T

import Latin.Types
import Wiki.Types
import Wiki.Markup

-- Expects input to be pre-filtered to contain only the Latin POS section.

getDefinitions :: [WikiLine] -> Maybe Definition
getDefinitions lines =
	case (filterUselessDefinitions . getBullets) lines of
		[] -> Nothing
		defs -> Just $ Definition defs

filterUselessDefinitions :: [T.Text] -> [T.Text]
filterUselessDefinitions = (map unMarkup) . (filter (not . isUselessDefinition))

isUselessDefinition :: T.Text -> Bool
isUselessDefinition d = or [
	T.isInfixOf "{{form of|" d,
	T.isInfixOf "{{alternative form of|" d,
	T.isInfixOf "{{inflection of|" d]

getBullets :: [WikiLine] -> [T.Text]
getBullets ((NumBullet txt):xs) = txt:(getBullets xs)
getBullets ((Section _ _):_) = []
getBullets (_:xs) = getBullets xs
getBullets [] = []
