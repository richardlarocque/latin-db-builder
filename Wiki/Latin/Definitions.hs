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
