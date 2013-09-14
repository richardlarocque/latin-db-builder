module Latin.Declension where

import qualified Data.Text as T

data Case =
	Nominative
	| Genitive
	| Dative
	| Accusative
	| Ablative
	| Vocative
	| Locative
	deriving (Show,Eq)

data Gender =
	Masculine
	| Feminine
	| Neuter
	deriving Show

data Number =
	Singular
	| Plural
	deriving (Show, Eq)

data InflectedNoun = InflectedNoun Case Number T.Text
	deriving Show

data InflectedAdjective = InflectedAdjective Case Number Gender T.Text
	deriving Show
