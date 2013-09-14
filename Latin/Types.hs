module Latin.Types where

import qualified Data.Text as T

import Latin.PartsOfSpeech
import Latin.Conjugation
import Latin.Declension

data Definition = Definition [T.Text]
	deriving (Show)

data LatinEntry =
	BasicEntry PartOfSpeech T.Text Definition
	| NounEntry T.Text Definition [InflectedNoun]
	| AdjectiveEntry T.Text Definition [InflectedAdjective]
	| VerbEntry T.Text Definition [VerbForm]
	deriving (Show)
