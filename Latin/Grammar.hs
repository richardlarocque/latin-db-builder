module Latin.Grammar where

import qualified Data.Text as T

data Case =
	Nominative
	| Genitive
	| Dative
	| Accusative
	| Ablative
	| Vocative
	| Locative
	deriving (Show,Enum)

data Gender =
	Masculine
	| Feminine
	| Neuter
	deriving (Show,Enum)

data Number =
	Singular
	| Plural
	deriving (Show,Enum)

data Person =
	First
	| Second
	| Third
	deriving (Show,Enum)

data Mood =
	Indicative
	| Subjunctive
	deriving (Show,Enum)

data Voice =
	Active
	| Passive
	deriving (Show,Enum)

data Tense =
	Present
	| Imperfect
	| Future
	| Perfect
	| Pluperfect
	| FuturePerfect
	deriving (Show,Enum)

data GenderedInflection = GenderedInflection Case Number Gender T.Text
	deriving (Show)

data Inflection = Inflection Case Number T.Text
	deriving (Show)

data VerbForm =
	Conjugated Mood Voice Tense Number Person T.Text
	| Imperative Voice Tense Number T.Text
	| Infinitive Voice Tense T.Text
	| Participle Voice Tense Case Number Gender T.Text
	deriving (Show)

data AdverbForm =
	Normal T.Text
	| Comparative T.Text
	| Superlative T.Text
	deriving (Show)
