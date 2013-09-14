module Latin.Conjugation where

import Prelude
import qualified Data.Text as T

data Person =
	First
	| Second
	| Third
	deriving (Show)

data Number =
	Singular
	| Plural
	deriving (Show)

data Mood =
	Indicative
	| Subjunctive
	deriving (Show)

data Voice =
	Active
	| Passive
	deriving (Show)

data Tense =
	Present
	| Imperfect
	| Future
	| Perfect
	| Pluperfect
	| FuturePerfect
	deriving (Show)

data VerbForm =
	Conjugated Mood Voice Tense Number Person T.Text
	| Imperative Voice Tense Number T.Text
	| Infinitive Voice Tense T.Text
	| Participle Voice Tense T.Text
	deriving (Show)
