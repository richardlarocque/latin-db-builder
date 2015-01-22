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
