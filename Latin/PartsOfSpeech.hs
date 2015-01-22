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

module Latin.PartsOfSpeech(
	PartOfSpeech(..),
	sectionHeader,
	latin_parts_of_speech) where

data PartOfSpeech =
	Adjective
	| Adverb
	| Conjunction
	| Determiner
	| Interjection
	| Noun
	| Numeral
	| Particle
	| Preposition
	| Pronoun
	| Verb deriving (Show,Eq,Enum)

latin_parts_of_speech :: [PartOfSpeech]
latin_parts_of_speech =
	[ Adjective
	, Adverb
	, Conjunction
	, Determiner
	, Interjection
	, Noun
	, Numeral
	, Particle
	, Preposition
	, Pronoun
	, Verb ]

sectionHeader :: PartOfSpeech -> String
sectionHeader Adjective = "Adjective"
sectionHeader Adverb = "Adverb"
sectionHeader Conjunction = "Conjunction"
sectionHeader Determiner = "Determiner"
sectionHeader Interjection = "Interjection"
sectionHeader Noun = "Noun"
sectionHeader Numeral = "Numeral"
sectionHeader Particle = "Particle"
sectionHeader Preposition = "Preposition"
sectionHeader Pronoun = "Pronoun"
sectionHeader Verb = "Verb"
