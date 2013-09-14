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
	| Verb deriving (Show,Enum)

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
