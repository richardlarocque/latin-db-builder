module Latin.Types where

import qualified Data.Text as T

import Latin.PartsOfSpeech
import Latin.Grammar

data Definition = Definition [T.Text]
	deriving (Show)

data LatinEntry =
	BasicEntry PartOfSpeech T.Text T.Text Definition
	| VerbEntry T.Text T.Text Definition [VerbForm]
	| NounEntry T.Text T.Text Definition [Inflection]
	| AdjectiveEntry T.Text T.Text Definition [GenderedInflection]
	| DeterminerEntry T.Text T.Text Definition [GenderedInflection]
        | AdverbEntry T.Text T.Text Definition [AdverbForm]
	| PronounEntry T.Text T.Text Definition [Inflection]
	| PronounEntryG T.Text T.Text Definition [GenderedInflection]
	deriving (Show)

getLemma :: LatinEntry -> T.Text
getLemma (BasicEntry _ l _ _) = l
getLemma (VerbEntry l _ _ _) = l
getLemma (NounEntry l _ _ _) = l
getLemma (AdjectiveEntry l _ _ _) = l
getLemma (DeterminerEntry l _ _ _) = l
getLemma (AdverbEntry l _ _ _) = l
getLemma (PronounEntry l _ _ _) = l
getLemma (PronounEntryG l _ _ _) = l

getPart :: LatinEntry -> PartOfSpeech
getPart (BasicEntry p _ _ _) = p
getPart (NounEntry _ _ _ _) = Noun
getPart (AdjectiveEntry _ _ _ _) = Adjective
getPart (DeterminerEntry _ _ _ _) = Determiner
getPart (VerbEntry _ _ _ _) = Verb
getPart (AdverbEntry _ _ _ _) = Adverb
getPart (PronounEntry _ _ _ _) = Pronoun
getPart (PronounEntryG _ _ _ _) = Pronoun

getDefs :: LatinEntry -> [T.Text]
getDefs (BasicEntry _ _ _ (Definition defs)) = defs
getDefs (NounEntry _ _ (Definition defs) _) = defs
getDefs (AdjectiveEntry _ _ (Definition defs) _) = defs
getDefs (DeterminerEntry _ _ (Definition defs) _) = defs
getDefs (VerbEntry _ _ (Definition defs) _) = defs
getDefs (AdverbEntry _ _ (Definition defs) _) = defs
getDefs (PronounEntry _ _ (Definition defs) _) = defs
getDefs (PronounEntryG _ _ (Definition defs) _) = defs

getHeader :: LatinEntry -> T.Text
getHeader (BasicEntry _ _ h _) = h
getHeader (NounEntry _ h _ _) = h
getHeader (AdjectiveEntry _ h _ _) = h
getHeader (DeterminerEntry _ h _ _) = h
getHeader (VerbEntry _ h _ _) = h
getHeader (AdverbEntry _ h _ _) = h
getHeader (PronounEntry _ h _ _) = h
getHeader (PronounEntryG _ h _ _) = h
