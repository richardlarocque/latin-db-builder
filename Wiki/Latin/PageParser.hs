module Wiki.Latin.PageParser where

import qualified Data.Text as T
import Data.Maybe
import Data.List

import Latin.Types
import Latin.PartsOfSpeech

import Wiki.Latin.NounDecl
import Wiki.Latin.AdjectiveDecl
import Wiki.Latin.VerbConjugation
import Wiki.Latin.AdverbForms
import Wiki.Latin.Pronouns
import Wiki.Latin.Headword
import Wiki.Latin.Definitions

import Wiki.PageParser
import Wiki.Types

import Debug.Trace

getStaticLatinEntries :: [LatinEntry]
getStaticLatinEntries = pronoun_entries

extractLatinEntries :: Page -> [LatinEntry]
extractLatinEntries (Page lemma txt) =
	let sections = getPOSSections $ getSection 2 (T.pack "Latin") $ parsePage txt in
	catMaybes [ toLatinEntry part lemma section | (part,section) <- sections ]

hardcoded_pronouns :: [T.Text]
hardcoded_pronouns = map getLemma pronoun_entries

skipIfHardCoded :: T.Text -> Maybe ()
skipIfHardCoded lemma =
	if lemma `elem` hardcoded_pronouns
		then Nothing
		else Just ()

toLatinEntry :: PartOfSpeech -> T.Text -> [WikiLine] -> Maybe LatinEntry
toLatinEntry part lemma section = do
	skipIfHardCoded lemma
	let templates = extractTemplates section
	head <- getHeadword lemma section templates
	defs <- getDefinitions section
	let basic_entry = BasicEntry part lemma head defs
	let rich_entry = case part of
		Noun -> do
			(_,inflections) <- getNounInflections (extractTemplates section)
			return $ NounEntry lemma head defs inflections
		Adjective -> do
			(_,inflections) <- getAdjectiveInflections (extractTemplates section)
			return $ AdjectiveEntry lemma head defs inflections
		-- Determiners sometimes parse like adjectives
		Determiner -> do
			(_,inflections) <- getAdjectiveInflections (extractTemplates section)
			return $ DeterminerEntry lemma head defs inflections
		Verb -> do
			(_,conjugations) <- getVerbConjugations (extractTemplates section)
			return $ VerbEntry lemma head defs conjugations
                Adverb -> do
			(_,forms) <- getAdverbForms (extractTemplates section)
			return $ AdverbEntry lemma head defs forms
		Pronoun -> Nothing -- Skip all pronouns.  We use static definitions for them.
		_ -> Nothing
	return $ fromMaybe basic_entry rich_entry
