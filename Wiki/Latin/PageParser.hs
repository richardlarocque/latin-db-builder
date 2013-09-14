module Wiki.Latin.PageParser where

import Prelude

import qualified Data.Text as T
import Data.Maybe
import Data.List

import Latin.Types
import Latin.PartsOfSpeech

import Wiki.Latin.NounDecl
import Wiki.Latin.AdjectiveDecl
import Wiki.Latin.VerbConjugation

import Wiki.PageParser
import Wiki.Types

extractLatinEntries :: Page -> [LatinEntry]
extractLatinEntries (Page lemma txt) =
	let sections = getPOSSections $ getSection 2 (T.pack "Latin") $ parsePage txt in
	catMaybes [ toLatinEntry part lemma section | (part,section) <- sections, isInteresting section ]
	where isInteresting section = hasInterestingHeadword (extractTemplates section)

toLatinEntry :: PartOfSpeech -> T.Text -> [WikiLine] -> Maybe LatinEntry
toLatinEntry Noun lemma section = do
	let defs = getDefinitions section
	(_,inflections) <- getNounInflections (extractTemplates section)
	return $ NounEntry lemma defs inflections
toLatinEntry Adjective lemma section = do
	let defs = getDefinitions section
	(_,inflections) <- getAdjectiveInflections (extractTemplates section)
	return $ AdjectiveEntry lemma defs inflections
toLatinEntry Verb lemma section = do
	let defs = getDefinitions section
	(_,conjugations) <- getVerbConjugations (extractTemplates section)
	return $ VerbEntry lemma defs conjugations
toLatinEntry pos lemma section = do
	let defs = getDefinitions section
	return $ BasicEntry pos lemma defs
