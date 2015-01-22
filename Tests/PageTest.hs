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

module Tests.PageTest(allTests) where

import System.FilePath
import Test.HUnit

import qualified Paths_latin_db as P

import Wiki.Latin.PageParser
import Wiki.DumpReader
import Latin.Types
import Latin.PartsOfSpeech

import Debug.Trace

readEntries :: String -> IO [LatinEntry]
readEntries name =
	let input_file = "test_input/" ++ name ++ ".xml" in
	do
		pages <- getPages input_file
		checkPageResult pages
		let page = head pages
		return $ extractLatinEntries page

checkPageResult :: [Page] -> IO ()
checkPageResult pages = case pages of
	[] -> assertFailure "Failed to read page"
	[page] -> return ()
	x -> assertFailure $ "Read too many pages" ++ (show $ x)

numEntriesTest :: String -> Int -> Test
numEntriesTest name entryCount = TestCase $ do
	entries <- readEntries name
	assertEqual ("Entries in " ++ name) entryCount (length entries)

partsOfSpeechTest :: String -> [PartOfSpeech] -> Test
partsOfSpeechTest name parts = TestCase $ do
	entries <- readEntries name
	assertEqual ("Entries in " ++ name) (length parts) (length entries)
	let actual_parts = map getPart entries
	assertEqual ("Parts of speech for " ++ name) parts actual_parts

formsCountTest :: String -> [Int] -> Test
formsCountTest name counts = TestCase $ do
	entries <- readEntries name
	let entryFormCounts = map getFormsCount entries
	sequence_ [ assertEqual ("FormCounts " ++ (show n)) e a | (n, e, a) <- zip3 [0..] counts entryFormCounts ]

getFormsCount :: LatinEntry -> Int
getFormsCount (BasicEntry _ _ _ _) = 0
getFormsCount (VerbEntry _ _ _ fs) = length fs
getFormsCount (NounEntry _ _ _ fs) = length fs
getFormsCount (AdjectiveEntry _ _ _ fs) = length fs
getFormsCount (DeterminerEntry _ _ _ fs) = length fs
getFormsCount (AdverbEntry _ _ _ fs) = length fs
getFormsCount (PronounEntry _ _ _ fs) = length fs
getFormsCount (PronounEntryG _ _ _ fs) = length fs

zeroEntriesTests :: Test
zeroEntriesTests =
	TestList $ [ TestLabel name $ numEntriesTest name 0 | name <- nameList ]
	where nameList = [
		-- Ingore phrases.
		"sum ubi",
		"nulla osta",

		-- Ignore verb forms.
		"accumulator",

		-- Ignore non-Latin entries.
		"litoral",
		"unusual",

		-- Ignore forms we've hard-coded and banned.
		"nulla",
		"tuus",
		"his",

		-- Ingore entries not using stanard templates.
		"unus"]

numEntriesTests :: Test
numEntriesTests = TestList $ [ TestLabel name $ numEntriesTest name count | (name, count) <- nameCountMap ]
	where nameCountMap = [
		("Estonia", 1),
		("Iuppiter", 1),
		("absinthium", 1),
		("conrideo", 1),
		("consensus", 2),
		("cor", 1),
		("cujusmodi", 1),
		("defixio", 1),
		("deus", 1),
		("dico", 2),
		("divus", 2),
		("enim", 1),
		("facio", 1),
		("fama", 1),
		("gens", 1),
		("iam", 1),
		("ignis", 1),
		("insula", 1),
		("lente", 1),
		("litus", 1),
		("male", 1),
		("moenia", 1),
		("nullus", 1),
		("pectus", 1),
		("procul", 1),
		("puer", 1),
		("pusillanimis", 1),
		("sanguineus", 1),
		("sanguis", 1),
		("semel", 1),
		("unda", 1),
		("urbs", 1)]

partsOfSpeechTests :: Test
partsOfSpeechTests = TestList $ [ TestLabel name $ partsOfSpeechTest name parts | (name, parts) <- namePartMap ]
	where namePartMap = [
		("abactor", [Noun]),
		("at", [Conjunction]),
		("eo", [Pronoun,Adverb,Verb]),
		("solus", [Adjective]),
		("multus", [Determiner]),
		("in", [Preposition])]

formsCountTests :: Test
formsCountTests =
	TestList $ [ TestLabel name $ formsCountTest name parts | (name, parts) <- namePartMap ]
	where namePartMap = [
		("abactor", [12]),
		("at", [0]),
		("eo", [0,1,148]),
		("solus", [36]),
		("multus", [36]),
		("in", [0])]

allTests = TestList [
	TestLabel "zeroEntriesTests" zeroEntriesTests,
	TestLabel "numEntriesTests" numEntriesTests,
	TestLabel "partsOfSpeechTests" partsOfSpeechTests,
	TestLabel "formsCountTest" formsCountTests]
