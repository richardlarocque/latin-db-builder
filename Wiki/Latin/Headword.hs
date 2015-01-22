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

{-# LANGUAGE OverloadedStrings #-}

module Wiki.Latin.Headword where

import qualified Data.Text as T

import Data.List
import Data.Maybe
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Debug.Trace

import Wiki.Types

getHeadword :: T.Text -> [WikiLine] -> [TemplateRef] -> Maybe T.Text
getHeadword lemma lines templates =
	-- Always ignore words with unusual lemmas
	if (isBannedLemma lemma)
		then Nothing
		else listToMaybe $ catMaybes [
			getHeadwordFromTemplates lemma templates,
			maybeLemmaAsHeadword lemma templates]
	-- Fall back to lemma as headword if we believe this is not
	-- a "word form" or some other uninteresting entry.
	where maybeLemmaAsHeadword l ts =
		if (containsBannedTemplate ts)
			then Nothing
			else Just lemma

getHeadwordFromTemplates :: T.Text -> [TemplateRef] -> Maybe T.Text
getHeadwordFromTemplates lemma xs = listToMaybe $ mapMaybe (parseHeadword lemma) xs

parseHeadword :: T.Text -> TemplateRef -> Maybe T.Text
parseHeadword lemma (TemplateRef (h:l:p:args))
	| h == "head" && l == "la" && elem p interesting_parts
	= Just $ fromMaybe lemma (getNamedArg "head" args)
parseHeadword _ (TemplateRef (n:args)) | n == "la-verb" = la_verb args
parseHeadword _ (TemplateRef (n:args)) | n == "la-noun" = la_noun args
parseHeadword _ (TemplateRef (n:args)) | n == "la-proper noun" = la_noun args
parseHeadword _ (TemplateRef (n:args)) | n == "la-adj-1&2" = la_adj12 args
parseHeadword _ (TemplateRef (n:args)) | n == "la-adj-3rd-1E" = la_adj3_1E args
parseHeadword _ (TemplateRef (n:args)) | n == "la-adj-3rd-2E" = la_adj3_2E args
parseHeadword _ (TemplateRef (n:args)) | n == "la-adj-3rd-3E" = la_adj3_3E args
parseHeadword _ (TemplateRef (n:args)) | n == "la-adv" = la_adv args
parseHeadword _ (TemplateRef (n:args)) | n == "la-interj" = la_interj args
parseHeadword _ _ = Nothing

stripNamedArgs :: [T.Text] -> [T.Text]
stripNamedArgs xs = filter (not.T.any (\x -> x == '=')) xs -- FIXME wrong

getNamedArg :: T.Text -> [T.Text] -> Maybe T.Text
getNamedArg argName args = listToMaybe $ mapMaybe (T.stripPrefix a) args
	where a = T.snoc argName '='

la_verb :: [T.Text] -> Maybe T.Text
la_verb args = let
	unnamed_args = stripNamedArgs args in
	case unnamed_args of
		[_,pp1,_,pp2,_,pp3,_,pp4] -> Just $ T.intercalate ", " [pp1, pp2, pp3, pp4]
		[_,pp1,_,pp2,_,pp3] -> Just $ T.intercalate ", " [pp1, pp2, pp3]
		[_,pp1,_,pp2] -> Just $ T.intercalate ", " [pp1, pp2]
		_ -> Nothing

la_noun :: [T.Text] -> Maybe T.Text
la_noun args = let
	unnamed_args = stripNamedArgs args
	extra_gender = fromMaybe "" (getG2 args) in
	case unnamed_args of
		[pp1,_,pp2,gender,infl] ->
			Just $ T.concat [pp1, ", ", pp2, " (", gender, extra_gender, ")" ]
		_ -> Nothing
	where getG2 args = do
		g2_arg <- getNamedArg "g2" args
		return $ T.concat [ ", ", g2_arg ]

la_adj12 :: [T.Text] -> Maybe T.Text
la_adj12 args =
	case args of
		[pp1,_,pp2,_,pp3] -> Just $ (T.intercalate ", " [pp1, pp2, pp3])
		_ -> Nothing

la_adj3_1E :: [T.Text] -> Maybe T.Text
la_adj3_1E args =
	case args of
		[pp1,_,pp2] -> Just $ T.concat [ pp1, ", (gen ", pp2, ")" ]
		_ -> Nothing 

la_adj3_2E :: [T.Text] -> Maybe T.Text
la_adj3_2E args =
	case args of
		[pp1,_,pp2] -> Just $ T.intercalate ", " [ pp1, pp2 ]
		_ -> Nothing

la_adj3_3E :: [T.Text] -> Maybe T.Text
la_adj3_3E args =
	case args of
		[pp1,_,pp2,_,pp3] -> Just $ T.intercalate ", " [ pp1, pp2, pp3 ]
		_ -> Nothing

la_adv :: [T.Text] -> Maybe T.Text
la_adv args = 
	case args of
		[stem,stem_mac,pattern] | stem_mac == "" -> patternHelper stem pattern
		[_,stem_mac,pattern] -> patternHelper stem_mac pattern
		[pp1,_,_,_] -> Just pp1
		[pp1,_,_,_,_] -> Just pp1
	where
	patternHelper stem pattern =
		if pattern == "e" then Just $ stem `T.append` "ē"
		else if pattern == "er" then Just $ stem `T.append` "er"
		else if pattern == "ter" then Just $ stem `T.append` "ter"
		else if pattern == "iter" then Just $ stem `T.append` "iter"
		else if pattern == "im" then Just $ stem `T.append` "im"
		else if pattern == "-" then Just stem
		else Nothing

la_interj :: [T.Text] -> Maybe T.Text
la_interj args =
	case args of
		[mac] -> Just $ mac
		_ -> Nothing

interesting_parts :: [T.Text]
interesting_parts = [
	"adjective",
	"adverb",
	"conjunction",
	"determiner",
	"interjection",
	"noun",
	"numeral",
	"particle",
	"preposition",
	"pronoun",
	"verb"]

containsBannedTemplate :: [TemplateRef] -> Bool
containsBannedTemplate trefs = any isBannedTemplate trefs
	where isBannedTemplate (TemplateRef (name:_)) = name `elem` banned_headword_templates

-- We don't want anything to do with sections containing these templates.
banned_headword_templates :: [T.Text]
banned_headword_templates = [
	"la-adj-form",
	"la-adj-comparative",
	"la-adj-superlative",
	"la-gerund",
	"la-gerund-form",
	"la-gerundive",
	"la-letter",
	"la-noun-form",
	"la-num-1&2",
	"la-num-3rd-1E",
	"la-num-card",
	"la-num-form",
	"la-num-mult",
	"la-part-form",
	"la-future participle",
	"la-perfect participle",
	"la-present participle",
	"la-pronoun-form",
	"la-punctuation mark",
	"la-suffix-1&2",
	"la-suffix-3rd-2E",
	"la-suffix-adv",
	"la-suffix-form",
	"la-suffix-noun",
	"la-verb-form" ]

-- If the lemma contains spaces or dashes or other junk, it's probably
-- not a word.
isBannedLemma :: T.Text -> Bool
isBannedLemma lemma = not (lemma =~ ("^[a-zA-Z]+$" :: String))
