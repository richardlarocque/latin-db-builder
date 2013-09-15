{-# LANGUAGE OverloadedStrings #-}

module Wiki.Latin.AdverbForms where

import qualified Data.Text as T

import Data.Maybe

import Latin.Grammar
import Wiki.Latin.Headword
import Wiki.Types

getAdverbForms :: [TemplateRef] -> Maybe (T.Text, [AdverbForm])
getAdverbForms trefs = do
	args <- listToMaybe $ [ xs | (TemplateRef (x:xs)) <- trefs, x == "la-adv" ]
	forms <- getAdverbForms' args
	return ("la-adv", forms)

getAdverbForms' :: [T.Text] -> Maybe [AdverbForm]
getAdverbForms' args =
	case args of
		[stem,stem_mac,pattern] | stem_mac == "" -> patternHelper stem pattern
		[_,stem_mac,pattern] -> patternHelper stem_mac pattern
		[pp1,pp2_nomac,pp2_mac,pp3] -> let
			pp2' = if pp2_mac == "" then pp2_nomac else pp2_mac in
			withForms pp1 pp2' pp3
		[pp1,pp2_nomac,pp2_mac,pp3_mac,_] -> let
			pp2' = if pp2_mac == "" then pp2_nomac else pp2_mac in
			withForms pp1 pp2' pp3_mac
	where
	patternHelper stem pattern =
		if pattern == "e" then withSuffixes stem "ē" "ius" "issimē"
		else if pattern == "er" then withSuffixes stem "er" "ius" "issimē"
		else if pattern == "ter" then withSuffixes stem "ter" "ius" "issimē"
		else if pattern == "iter" then withSuffixes stem "iter" "ius" "issimē"
		else if pattern == "im" then withSuffixes stem "im" "ius" "issimē"
		else if pattern == "-" then Just [ Normal stem ]
		else Nothing
	withSuffixes stem n c s = Just $ [
		Normal (stem `T.append` n),
		Comparative (stem `T.append` c),
		Superlative (stem `T.append` s) ]
	withForms n c s = Just $ [
		Normal n,
		Comparative c,
		Superlative s ]

