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

