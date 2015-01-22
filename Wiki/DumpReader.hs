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

module Wiki.DumpReader(Page,getPages) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import Text.XML.Expat.SAX
import Data.Maybe

import Wiki.Types

type SAXType = SAXEvent String T.Text

saxProcessDoc :: [SAXType] -> [Page]
saxProcessDoc = catMaybes . saxProcessDoc'

saxProcessDoc' :: [SAXType] -> [Maybe Page]
saxProcessDoc' [] = []
saxProcessDoc' xs = 
	case saxFindTag "page" xs of
		Just (page, rest) -> (processPageContents page):(saxProcessDoc' rest)
		Nothing -> []

processPageContents :: [SAXType] -> Maybe Page
processPageContents xs = do
	(title, s1) <- saxFindText "title" xs
	(text, _) <- saxFindText "text" s1
	return $ Page title text

isEndTag :: String -> SAXType -> Bool
isEndTag name (EndElement etag) = etag == name
isEndTag _ _ = False

saxFindTag :: String -> [SAXType] -> Maybe ([SAXType], [SAXType])
saxFindTag _ [] = Nothing
saxFindTag needle ((StartElement tag _):xs) | needle == tag = Just $ span (not.(isEndTag needle)) xs
saxFindTag needle (_:xs) = saxFindTag needle xs

saxFindText :: String -> [SAXType] -> Maybe (T.Text, [SAXType])
saxFindText needle xs = do
	(in_tag, rest) <- saxFindTag needle xs
	return (saxTextContents in_tag, rest)

saxTextContents :: [SAXType] -> T.Text 
saxTextContents xs = T.concat $ mapMaybe getText xs
	where getText (CharacterData txt) = Just txt
	      getText _ = Nothing

getPages :: String -> IO [Page]
getPages filename = do
	xml_text <- LB.readFile filename
	return $ saxProcessDoc $ parse defaultParseOptions xml_text
