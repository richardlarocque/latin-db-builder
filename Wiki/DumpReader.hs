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
saxProcessDoc' xs = let
	(page, rest) = saxFindTag "page" xs
	result = processPageContents page in
	result:(saxProcessDoc' rest)

processPageContents :: [SAXType] -> Maybe Page
processPageContents xs = do
	(title, s1) <- saxFindText "title" xs
	(text, _) <- saxFindText "text" s1
	return $ Page title text

isEndTag :: String -> SAXType -> Bool
isEndTag name (EndElement etag) = etag == name
isEndTag _ _ = False

saxFindTag :: String -> [SAXType] -> ([SAXType], [SAXType])
saxFindTag _ [] = ([], [])
saxFindTag needle ((StartElement tag _):xs) | needle == tag = span (not.(isEndTag needle)) xs
saxFindTag needle (_:xs) = saxFindTag needle xs

saxFindText :: String -> [SAXType] -> Maybe (T.Text, [SAXType])
saxFindText needle xs =
	let (in_tag, rest) = saxFindTag needle xs in
	Just (saxTextContents in_tag, rest)

saxTextContents :: [SAXType] -> T.Text 
saxTextContents xs = T.concat $ mapMaybe getText xs
	where getText (CharacterData txt) = Just txt
	      getText _ = Nothing

getPages :: String -> IO [Page]
getPages filename = do
	xml_text <- LB.readFile filename
	return $ saxProcessDoc $ parse defaultParseOptions xml_text
