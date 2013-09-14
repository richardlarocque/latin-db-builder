module Wiki.PageParser(
WikiLine,
extractTemplates,
getDefinitions,
getPOSSections,
getSection,
hasInterestingHeadword,
parsePage) where

import qualified Data.Text as T
import Data.Maybe
import Data.List
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Wiki.Types
import Wiki.Markup
import Latin.PartsOfSpeech

import Latin.Types

data WikiLine =
	Section Int T.Text
	| BlankLine
	| Line T.Text
	| Bullet T.Text
	| NumBullet T.Text
	deriving Show

getTemplates :: T.Text -> [TemplateRef]
getTemplates txt = 
	let submatches = firstSubMatches $ txt =~ ("{{([^}]*)}}" :: String) in
	map (TemplateRef.(T.splitOn (T.singleton '|'))) submatches
	where firstSubMatches :: [[T.Text]] -> [T.Text]
	      firstSubMatches ms = map (\x -> x !! 1) ms

-- We don't bother trying to extract templates from bullets or section headers.
extractTemplates :: [WikiLine] -> [TemplateRef]
extractTemplates ls = concat [ getTemplates txt | (Line txt) <- ls ]

parsePage :: T.Text -> [WikiLine]
parsePage page = map parseLine (T.lines page)

parseLine :: T.Text -> WikiLine
parseLine line = head $ catMaybes $ [ f line | f <- line_parsers ]

line_parsers :: [T.Text -> Maybe WikiLine]
line_parsers = [
	parseSectionHeader,
	parseBlankLine,
	parseBullet,
	parseNumBullet,
	Just . Line . unMarkup]

parseSectionHeader :: T.Text -> Maybe WikiLine
parseSectionHeader line = do
	matches <- (line =~~ "^(=+)([^=]+)")
	let matches' = getAllTextSubmatches matches :: [T.Text]
	let equal_signs = matches' !! 1
	let section_level = T.length equal_signs
	let header_text = matches' !! 2
	return $ Section section_level header_text

parseBlankLine :: T.Text -> Maybe WikiLine
parseBlankLine txt =
	if txt == (T.empty)
		then Just BlankLine
		else Nothing

parseBullet :: T.Text -> Maybe WikiLine
parseBullet l = do
	unbullet <- T.stripPrefix (T.pack "* ") l
	return $ Bullet (unMarkup unbullet)

parseNumBullet :: T.Text -> Maybe WikiLine
parseNumBullet l = do
	unbullet <- T.stripPrefix (T.pack "# ") l
	return $ NumBullet (unMarkup unbullet)

getSection :: Int -> T.Text -> [WikiLine] -> [WikiLine]
getSection level name = (dropNonSectionSuffix.tryDropHead.dropNonSectionPrefix)
	where
		dropNonSectionPrefix = dropWhile (not.(isMatchingHeader level name))
		dropNonSectionSuffix = takeWhile (not.(isLevelNHeader level))
		tryDropHead ys = case ys of
			(_:rest) -> rest
			[] -> []
		isMatchingHeader l n h = case h of
			(Section l' n') -> l == l' && n == n'
			_ -> False
		isLevelNHeader l h = case h of
			(Section l' _) -> l == l'
			_ -> False

-- Mess of code to parse into sections.  Could this be written better?

getPOSSections :: [WikiLine] -> [(PartOfSpeech, [WikiLine])]
getPOSSections ls = getPOSSections' [] ls

getPOSSections' :: [(PartOfSpeech, [WikiLine])] -> [WikiLine] -> [(PartOfSpeech, [WikiLine])]
getPOSSections' buf ls = 
	case getPOSSection ls of
		Just (result, rest) -> getPOSSections' (result:buf) rest
		Nothing -> buf

getPOSSection :: [WikiLine] -> Maybe ((PartOfSpeech, [WikiLine]), [WikiLine])
getPOSSection ls = do
	(part, level, after_header) <- getPOSStart ls
	let (section, rest) = span (not.(isLevelNHeader level)) after_header
	return ((part, section), rest)
	where
		isLevelNHeader n h = case h of
			(Section l _) -> l == n
			_ -> False

getPOSStart :: [WikiLine] -> Maybe (PartOfSpeech, Int, [WikiLine])
getPOSStart ((Section l h):xs) =
	case lookup h header_part_map of
		Just part -> Just (part, l, xs)
		Nothing -> getPOSStart xs
getPOSStart (_:xs) = getPOSStart xs
getPOSStart [] = Nothing

header_part_map :: [(T.Text, PartOfSpeech)]
header_part_map = [((T.pack.sectionHeader) p, p) | p <- latin_parts_of_speech]

-- Expects input to be pre-filtered to contain only the Latin POS section.

getDefinitions :: [WikiLine] -> Definition
getDefinitions = (Definition . getBullets)
	where
		getBullets ((NumBullet txt):xs) = txt:(getBullets xs)
		getBullets ((Section _ _):_) = []
		getBullets (_:xs) = getBullets xs
		getBullets [] = []

--

hasInterestingHeadword :: [TemplateRef] -> Bool
hasInterestingHeadword = any hasInterestingHeadword'
	where
		hasInterestingHeadword' t = case t of
			(TemplateRef (h:l:p:_))
				| h == (T.pack "head") && l == (T.pack "la")
				-> elem p interesting_parts
			(TemplateRef (n:_)) -> elem n interesting_templates
			_ -> False

interesting_templates :: [T.Text]
interesting_templates = map T.pack [
	"la-verb",
	"la-noun",
	"la-proper noun",
	"la-adj-1&2",
	"la-adj-3rd-1E",
	"la-adj-3rd-2E",
	"la-adj-3rd-3E",
	"la-adv",
	"la-interj"]

interesting_parts :: [T.Text]
interesting_parts = map T.pack [
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
