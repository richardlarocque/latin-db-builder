module Wiki.Markup where

import Prelude

import qualified Data.Text as T

import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator

import Data.Maybe
import Data.List

import Debug.Trace

validLinkChar :: Parser Char
validLinkChar = noneOf "|]"

unLink :: Parser T.Text
unLink = do
	string "[["
	text <- many (validLinkChar)
	rename_text <- optionMaybe $ do
		char '|'
		many (validLinkChar)
	string "]]"
	return $ T.pack $ fromMaybe text rename_text

unFormOf :: Parser T.Text
unFormOf = do
	string "{{form of|"
	form <- many (noneOf "|}")
	optional $ do { char '|'; lang_la }
	char '|'
	link <- many letter
	link_name <- optionMaybe $ do
		char '|'
		many letter >>= return
	optional $ do { char '|'; lang_la } 
	string "}}"
	return $ T.pack $ form ++ " of " ++ (fromMaybe link link_name)
	where
		lang_la = string "lang=la"

unAltOf :: Parser T.Text
unAltOf = do
	string "{{"
	altof <- (try $ string "alternative form of") <|> (string "alternative spelling of")
	char '|'
	link <- many (noneOf "|}")
	link_name <- optionMaybe $ do
		char '|'
		many letter >>= return
	many $ noneOf "}"
	string "}}"
	return $ T.pack $ "alternative form of " ++ (fromMaybe link link_name)

unQualifier :: Parser T.Text
unQualifier = do
	string "{{"
	altof <- (try $ string "qualifier")
	char '|'
	inside <- many (noneOf "}")
	string "}}"
	return $ T.pack inside

unInflectionOf :: Parser T.Text
unInflectionOf = do
	string "{{inflection of|"
	without_macron <- many letter
	with_macron <- optionMaybe $ do { char '|'; many1 letter }
	char '|'
	la_case <- many1 letter
	char '|'
	num <- char 's' <|> char 'p'
	string "|lang=la}}"
	let la_case' = toPrettyCase la_case
	let num' = toPrettyNumber num
	return $ T.pack $ la_case' ++ " " ++ num' ++ " of " ++ (fromMaybe without_macron with_macron)
	where
		toPrettyCase c = case c of
			"nom" -> "nominative"
			"gen" -> "genitive"
			"dat" -> "dative"
			"acc" -> "accusative"
			"abl" -> "ablative"
			"loc" -> "locative"
			"voc" -> "vocative"
		toPrettyNumber n = case n of
			's' -> "singular"
			'p' -> "plural"

unContextOf :: Parser T.Text
unContextOf = do
	string "{{context|"
	contexts <- sepBy1 (many (noneOf "|}")) (char '|')
	string "}}"
	-- Use init to drop the last parameter ("lang=la" most of the time).
	return $ T.pack $ parenthesize $ intercalate ", " (init contexts)
	where
		parenthesize x = "(" ++ x ++ ")"

unGloss :: Parser T.Text
unGloss = do
	string "{{gloss|"
	content <- many $ noneOf "}"
	string "}}"
	return $ T.pack $ "(" ++ content ++ ")"

unBold :: Parser T.Text
unBold = do
	char '\''
	char '\''
	char '\''
	text <- many (alphaNum <|> space)
	char '\''
	char '\''
	char '\''
	return $ T.pack (text)

reQuote :: Parser T.Text
reQuote = do
	char '\''
	char '\''
	return $ T.singleton '"'

normalChar :: Parser T.Text
normalChar = anyChar >>= return . T.singleton

-- TODO: Need a better solution for this.  Do two passes?
-- One to unlink, unbold, etc. and another to remove templates?
unMarkup' :: Parser T.Text
unMarkup' = many (
	try unLink
	<|> try unBold
	<|> try unFormOf
	<|> try unAltOf
	<|> try unQualifier
	<|> try unInflectionOf
	<|> try unContextOf
	<|> try unGloss
	<|> try normalChar) >>= (return . T.concat)

unMarkup :: T.Text -> T.Text
unMarkup t =
	case runParser unMarkup' () "" t of
		Left err -> t
		Right result -> result

