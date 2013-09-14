module Wiki.Markup where

import Prelude

import qualified Data.Text as T

import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator

import Debug.Trace

validLinkChar :: Parser Char
validLinkChar = alphaNum <|> space <|> char '#'

unLink :: Parser T.Text
unLink = do
	_ <- char '['
	_ <- char '['
	text <- many (validLinkChar)
	_ <- optional $ do
		_ <- char '|'
		many (validLinkChar)
	_ <- char ']'
	_ <- char ']'
	return $ T.pack text

reBold :: Parser T.Text
reBold = do
	_ <- char '\''
	_ <- char '\''
	_ <- char '\''
	text <- many (alphaNum <|> space)
	_ <- char '\''
	_ <- char '\''
	_ <- char '\''
	return $ T.pack ("<em>" ++ text ++ "</em>")

reQuote :: Parser T.Text
reQuote = do
	_ <- char '\''
	_ <- char '\''
	return $ T.singleton '"'

normalChar :: Parser T.Text
normalChar = anyChar >>= return . T.singleton

unMarkup' :: Parser T.Text
unMarkup' = many (
        try unLink
	<|> try reBold
	<|> try normalChar) >>= (return . T.concat)

unMarkup :: T.Text -> T.Text
unMarkup t =
	case runParser unMarkup' () "" t of
		Left err -> traceShow err $ t
		Right result -> result

