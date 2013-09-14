module Wiki.Types where

import qualified Data.Text as T

import Latin.PartsOfSpeech
import Latin.Conjugation
import Latin.Declension

data Page = Page T.Text T.Text

data TemplateRef = TemplateRef [T.Text]
	deriving Show
