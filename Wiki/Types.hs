module Wiki.Types where

import qualified Data.Text as T

data Page = Page T.Text T.Text
	deriving Show

data TemplateRef = TemplateRef [T.Text]
	deriving Show

data WikiLine =
	Section Int T.Text
	| BlankLine
	| Line T.Text
	| Bullet T.Text
	| NumBullet T.Text
	deriving Show
