module Util.Normalize(unmacron, toNormalForm) where

import qualified Data.Text as T
import Data.Text.ICU
import Data.Text.ICU.Char

unmacron :: T.Text -> T.Text
unmacron t = T.filter (property Alphabetic) (normalize NFKD t)

toNormalForm :: T.Text -> T.Text
toNormalForm = T.toLower . unmacron
