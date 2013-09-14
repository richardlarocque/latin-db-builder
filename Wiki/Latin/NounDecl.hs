module Wiki.Latin.NounDecl(getNounInflections) where

import qualified Data.Text as T
import Data.Maybe

import Wiki.Types
import Wiki.PageParser

import Latin.Declension

import Debug.Trace

getNounInflections :: [TemplateRef] -> Maybe (T.Text, [InflectedNoun])
getNounInflections trefs = listToMaybe $ mapMaybe readNounInflectionTemplate trefs

readNounInflectionTemplate :: TemplateRef -> Maybe (T.Text, [InflectedNoun])
readNounInflectionTemplate (TemplateRef []) = Nothing
readNounInflectionTemplate (TemplateRef [_]) = Nothing
readNounInflectionTemplate (TemplateRef (name:params))
	| name == (T.pack "la-decl-1st") = wrapReadFunc la_decl_1st name params
	| name == (T.pack "la-decl-1st-Greek") = wrapReadFunc la_decl_1st_Greek name params
	| name == (T.pack "la-decl-1st-Greek-Me") = wrapReadFunc la_decl_1st_Greek_Me name params
	| name == (T.pack "la-decl-1st-Greek-Ma") = wrapReadFunc la_decl_1st_Greek_Ma name params
	| name == (T.pack "la-decl-1st-abus") = wrapReadFunc la_decl_1st_abus name params
	| name == (T.pack "la-decl-1st-loc") = wrapReadFunc la_decl_1st_loc name params
	| name == (T.pack "la-decl-2nd") = wrapReadFunc la_decl_2nd name params
	| name == (T.pack "la-decl-2nd-Greek") = wrapReadFunc la_decl_2nd_Greek name params
	| name == (T.pack "la-decl-2nd-N") = wrapReadFunc la_decl_2nd_N name params
	| name == (T.pack "la-decl-2nd-N-loc") = wrapReadFunc la_decl_2nd_N_loc name params
	| name == (T.pack "la-decl-2nd-ius") = wrapReadFunc la_decl_2nd_ius name params
	| name == (T.pack "la-decl-2nd-loc") = wrapReadFunc la_decl_2nd_loc name params
	| name == (T.pack "la-decl-3rd") = wrapReadFunc la_decl_3rd name params
	| name == (T.pack "la-decl-3rd_N") = wrapReadFunc la_decl_3rd_N name params
	| name == (T.pack "la-decl-3rd_loc") = wrapReadFunc la_decl_3rd_loc name params
	| name == (T.pack "la-decl-4th") = wrapReadFunc la_decl_4th name params
	| name == (T.pack "la-decl-4th_N") = wrapReadFunc la_decl_4th_N name params
	| name == (T.pack "la-decl-5th_CONS") = wrapReadFunc la_decl_5th_CONS name params
	| name == (T.pack "la-decl-5th_VOW") = wrapReadFunc la_decl_5th_VOW name params
	| otherwise = Nothing

wrapReadFunc :: ([T.Text] -> Maybe [InflectedNoun]) -> T.Text -> [T.Text] -> Maybe (T.Text, [InflectedNoun])
wrapReadFunc f name (p:ps) = do
	decls <- case T.stripPrefix (T.pack "num=") p of
		Nothing -> f (p:ps)
		Just n | n == (T.pack "sg") -> f ps >>= return.sgOnly
		Just n | n == (T.pack "pl") -> f ps >>= return.plOnly
		Just _ -> error $ "Unexpected num= parameter: " ++ (show p)
	return (name, decls)
	where
		sgOnly ds = [ d | d@(InflectedNoun _ Singular _) <- ds ]
		plOnly ds = [ d | d@(InflectedNoun _ Plural _) <- ds ]
wrapReadFunc f n xs = f xs >>= \x -> return (n, x)

withSuffix :: T.Text -> String -> T.Text
withSuffix a b = a `T.append` (T.pack b)

nom_sg, gen_sg, dat_sg, acc_sg, abl_sg, voc_sg, loc_sg, nom_pl, gen_pl, dat_pl, acc_pl, abl_pl, voc_pl, loc_pl :: T.Text -> InflectedNoun
nom_sg = InflectedNoun Nominative Singular
gen_sg = InflectedNoun Genitive Singular
dat_sg = InflectedNoun Dative Singular
acc_sg = InflectedNoun Accusative Singular
abl_sg = InflectedNoun Ablative Singular
voc_sg = InflectedNoun Vocative Singular
loc_sg = InflectedNoun Locative Singular
nom_pl = InflectedNoun Nominative Plural
gen_pl = InflectedNoun Genitive Plural
dat_pl = InflectedNoun Dative Plural
acc_pl = InflectedNoun Accusative Plural
abl_pl = InflectedNoun Ablative Plural
voc_pl = InflectedNoun Vocative Plural
loc_pl = InflectedNoun Locative Plural

la_decl_1st, la_decl_1st_Greek, la_decl_1st_Greek_Ma, la_decl_1st_Greek_Me, la_decl_1st_abus, la_decl_1st_loc, la_decl_2nd, la_decl_2nd_Greek, la_decl_2nd_N, la_decl_2nd_N_loc, la_decl_2nd_ius, la_decl_2nd_loc, la_decl_3rd, la_decl_3rd_N, la_decl_3rd_loc, la_decl_4th, la_decl_4th_N, la_decl_5th_CONS, la_decl_5th_VOW :: [T.Text] -> Maybe [InflectedNoun]

la_decl_1st (stem:_) = Just [
	nom_sg (stem `withSuffix` "a"),
	gen_sg (stem `withSuffix` "ae"),
	dat_sg (stem `withSuffix` "ae"),
	acc_sg (stem `withSuffix` "am"),
	abl_sg (stem `withSuffix` "ā"),
	voc_sg (stem `withSuffix` "a"),
	nom_pl (stem `withSuffix` "ae"),
	gen_pl (stem `withSuffix` "ārum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "ās"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "ae")]
la_decl_1st _ = Nothing 

la_decl_1st_Greek (stem:_) = Just [
	nom_sg (stem `withSuffix` "ē"),
	gen_sg (stem `withSuffix` "ēs"),
	dat_sg (stem `withSuffix` "ae"),
	acc_sg (stem `withSuffix` "ēn"),
	abl_sg (stem `withSuffix` "ē"),
	voc_sg (stem `withSuffix` "ē"),
	nom_pl (stem `withSuffix` "ae"),
	gen_pl (stem `withSuffix` "ārum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "ās"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "ae")]
la_decl_1st_Greek _ = Nothing

la_decl_1st_Greek_Ma (stem:_) = Just [
	nom_sg (stem `withSuffix` "ās"),
	gen_sg (stem `withSuffix` "ae"),
	dat_sg (stem `withSuffix` "ae"),
	acc_sg (stem `withSuffix` "ān"),
	abl_sg (stem `withSuffix` "ā"),
	voc_sg (stem `withSuffix` "a"),
	nom_pl (stem `withSuffix` "ae"),
	gen_pl (stem `withSuffix` "ārum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "ās"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "ae")]
la_decl_1st_Greek_Ma _ = Nothing

la_decl_1st_Greek_Me (stem:_) = Just [
	nom_sg (stem `withSuffix` "ēs"),
	gen_sg (stem `withSuffix` "ae"),
	dat_sg (stem `withSuffix` "ae"),
	acc_sg (stem `withSuffix` "ēn"),
	abl_sg (stem `withSuffix` "ē"),
	voc_sg (stem `withSuffix` "ē"),
	nom_pl (stem `withSuffix` "ae"),
	gen_pl (stem `withSuffix` "ārum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "ās"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "ae")]
la_decl_1st_Greek_Me _ = Nothing

la_decl_1st_abus (stem:_) = Just [
	nom_sg (stem `withSuffix` "a"),
	gen_sg (stem `withSuffix` "ae"),
	dat_sg (stem `withSuffix` "ae"),
	acc_sg (stem `withSuffix` "am"),
	abl_sg (stem `withSuffix` "ā"),
	voc_sg (stem `withSuffix` "a"),
	nom_pl (stem `withSuffix` "ae"),
	gen_pl (stem `withSuffix` "ārum"),
	dat_pl (stem `withSuffix` "ābus"),
	acc_pl (stem `withSuffix` "ās"),
	abl_pl (stem `withSuffix` "ābus"),
	voc_pl (stem `withSuffix` "ae")]
la_decl_1st_abus _ = Nothing

la_decl_1st_loc (stem:_) = Just [
	nom_sg (stem `withSuffix` "a"),
	gen_sg (stem `withSuffix` "ae"),
	dat_sg (stem `withSuffix` "ae"),
	acc_sg (stem `withSuffix` "am"),
	abl_sg (stem `withSuffix` "ā"),
	voc_sg (stem `withSuffix` "a"),
	loc_sg (stem `withSuffix` "ae"),
	nom_pl (stem `withSuffix` "ae"),
	gen_pl (stem `withSuffix` "ārum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "ās"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "ae"),
	loc_pl (stem `withSuffix` "īs")]
la_decl_1st_loc _ = Nothing

la_decl_2nd (stem:_) = Just [
	nom_sg (stem `withSuffix` "us"),
	gen_sg (stem `withSuffix` "ī"),
	dat_sg (stem `withSuffix` "ō"),
	acc_sg (stem `withSuffix` "um"),
	abl_sg (stem `withSuffix` "ō"),
	voc_sg (stem `withSuffix` "e"),
	nom_pl (stem `withSuffix` "ī"),
	gen_pl (stem `withSuffix` "ōrum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "ōs"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "ī")]
la_decl_2nd _ = Nothing

la_decl_2nd_Greek (stem:_) = Just [
	nom_sg (stem `withSuffix` "os"),
	gen_sg (stem `withSuffix` "ī"),
	dat_sg (stem `withSuffix` "ō"),
	acc_sg (stem `withSuffix` "on"),
	abl_sg (stem `withSuffix` "ō"),
	voc_sg (stem `withSuffix` "e"),
	nom_pl (stem `withSuffix` "ī"),
	gen_pl (stem `withSuffix` "ōrum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "ōs"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "ī")]
la_decl_2nd_Greek _ = Nothing

la_decl_2nd_N (stem:_) = Just [
	nom_sg (stem `withSuffix` "um"),
	gen_sg (stem `withSuffix` "ī"),
	dat_sg (stem `withSuffix` "ō"),
	acc_sg (stem `withSuffix` "um"),
	abl_sg (stem `withSuffix` "ō"),
	voc_sg (stem `withSuffix` "um"),
	nom_pl (stem `withSuffix` "a"),
	gen_pl (stem `withSuffix` "ōrum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "a"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "a")]
la_decl_2nd_N _ = Nothing

la_decl_2nd_N_loc (stem:_) = Just [
	nom_sg (stem `withSuffix` "um"),
	gen_sg (stem `withSuffix` "ī"),
	dat_sg (stem `withSuffix` "ō"),
	acc_sg (stem `withSuffix` "um"),
	abl_sg (stem `withSuffix` "ō"),
	voc_sg (stem `withSuffix` "um"),
	loc_sg (stem `withSuffix` "ī"),
	nom_pl (stem `withSuffix` "a"),
	gen_pl (stem `withSuffix` "ōrum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "a"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "a"),
	loc_pl (stem `withSuffix` "īs")]
la_decl_2nd_N_loc _ = Nothing

la_decl_2nd_ius (stem:_) = Just [
	nom_sg (stem `withSuffix` "ius"),
	gen_sg (stem `withSuffix` "iī"),
	dat_sg (stem `withSuffix` "iō"),
	acc_sg (stem `withSuffix` "ium"),
	abl_sg (stem `withSuffix` "iō"),
	voc_sg (stem `withSuffix` "i"),
	nom_pl (stem `withSuffix` "iī"),
	gen_pl (stem `withSuffix` "iōrum"),
	dat_pl (stem `withSuffix` "iīs"),
	acc_pl (stem `withSuffix` "iōs"),
	abl_pl (stem `withSuffix` "iīs"),
	voc_pl (stem `withSuffix` "iī")]
la_decl_2nd_ius _ = Nothing

la_decl_2nd_loc (stem:_) = Just [
	nom_sg (stem `withSuffix` "us"),
	gen_sg (stem `withSuffix` "ī"),
	dat_sg (stem `withSuffix` "ō"),
	acc_sg (stem `withSuffix` "um"),
	abl_sg (stem `withSuffix` "ō"),
	voc_sg (stem `withSuffix` "e"),
	loc_sg (stem `withSuffix` "ī"),
	nom_pl (stem `withSuffix` "ī"),
	gen_pl (stem `withSuffix` "ōrum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "ōs"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "ī"),
	loc_pl (stem `withSuffix` "īs")]
la_decl_2nd_loc _ = Nothing

la_decl_3rd (nom:stem:_) = Just [
	nom_sg nom,
	gen_sg (stem `withSuffix` "is"),
	dat_sg (stem `withSuffix` "ī"),
	acc_sg (stem `withSuffix` "em"),
	abl_sg (stem `withSuffix` "e"),
	voc_sg nom,
	nom_pl (stem `withSuffix` "ēs"),
	gen_pl (stem `withSuffix` "um"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "ēs"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "ēs")]
la_decl_3rd _ = Nothing

la_decl_3rd_N (nom:stem:_) = Just [
	nom_sg nom,
	gen_sg (stem `withSuffix` "is"),
	dat_sg (stem `withSuffix` "ī"),
	acc_sg nom,
	abl_sg (stem `withSuffix` "e"),
	voc_sg nom,
	nom_pl (stem `withSuffix` "a"),
	gen_pl (stem `withSuffix` "um"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "a"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "a")]
la_decl_3rd_N _ = Nothing

la_decl_3rd_loc (nom:stem:_) = Just [
	nom_sg nom,
	gen_sg (stem `withSuffix` "is"),
	dat_sg (stem `withSuffix` "ī"),
	acc_sg (stem `withSuffix` "em"),
	abl_sg (stem `withSuffix` "e"),
	voc_sg nom,
	loc_sg (stem `withSuffix` "e"),
	nom_pl (stem `withSuffix` "ēs"),
	gen_pl (stem `withSuffix` "um"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "ēs"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "ēs"),
	loc_pl (stem `withSuffix` "ēs")]
la_decl_3rd_loc _ = Nothing

la_decl_4th (stem:_) = Just [
	nom_sg (stem `withSuffix` "us"),
	gen_sg (stem `withSuffix` "ūs"),
	dat_sg (stem `withSuffix` "uī"),
	acc_sg (stem `withSuffix` "um"),
	abl_sg (stem `withSuffix` "ū"),
	voc_sg (stem `withSuffix` "us"),
	nom_pl (stem `withSuffix` "ūs"),
	gen_pl (stem `withSuffix` "uum"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "ūs"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "ūs")]
la_decl_4th _ = Nothing

la_decl_4th_N (stem:_) = Just [
	nom_sg (stem `withSuffix` "ū"),
	gen_sg (stem `withSuffix` "ūs"),
	dat_sg (stem `withSuffix` "uī"),
	acc_sg (stem `withSuffix` "ū"),
	abl_sg (stem `withSuffix` "ū"),
	voc_sg (stem `withSuffix` "ū"),
	nom_pl (stem `withSuffix` "ua"),
	gen_pl (stem `withSuffix` "uum"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "ua"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "ua")]
la_decl_4th_N _ = Nothing

la_decl_5th_CONS (stem:_) = Just [
	nom_sg (stem `withSuffix` "ēs"),
	gen_sg (stem `withSuffix` "eī"),
	dat_sg (stem `withSuffix` "eī"),
	acc_sg (stem `withSuffix` "em"),
	abl_sg (stem `withSuffix` "ē"),
	voc_sg (stem `withSuffix` "ēs"),
	nom_pl (stem `withSuffix` "ēs"),
	gen_pl (stem `withSuffix` "ērum"),
	dat_pl (stem `withSuffix` "ēbus"),
	acc_pl (stem `withSuffix` "ēs"),
	abl_pl (stem `withSuffix` "ēbus"),
	voc_pl (stem `withSuffix` "ēs")]
la_decl_5th_CONS _ = Nothing

la_decl_5th_VOW (stem:_) = Just [
	nom_sg (stem `withSuffix` "ēs"),
	gen_sg (stem `withSuffix` "ēī"),
	dat_sg (stem `withSuffix` "ēī"),
	acc_sg (stem `withSuffix` "em"),
	abl_sg (stem `withSuffix` "ē"),
	voc_sg (stem `withSuffix` "ēs"),
	nom_pl (stem `withSuffix` "ēs"),
	gen_pl (stem `withSuffix` "ērum"),
	dat_pl (stem `withSuffix` "ēbus"),
	acc_pl (stem `withSuffix` "ēs"),
	abl_pl (stem `withSuffix` "ēbus"),
	voc_pl (stem `withSuffix` "ēs")]
la_decl_5th_VOW _ = Nothing

