{-# LANGUAGE OverloadedStrings #-}

module Wiki.Latin.NounDecl(getNounInflections) where

import qualified Data.Text as T
import Data.Maybe

import Wiki.Types
import Wiki.PageParser

import Latin.Grammar

import Debug.Trace

getNounInflections :: [TemplateRef] -> Maybe (T.Text, [Inflection])
getNounInflections trefs = listToMaybe $ mapMaybe readNounInflectionTemplate trefs

readNounInflectionTemplate :: TemplateRef -> Maybe (T.Text, [Inflection])
readNounInflectionTemplate (TemplateRef []) = Nothing
readNounInflectionTemplate (TemplateRef [name])
	| name == "la-decl-agnus" = Just (name, la_decl_agnus)
	| name == "la-decl-bos" = Just (name, la_decl_bos)
	| name == "la-decl-deus" = Just (name, la_decl_deus)
	| name == "la-decl-domus" = Just (name, la_decl_domus)
	| name == "la-decl-iesus" = Just (name, la_decl_iesus)
	| name == "la-decl-sus" = Just (name, la_decl_sus)
	| name == "la-decl-venum" = Just (name, la_decl_venum)
	| name == "la-decl-vis" = Just (name, la_decl_vis)
	| otherwise = Nothing
readNounInflectionTemplate (TemplateRef (name:params))
	| name == "la-decl-1st" = wrapReadFunc la_decl_1st name params
	| name == "la-decl-1st-abus" = wrapReadFunc la_decl_1st_abus name params
	| name == "la-decl-1st-am" = wrapReadFunc la_decl_1st_am name params
	| name == "la-decl-1st-Greek" = wrapReadFunc la_decl_1st_Greek name params
	| name == "la-decl-1st-Greek-Ma" = wrapReadFunc la_decl_1st_Greek_Ma name params
	| name == "la-decl-1st-Greek-Me" = wrapReadFunc la_decl_1st_Greek_Me name params
	| name == "la-decl-1st-loc" = wrapReadFunc la_decl_1st_loc name params
	| name == "la-decl-2nd" = wrapReadFunc la_decl_2nd name params
	| name == "la-decl-2nd-er" = wrapReadFunc la_decl_2nd_er name params
	| name == "la-decl-2nd-Greek" = wrapReadFunc la_decl_2nd_Greek name params
	| name == "la-decl-2nd-ius" = wrapReadFunc la_decl_2nd_ius name params
	| name == "la-decl-2nd-loc" = wrapReadFunc la_decl_2nd_loc name params
	| name == "la-decl-2nd-N" = wrapReadFunc la_decl_2nd_N name params
	| name == "la-decl-2nd-N-Greek" = wrapReadFunc la_decl_2nd_N_Greek name params
	| name == "la-decl-2nd-N-loc" = wrapReadFunc la_decl_2nd_N_loc name params
	| name == "la-decl-2nd-N-us" = wrapReadFunc la_decl_2nd_N_us name params
	| name == "la-decl-3rd" = wrapReadFunc la_decl_3rd name params
	| name == "la-decl-3rd-Greek-er" = wrapReadFunc la_decl_3rd_Greek_er name params
	| name == "la-decl-3rd-I" = wrapReadFunc la_decl_3rd_I name params
	| name == "la-decl-3rd-I-ignis" = wrapReadFunc la_decl_3rd_I_ignis name params
	| name == "la-decl-3rd-I-loc" = wrapReadFunc la_decl_3rd_I_loc name params
	| name == "la-decl-3rd-I-navis" = wrapReadFunc la_decl_3rd_I_navis name params
	| name == "la-decl-3rd-loc" = wrapReadFunc la_decl_3rd_loc name params
	| name == "la-decl-3rd-N" = wrapReadFunc la_decl_3rd_N name params
	| name == "la-decl-3rd-N-a" = wrapReadFunc la_decl_3rd_N_a name params
	| name == "la-decl-3rd-N-I" = wrapReadFunc la_decl_3rd_N_I name params
	| name == "la-decl-3rd-N-I-pure" = wrapReadFunc la_decl_3rd_N_I_pure name params
	| name == "la-decl-4th" = wrapReadFunc la_decl_4th name params
	| name == "la-decl-4th-argo" = wrapReadFunc la_decl_4th_argo name params
	| name == "la-decl-4th-N" = wrapReadFunc la_decl_4th_N name params
	| name == "la-decl-4th-ubus" = wrapReadFunc la_decl_4th_ubus name params
	| name == "la-decl-5th-CONS" = wrapReadFunc la_decl_5th_CONS name params
	| name == "la-decl-5th-VOW" = wrapReadFunc la_decl_5th_VOW name params
	| otherwise = Nothing

wrapReadFunc :: ([T.Text] -> Maybe [Inflection]) -> T.Text -> [T.Text] -> Maybe (T.Text, [Inflection])
wrapReadFunc f name (p:ps) = do
	decls <- case T.stripPrefix (T.pack "num=") p of
		Nothing -> f (p:ps)
		Just n | n == (T.pack "sg") -> f ps >>= return.sgOnly
		Just n | n == (T.pack "pl") -> f ps >>= return.plOnly
		Just _ -> error $ "Unexpected num= parameter: " ++ (show p)
	return (name, decls)
	where
		sgOnly ds = [ d | d@(Inflection _ Singular _) <- ds ]
		plOnly ds = [ d | d@(Inflection _ Plural _) <- ds ]
wrapReadFunc f n xs = f xs >>= \x -> return (n, x)

withSuffix :: T.Text -> String -> T.Text
withSuffix a b = a `T.append` (T.pack b)

nom_sg, gen_sg, dat_sg, acc_sg, abl_sg, voc_sg, loc_sg, nom_pl, gen_pl, dat_pl, acc_pl, abl_pl, voc_pl, loc_pl :: T.Text -> Inflection
nom_sg = Inflection Nominative Singular
gen_sg = Inflection Genitive Singular
dat_sg = Inflection Dative Singular
acc_sg = Inflection Accusative Singular
abl_sg = Inflection Ablative Singular
voc_sg = Inflection Vocative Singular
loc_sg = Inflection Locative Singular
nom_pl = Inflection Nominative Plural
gen_pl = Inflection Genitive Plural
dat_pl = Inflection Dative Plural
acc_pl = Inflection Accusative Plural
abl_pl = Inflection Ablative Plural
voc_pl = Inflection Vocative Plural
loc_pl = Inflection Locative Plural

la_decl_1st, la_decl_1st_abus, la_decl_1st_am, la_decl_1st_Greek, la_decl_1st_Greek_Ma, la_decl_1st_Greek_Me, la_decl_1st_loc, la_decl_2nd, la_decl_2nd_er, la_decl_2nd_Greek, la_decl_2nd_N, la_decl_2nd_N_us, la_decl_2nd_N_Greek, la_decl_2nd_N_loc, la_decl_2nd_ius, la_decl_2nd_loc, la_decl_3rd, la_decl_3rd_Greek_er, la_decl_3rd_I, la_decl_3rd_I_ignis, la_decl_3rd_I_loc, la_decl_3rd_I_navis, la_decl_3rd_loc, la_decl_3rd_N, la_decl_3rd_N_a, la_decl_3rd_N_I, la_decl_3rd_N_I_pure, la_decl_4th, la_decl_4th_argo, la_decl_4th_N, la_decl_4th_ubus, la_decl_5th_CONS, la_decl_5th_VOW :: [T.Text] -> Maybe [Inflection]

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

la_decl_1st_am (stem:_) = Just [
	nom_sg (stem `withSuffix` "am"),
	gen_sg (stem `withSuffix` "ae"),
	dat_sg (stem `withSuffix` "ae"),
	acc_sg (stem `withSuffix` "am"),
	abl_sg (stem `withSuffix` "ā"),
	voc_sg (stem `withSuffix` "am"),
	nom_pl (stem `withSuffix` "ae"),
	gen_pl (stem `withSuffix` "ārum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "ās"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "ae")]
la_decl_1st_am _ = Nothing 

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

la_decl_2nd_er (stem:_) = Just [
	nom_sg stem,
	gen_sg (stem `withSuffix` "ī"),
	dat_sg (stem `withSuffix` "ō"),
	acc_sg (stem `withSuffix` "um"),
	abl_sg (stem `withSuffix` "ō"),
	voc_sg stem,
	nom_pl (stem `withSuffix` "ī"),
	gen_pl (stem `withSuffix` "ōrum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "ōs"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "ī")]
la_decl_2nd_er _ = Nothing

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

la_decl_2nd_N_us (stem:_) = Just [
	nom_sg (stem `withSuffix` "us"),
	gen_sg (stem `withSuffix` "ī"),
	dat_sg (stem `withSuffix` "ō"),
	acc_sg (stem `withSuffix` "us"),
	abl_sg (stem `withSuffix` "ō"),
	voc_sg (stem `withSuffix` "us")]
la_decl_2nd_N_us _ = Nothing

la_decl_2nd_N_Greek (stem:_) = Just [
	nom_sg (stem `withSuffix` "on"),
	gen_sg (stem `withSuffix` "ī"),
	dat_sg (stem `withSuffix` "ō"),
	acc_sg (stem `withSuffix` "on"),
	abl_sg (stem `withSuffix` "ō"),
	voc_sg (stem `withSuffix` "on"),
	nom_pl (stem `withSuffix` "a"),
	gen_pl (stem `withSuffix` "ōrum"),
	dat_pl (stem `withSuffix` "īs"),
	acc_pl (stem `withSuffix` "a"),
	abl_pl (stem `withSuffix` "īs"),
	voc_pl (stem `withSuffix` "a")]
la_decl_2nd_N_Greek _ = Nothing

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

la_decl_3rd_Greek_er (stem:_) = Just [
	nom_sg (stem `withSuffix` "ēr"),
	gen_sg (stem `withSuffix` "eris"),
	dat_sg (stem `withSuffix` "erī"),
	acc_sg (stem `withSuffix` "era"),
	abl_sg (stem `withSuffix` "ere"),
	voc_sg (stem `withSuffix` "ēr"),
	nom_pl (stem `withSuffix` "erēs"),
	gen_pl (stem `withSuffix` "erum"),
	dat_pl (stem `withSuffix` "eribus"),
	acc_pl (stem `withSuffix` "erēs"),
	abl_pl (stem `withSuffix` "eribus"),
	voc_pl (stem `withSuffix` "erēs")]
la_decl_3rd_Greek_er _ = Nothing

la_decl_3rd_I (nom:stem:_) = Just [
	nom_sg nom,
	gen_sg (stem `withSuffix` "is"),
	dat_sg (stem `withSuffix` "ī"),
	acc_sg (stem `withSuffix` "em"),
	abl_sg (stem `withSuffix` "e"),
	voc_sg nom,
	nom_pl (stem `withSuffix` "ēs"),
	gen_pl (stem `withSuffix` "ium"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "ēs"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "ēs")]
la_decl_3rd_I _ = Nothing

la_decl_3rd_I_ignis (nom:stem:_) = Just [
	nom_sg nom,
	gen_sg (stem `withSuffix` "is"),
	dat_sg (stem `withSuffix` "ī"),
	acc_sg (stem `withSuffix` "em"),
	abl_sg (stem `withSuffix` "ī"),
	voc_sg nom,
	nom_pl (stem `withSuffix` "ēs"),
	gen_pl (stem `withSuffix` "ium"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "īs"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "ēs")]
la_decl_3rd_I_ignis _ = Nothing

la_decl_3rd_I_loc (nom:stem:_) = Just [
	nom_sg nom,
	gen_sg (stem `withSuffix` "is"),
	dat_sg (stem `withSuffix` "ī"),
	acc_sg (stem `withSuffix` "em"),
	abl_sg (stem `withSuffix` "e"),
	voc_sg nom,
	loc_sg (stem `withSuffix` "e"),
	nom_pl (stem `withSuffix` "ēs"),
	gen_pl (stem `withSuffix` "ium"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "ēs"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "ēs"),
	loc_pl (stem `withSuffix` "ēs")]
la_decl_3rd_I_loc _ = Nothing

la_decl_3rd_I_navis (nom:stem:_) = Just [
	nom_sg nom,
	gen_sg (stem `withSuffix` "is"),
	dat_sg (stem `withSuffix` "ī"),
	acc_sg (stem `withSuffix` "im"),
	abl_sg (stem `withSuffix` "ī"),
	voc_sg nom,
	nom_pl (stem `withSuffix` "ēs"),
	gen_pl (stem `withSuffix` "ium"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "īs"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "ēs")]
la_decl_3rd_I_navis _ = Nothing

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

-- TODO: This one actually has a 1st decl variant, too.
la_decl_3rd_N_a (stem:_) = Just [
	nom_sg (stem `withSuffix` "a"),
	gen_sg (stem `withSuffix` "atis"),
	dat_sg (stem `withSuffix` "atī"),
	acc_sg (stem `withSuffix` "a"),
	abl_sg (stem `withSuffix` "ate"),
	voc_sg (stem `withSuffix` "a"),
	nom_pl (stem `withSuffix` "ata"),
	gen_pl (stem `withSuffix` "atum"),
	dat_pl (stem `withSuffix` "atibus"),
	acc_pl (stem `withSuffix` "ata"),
	abl_pl (stem `withSuffix` "atibus"),
	voc_pl (stem `withSuffix` "ata")]
la_decl_3rd_N_a _ = Nothing

la_decl_3rd_N_I (nom:stem:_) = Just [
	nom_sg nom,
	gen_sg (stem `withSuffix` "is"),
	dat_sg (stem `withSuffix` "ī"),
	acc_sg nom,
	abl_sg (stem `withSuffix` "e"),
	voc_sg nom,
	nom_pl (stem `withSuffix` "a"),
	gen_pl (stem `withSuffix` "ium"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "a"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "a")]
la_decl_3rd_N_I _ = Nothing

la_decl_3rd_N_I_pure (nom:stem:_) = Just [
	nom_sg nom,
	gen_sg (stem `withSuffix` "is"),
	dat_sg (stem `withSuffix` "ī"),
	acc_sg nom,
	abl_sg (stem `withSuffix` "e"),
	voc_sg nom,
	nom_pl (stem `withSuffix` "ia"),
	gen_pl (stem `withSuffix` "ium"),
	dat_pl (stem `withSuffix` "ibus"),
	acc_pl (stem `withSuffix` "ia"),
	abl_pl (stem `withSuffix` "ibus"),
	voc_pl (stem `withSuffix` "a")]
la_decl_3rd_N_I_pure _ = Nothing

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

la_decl_4th_argo (stem:_) = Just [
	nom_sg (stem `withSuffix` "ō"),
	gen_sg (stem `withSuffix` "ūs"),
	dat_sg (stem `withSuffix` "uī"),
	acc_sg (stem `withSuffix` "ō"),
	abl_sg (stem `withSuffix` "uī"),
	voc_sg (stem `withSuffix` "ō")]
la_decl_4th_argo _ = Nothing

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

la_decl_4th_ubus (stem:_) = Just [
	nom_sg (stem `withSuffix` "us"),
	gen_sg (stem `withSuffix` "ūs"),
	dat_sg (stem `withSuffix` "uī"),
	acc_sg (stem `withSuffix` "um"),
	abl_sg (stem `withSuffix` "ū"),
	voc_sg (stem `withSuffix` "us"),
	nom_pl (stem `withSuffix` "ūs"),
	gen_pl (stem `withSuffix` "uum"),
	dat_pl (stem `withSuffix` "ubus"),
	acc_pl (stem `withSuffix` "ūs"),
	abl_pl (stem `withSuffix` "ubus"),
	voc_pl (stem `withSuffix` "ūs")]
la_decl_4th_ubus _ = Nothing

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

la_decl_agnus, la_decl_bos, la_decl_deus, la_decl_domus, la_decl_iesus, la_decl_sus, la_decl_venum, la_decl_vis :: [Inflection]

la_decl_agnus = [
	nom_sg "agnus",
	gen_sg "agnnī",
	dat_sg "agnō",
	acc_sg "agnum",
	abl_sg "agnō",
	voc_sg "agne",
	nom_pl "agnī",
	gen_pl "agnūs",
	dat_pl "agnīs",
	acc_pl "agnōs",
	abl_pl "agnīs",
	voc_pl "agnī" ]

la_decl_bos = [
	nom_sg "bōs",
	gen_sg "bovis",
	dat_sg "bovī",
	acc_sg "bovem",
	abl_sg "bove",
	voc_sg "bōs",
	nom_pl "bovēs",
	gen_pl "boum",
	dat_pl "bōbus",
	acc_pl "bovēs",
	abl_pl "bōbus",
	voc_pl "bovēs" ]

la_decl_deus = [
	nom_sg "deus",
	gen_sg "deī",
	dat_sg "deō",
	acc_sg "deum",
	abl_sg "deō",
	voc_sg "deus",
	nom_pl "dī",
	gen_pl "deorum",
	dat_pl "dīs",
	acc_pl "denōs",
	abl_pl "dīs",
	voc_pl "dī"]

la_decl_domus = [
	nom_sg  "domus",
	gen_sg  "domūs",
	dat_sg  "domuī",
	acc_sg  "domum",
	abl_sg  "domū",
	voc_sg  "domī",
	nom_pl  "domūs",
	gen_pl  "domuum",
	dat_pl  "domibus",
	acc_pl  "domūs",
	abl_pl  "domibus",
	voc_pl  "domīs"]

la_decl_iesus = [
	nom_sg  "Iēsus",
	gen_sg  "Iēsū",
	dat_sg  "Iēsū",
	acc_sg  "Iēsum",
	abl_sg  "Iēsū",
	voc_sg  "Iēsū"]

la_decl_sus = [
	nom_sg  "sūs",
	gen_sg  "sū",
	dat_sg  "sū",
	acc_sg  "sum",
	abl_sg  "sū",
	voc_sg  "sū"]

la_decl_venum = [
	dat_sg "vēnuī",
	acc_sg "vēnum"]

la_decl_vis = [
	nom_sg  "vīs",
	gen_sg  "vīs",
	dat_sg  "vī",
	acc_sg  "vim",
	abl_sg  "vī",
	voc_sg  "vīs",
	nom_pl  "vīrēs",
	gen_pl  "vīrium",
	dat_pl  "vīribus",
	acc_pl  "vīrēs",
	abl_pl  "vīribus",
	voc_pl  "vīrēs"]
