module Wiki.Latin.AdjectiveDecl(getAdjectiveInflections) where

import qualified Data.Text as T
import Data.Maybe

import Latin.Declension
import Wiki.Types
import Wiki.PageParser

getAdjectiveInflections :: [TemplateRef] -> Maybe (T.Text, [InflectedAdjective])
getAdjectiveInflections trefs = listToMaybe $ mapMaybe readInflectionTemplates trefs

readInflectionTemplates :: TemplateRef -> Maybe (T.Text, [InflectedAdjective])
readInflectionTemplates (TemplateRef (name:params))
	| name == (T.pack "la-decl-1&2") = wrapReadFunc la_decl_12 name params
	| name == (T.pack "la-decl-1&2-plural") = wrapReadFunc la_decl_12_plural name params
	| name == (T.pack "la-decl-3rd-1E") = wrapReadFunc la_decl_3_1E name params
	| name == (T.pack "la-decl-3rd-1E-PAR") = wrapReadFunc la_decl_3_1E_PAR name params
	| name == (T.pack "la-decl-3rd-2E") = wrapReadFunc la_decl_3_2E name params
	| name == (T.pack "la-decl-3rd-2E-pl") = wrapReadFunc la_decl_3_2E_pl name params
	| name == (T.pack "la-decl-3rd-3E") = wrapReadFunc la_decl_3_3E name params
readInflectionTemplates (TemplateRef _) = Nothing

wrapReadFunc :: ([T.Text] -> Maybe [InflectedAdjective]) -> T.Text -> [T.Text] -> Maybe (T.Text, [InflectedAdjective])
wrapReadFunc f n xs = f xs >>= \x -> return (n, x)

nom_sg_m = InflectedAdjective Nominative Singular Masculine
gen_sg_m = InflectedAdjective Genitive Singular Masculine
dat_sg_m = InflectedAdjective Dative Singular Masculine
acc_sg_m = InflectedAdjective Accusative Singular Masculine
abl_sg_m = InflectedAdjective Ablative Singular Masculine
voc_sg_m = InflectedAdjective Vocative Singular Masculine
nom_pl_m = InflectedAdjective Nominative Plural Masculine
gen_pl_m = InflectedAdjective Genitive Plural Masculine
dat_pl_m = InflectedAdjective Dative Plural Masculine
acc_pl_m = InflectedAdjective Accusative Plural Masculine
abl_pl_m = InflectedAdjective Ablative Plural Masculine
voc_pl_m = InflectedAdjective Vocative Plural Masculine

nom_sg_f = InflectedAdjective Nominative Singular Feminine
gen_sg_f = InflectedAdjective Genitive Singular Feminine
dat_sg_f = InflectedAdjective Dative Singular Feminine
acc_sg_f = InflectedAdjective Accusative Singular Feminine
abl_sg_f = InflectedAdjective Ablative Singular Feminine
voc_sg_f = InflectedAdjective Vocative Singular Feminine
nom_pl_f = InflectedAdjective Nominative Plural Feminine
gen_pl_f = InflectedAdjective Genitive Plural Feminine
dat_pl_f = InflectedAdjective Dative Plural Feminine
acc_pl_f = InflectedAdjective Accusative Plural Feminine
abl_pl_f = InflectedAdjective Ablative Plural Feminine
voc_pl_f = InflectedAdjective Vocative Plural Feminine

nom_sg_n = InflectedAdjective Nominative Singular Neuter
gen_sg_n = InflectedAdjective Genitive Singular Neuter
dat_sg_n = InflectedAdjective Dative Singular Neuter
acc_sg_n = InflectedAdjective Accusative Singular Neuter
abl_sg_n = InflectedAdjective Ablative Singular Neuter
voc_sg_n = InflectedAdjective Vocative Singular Neuter
nom_pl_n = InflectedAdjective Nominative Plural Neuter
gen_pl_n = InflectedAdjective Genitive Plural Neuter
dat_pl_n = InflectedAdjective Dative Plural Neuter
acc_pl_n = InflectedAdjective Accusative Plural Neuter
abl_pl_n = InflectedAdjective Ablative Plural Neuter
voc_pl_n = InflectedAdjective Vocative Plural Neuter

nom_sg_m, gen_sg_m, dat_sg_m, acc_sg_m, abl_sg_m, voc_sg_m, nom_pl_m, gen_pl_m, dat_pl_m, acc_pl_m, abl_pl_m, voc_pl_m, nom_sg_f, gen_sg_f, dat_sg_f, acc_sg_f, abl_sg_f, voc_sg_f, nom_pl_f, gen_pl_f, dat_pl_f, acc_pl_f, abl_pl_f, voc_pl_f, nom_sg_n, gen_sg_n, dat_sg_n, acc_sg_n, abl_sg_n, voc_sg_n, nom_pl_n, gen_pl_n, dat_pl_n, acc_pl_n, abl_pl_n, voc_pl_n :: T.Text -> InflectedAdjective

withSuffix :: T.Text -> String -> T.Text
withSuffix a b = a `T.append` (T.pack b)

la_decl_12, la_decl_12_plural, la_decl_3_1E, la_decl_3_1E_PAR, la_decl_3_2E, la_decl_3_2E_pl, la_decl_3_3E :: [T.Text] -> Maybe [InflectedAdjective]

la_decl_12', la_decl_12_plural', la_decl_3_2E', la_decl_3_2E_pl' :: T.Text -> Maybe [InflectedAdjective]

la_decl_3_1E', la_decl_3_1E_PAR', la_decl_3_3E' :: T.Text -> T.Text -> Maybe [InflectedAdjective]

la_decl_12 [_,stem] = la_decl_12' stem
la_decl_12 [stem] = la_decl_12' stem
la_decl_12 _ = Nothing

la_decl_12' stem = Just [
	nom_sg_m (stem `withSuffix` "us"),
	gen_sg_m (stem `withSuffix` "ī"),
	dat_sg_m (stem `withSuffix` "ō"),
	acc_sg_m (stem `withSuffix` "um"),
	abl_sg_m (stem `withSuffix` "ō"),
	voc_sg_m (stem `withSuffix` "e"),

	nom_sg_f (stem `withSuffix` "a"),
	gen_sg_f (stem `withSuffix` "ae"),
	dat_sg_f (stem `withSuffix` "ae"),
	acc_sg_f (stem `withSuffix` "am"),
	abl_sg_f (stem `withSuffix` "ā"),
	voc_sg_f (stem `withSuffix` "a"),

	nom_sg_n (stem `withSuffix` "um"),
	gen_sg_n (stem `withSuffix` "ī"),
	dat_sg_n (stem `withSuffix` "ō"),
	acc_sg_n (stem `withSuffix` "um"),
	abl_sg_n (stem `withSuffix` "ō"),
	voc_sg_n (stem `withSuffix` "um"),

	nom_pl_m (stem `withSuffix` "ī"),
	gen_pl_m (stem `withSuffix` "ōrum"),
	dat_pl_m (stem `withSuffix` "īs"),
	acc_pl_m (stem `withSuffix` "ōs"),
	abl_pl_m (stem `withSuffix` "īs"),
	voc_pl_m (stem `withSuffix` "ī"),

	nom_pl_f (stem `withSuffix` "ae"),
	gen_pl_f (stem `withSuffix` "ārum"),
	dat_pl_f (stem `withSuffix` "īs"),
	acc_pl_f (stem `withSuffix` "ās"),
	abl_pl_f (stem `withSuffix` "īs"),
	voc_pl_f (stem `withSuffix` "ae"),

	nom_pl_n (stem `withSuffix` "a"),
	gen_pl_n (stem `withSuffix` "ōrum"),
	dat_pl_n (stem `withSuffix` "īs"),
	acc_pl_n (stem `withSuffix` "a"),
	abl_pl_n (stem `withSuffix` "īs"),
	voc_pl_n (stem `withSuffix` "a")]

la_decl_12_plural [_,stem] = la_decl_12_plural' stem
la_decl_12_plural [stem] = la_decl_12_plural' stem
la_decl_12_plural _ = Nothing

la_decl_12_plural' stem = Just [
	nom_pl_m (stem `withSuffix` "ī"),
	nom_pl_f (stem `withSuffix` "ae"),
	nom_pl_n (stem `withSuffix` "a"),
	gen_pl_m (stem `withSuffix` "ōrum"),
	gen_pl_f (stem `withSuffix` "ārum"),
	gen_pl_n (stem `withSuffix` "ōrum"),
	dat_pl_m (stem `withSuffix` "īs"),
	dat_pl_f (stem `withSuffix` "īs"),
	dat_pl_n (stem `withSuffix` "īs"),
	acc_pl_m (stem `withSuffix` "ōs"),
	acc_pl_f (stem `withSuffix` "ās"),
	acc_pl_n (stem `withSuffix` "a"),
	abl_pl_m (stem `withSuffix` "īs"),
	abl_pl_f (stem `withSuffix` "īs"),
	abl_pl_n (stem `withSuffix` "īs"),
	voc_pl_m (stem `withSuffix` "ī"),
	voc_pl_f (stem `withSuffix` "ae"),
	voc_pl_n (stem `withSuffix` "a")]

la_decl_3_1E [nom,stem] = la_decl_3_1E' nom stem
la_decl_3_1E [stem] = la_decl_3_1E' stem stem
la_decl_3_1E _ = Nothing

la_decl_3_1E' nom stem = Just [
	nom_sg_m nom,
	gen_sg_m (stem `withSuffix` "is"),
	dat_sg_m (stem `withSuffix` "ī"),
	acc_sg_m (stem `withSuffix` "em"),
	abl_sg_m (stem `withSuffix` "ī"),
	voc_sg_m nom,

	nom_sg_f nom,
	gen_sg_f (stem `withSuffix` "is"),
	dat_sg_f (stem `withSuffix` "ī"),
	acc_sg_f (stem `withSuffix` "em"),
	abl_sg_f (stem `withSuffix` "ī"),
	voc_sg_f nom,

	nom_sg_n nom,
	gen_sg_n (stem `withSuffix` "is"),
	dat_sg_n (stem `withSuffix` "ī"),
	acc_sg_n nom,
	abl_sg_n (stem `withSuffix` "ī"),
	voc_sg_n nom,

	nom_pl_m (stem `withSuffix` "ēs"),
	gen_pl_m (stem `withSuffix` "ium"),
	dat_pl_m (stem `withSuffix` "ibus"),
	acc_pl_m (stem `withSuffix` "ēs"),
	abl_pl_m (stem `withSuffix` "ibus"),
	voc_pl_m (stem `withSuffix` "ēs"),

	nom_pl_f (stem `withSuffix` "ēs"),
	gen_pl_f (stem `withSuffix` "ium"),
	dat_pl_f (stem `withSuffix` "ibus"),
	acc_pl_f (stem `withSuffix` "ēs"),
	abl_pl_f (stem `withSuffix` "ibus"),
	voc_pl_f (stem `withSuffix` "ēs"),

	nom_pl_n (stem `withSuffix` "ia"),
	gen_pl_n (stem `withSuffix` "ium"),
	dat_pl_n (stem `withSuffix` "ibus"),
	acc_pl_n (stem `withSuffix` "ia"),
	abl_pl_n (stem `withSuffix` "ibus"),
	voc_pl_n (stem `withSuffix` "ia")]

la_decl_3_1E_PAR [nom,stem] = la_decl_3_1E_PAR' nom stem
la_decl_3_1E_PAR [stem] = la_decl_3_1E_PAR' stem stem
la_decl_3_1E_PAR _ = Nothing

la_decl_3_1E_PAR' nom stem = Just [
	nom_sg_m nom,
	gen_sg_m (stem `withSuffix` "is"),
	dat_sg_m (stem `withSuffix` "ī"),
	acc_sg_m (stem `withSuffix` "em"),
	abl_sg_m (stem `withSuffix` "e"),
	voc_sg_m nom,

	nom_sg_f nom,
	gen_sg_f (stem `withSuffix` "is"),
	dat_sg_f (stem `withSuffix` "ī"),
	acc_sg_f (stem `withSuffix` "em"),
	abl_sg_f (stem `withSuffix` "e"),
	voc_sg_f nom,

	nom_sg_n nom,
	gen_sg_n (stem `withSuffix` "is"),
	dat_sg_n (stem `withSuffix` "ī"),
	acc_sg_n nom,
	abl_sg_n (stem `withSuffix` "e"),
	voc_sg_n nom,

	nom_pl_m (stem `withSuffix` "ēs"),
	gen_pl_m (stem `withSuffix` "um"),
	dat_pl_m (stem `withSuffix` "ibus"),
	acc_pl_m (stem `withSuffix` "ēs"),
	abl_pl_m (stem `withSuffix` "ibus"),
	voc_pl_m (stem `withSuffix` "ēs"),

	nom_pl_f (stem `withSuffix` "ēs"),
	gen_pl_f (stem `withSuffix` "um"),
	dat_pl_f (stem `withSuffix` "ibus"),
	acc_pl_f (stem `withSuffix` "ēs"),
	abl_pl_f (stem `withSuffix` "ibus"),
	voc_pl_f (stem `withSuffix` "ēs"),

	nom_pl_n (stem `withSuffix` "a"),
	gen_pl_n (stem `withSuffix` "um"),
	dat_pl_n (stem `withSuffix` "ibus"),
	acc_pl_n (stem `withSuffix` "a"),
	abl_pl_n (stem `withSuffix` "ibus"),
	voc_pl_n (stem `withSuffix` "a")]

la_decl_3_2E [_,stem] = la_decl_3_2E' stem
la_decl_3_2E [stem] = la_decl_3_2E' stem
la_decl_3_2E _ = Nothing

la_decl_3_2E' stem = Just [
	nom_sg_m (stem `withSuffix` "is"),
	gen_sg_m (stem `withSuffix` "is"),
	dat_sg_m (stem `withSuffix` "ī"),
	acc_sg_m (stem `withSuffix` "em"),
	abl_sg_m (stem `withSuffix` "ī"),
	voc_sg_m (stem `withSuffix` "is"),

	nom_sg_f (stem `withSuffix` "is"),
	gen_sg_f (stem `withSuffix` "is"),
	dat_sg_f (stem `withSuffix` "ī"),
	acc_sg_f (stem `withSuffix` "em"),
	abl_sg_f (stem `withSuffix` "ī"),
	voc_sg_f (stem `withSuffix` "is"),

	nom_sg_n (stem `withSuffix` "e"),
	gen_sg_n (stem `withSuffix` "is"),
	dat_sg_n (stem `withSuffix` "ī"),
	acc_sg_n (stem `withSuffix` "e"),
	abl_sg_n (stem `withSuffix` "ī"),
	voc_sg_n (stem `withSuffix` "e"),

	nom_pl_m (stem `withSuffix` "ēs"),
	gen_pl_m (stem `withSuffix` "ium"),
	dat_pl_m (stem `withSuffix` "ibus"),
	acc_pl_m (stem `withSuffix` "ēs"),
	abl_pl_m (stem `withSuffix` "ibus"),
	voc_pl_m (stem `withSuffix` "ēs"),

	nom_pl_f (stem `withSuffix` "ēs"),
	gen_pl_f (stem `withSuffix` "ium"),
	dat_pl_f (stem `withSuffix` "ibus"),
	acc_pl_f (stem `withSuffix` "ēs"),
	abl_pl_f (stem `withSuffix` "ibus"),
	voc_pl_f (stem `withSuffix` "ēs"),

	nom_pl_n (stem `withSuffix` "ia"),
	gen_pl_n (stem `withSuffix` "ium"),
	dat_pl_n (stem `withSuffix` "ibus"),
	acc_pl_n (stem `withSuffix` "ia"),
	abl_pl_n (stem `withSuffix` "ibus"),
	voc_pl_n (stem `withSuffix` "ia")]

la_decl_3_2E_pl [stem] = la_decl_3_2E_pl' stem
la_decl_3_2E_pl [_,stem] = la_decl_3_2E_pl' stem
la_decl_3_2E_pl _ = Nothing

la_decl_3_2E_pl' stem = Just [
	nom_pl_m (stem `withSuffix` "ēs"),
	nom_pl_f (stem `withSuffix` "ēs"),
	nom_pl_n (stem `withSuffix` "ia"),
	gen_pl_m (stem `withSuffix` "ium"),
	gen_pl_f (stem `withSuffix` "ium"),
	gen_pl_n (stem `withSuffix` "ium"),
	dat_pl_m (stem `withSuffix` "ibus"),
	dat_pl_f (stem `withSuffix` "ibus"),
	dat_pl_n (stem `withSuffix` "ibus"),
	acc_pl_m (stem `withSuffix` "ēs"),
	acc_pl_f (stem `withSuffix` "ēs"),
	acc_pl_n (stem `withSuffix` "ia"),
	abl_pl_m (stem `withSuffix` "ibus"),
	abl_pl_f (stem `withSuffix` "ibus"),
	abl_pl_n (stem `withSuffix` "ibus"),
	voc_pl_m (stem `withSuffix` "ēs"),
	voc_pl_f (stem `withSuffix` "ēs"),
	voc_pl_n (stem `withSuffix` "ia")]

la_decl_3_3E [stem] = la_decl_3_3E' stem stem
la_decl_3_3E [nom,stem] = la_decl_3_3E' nom stem
la_decl_3_3E _ = Nothing

la_decl_3_3E' nom stem = Just $ [
	nom_sg_m nom,
	gen_sg_m (stem `withSuffix` "is"),
	dat_sg_m (stem `withSuffix` "ī"),
	acc_sg_m (stem `withSuffix` "em"),
	abl_sg_m (stem `withSuffix` "ī"),
	voc_sg_m nom,

	nom_sg_f (stem `withSuffix` "is"),
	gen_sg_f (stem `withSuffix` "is"),
	dat_sg_f (stem `withSuffix` "ī"),
	acc_sg_f (stem `withSuffix` "em"),
	abl_sg_f (stem `withSuffix` "ī"),
	voc_sg_f (stem `withSuffix` "is"),

	nom_sg_n (stem `withSuffix` "e"),
	gen_sg_n (stem `withSuffix` "is"),
	dat_sg_n (stem `withSuffix` "ī"),
	acc_sg_n (stem `withSuffix` "e"),
	abl_sg_n (stem `withSuffix` "ī"),
	voc_sg_n (stem `withSuffix` "e"),

	nom_pl_m (stem `withSuffix` "ēs"),
	gen_pl_m (stem `withSuffix` "ium"),
	dat_pl_m (stem `withSuffix` "ibus"),
	acc_pl_m (stem `withSuffix` "ēs"),
	abl_pl_m (stem `withSuffix` "ibus"),
	voc_pl_m (stem `withSuffix` "ēs"),

	nom_pl_f (stem `withSuffix` "ēs"),
	gen_pl_f (stem `withSuffix` "ium"),
	dat_pl_f (stem `withSuffix` "ibus"),
	acc_pl_f (stem `withSuffix` "ēs"),
	abl_pl_f (stem `withSuffix` "ibus"),
	voc_pl_f (stem `withSuffix` "ēs"),

	nom_pl_n (stem `withSuffix` "ia"),
	gen_pl_n (stem `withSuffix` "ium"),
	dat_pl_n (stem `withSuffix` "ibus"),
	acc_pl_n (stem `withSuffix` "ia"),
	abl_pl_n (stem `withSuffix` "ibus"),
	voc_pl_n (stem `withSuffix` "ia")]
