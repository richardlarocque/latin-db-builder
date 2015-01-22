{-
Copyright (C) 2014 Richard Larocque <richard.larocque@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Wiki.Latin.AdjectiveDecl(getAdjectiveInflections) where

import qualified Data.Text as T
import Data.Maybe

import Latin.Grammar
import Wiki.Types
import Wiki.PageParser

getAdjectiveInflections :: [TemplateRef] -> Maybe (T.Text, [GenderedInflection])
getAdjectiveInflections trefs = listToMaybe $ mapMaybe readInflectionTemplates trefs

readInflectionTemplates :: TemplateRef -> Maybe (T.Text, [GenderedInflection])
readInflectionTemplates (TemplateRef (name:params))
	| name == (T.pack "la-decl-1&2") = wrapReadFunc la_decl_12 name params
	| name == (T.pack "la-decl-1&2-ius") = wrapReadFunc la_decl_12ius name params
	| name == (T.pack "la-decl-1&2-plural") = wrapReadFunc la_decl_12_plural name params
	| name == (T.pack "la-decl-3rd-1E") = wrapReadFunc la_decl_3_1E name params
	| name == (T.pack "la-decl-3rd-1E-PAR") = wrapReadFunc la_decl_3_1E_PAR name params
	| name == (T.pack "la-decl-3rd-2E") = wrapReadFunc la_decl_3_2E name params
	| name == (T.pack "la-decl-3rd-2E-pl") = wrapReadFunc la_decl_3_2E_pl name params
	| name == (T.pack "la-decl-3rd-3E") = wrapReadFunc la_decl_3_3E name params
readInflectionTemplates (TemplateRef _) = Nothing

wrapReadFunc :: ([T.Text] -> Maybe [GenderedInflection]) -> T.Text -> [T.Text] -> Maybe (T.Text, [GenderedInflection])
wrapReadFunc f n xs = f xs >>= \x -> return (n, x)

nom_sg_m = GenderedInflection Nominative Singular Masculine
gen_sg_m = GenderedInflection Genitive Singular Masculine
dat_sg_m = GenderedInflection Dative Singular Masculine
acc_sg_m = GenderedInflection Accusative Singular Masculine
abl_sg_m = GenderedInflection Ablative Singular Masculine
voc_sg_m = GenderedInflection Vocative Singular Masculine
nom_pl_m = GenderedInflection Nominative Plural Masculine
gen_pl_m = GenderedInflection Genitive Plural Masculine
dat_pl_m = GenderedInflection Dative Plural Masculine
acc_pl_m = GenderedInflection Accusative Plural Masculine
abl_pl_m = GenderedInflection Ablative Plural Masculine
voc_pl_m = GenderedInflection Vocative Plural Masculine

nom_sg_f = GenderedInflection Nominative Singular Feminine
gen_sg_f = GenderedInflection Genitive Singular Feminine
dat_sg_f = GenderedInflection Dative Singular Feminine
acc_sg_f = GenderedInflection Accusative Singular Feminine
abl_sg_f = GenderedInflection Ablative Singular Feminine
voc_sg_f = GenderedInflection Vocative Singular Feminine
nom_pl_f = GenderedInflection Nominative Plural Feminine
gen_pl_f = GenderedInflection Genitive Plural Feminine
dat_pl_f = GenderedInflection Dative Plural Feminine
acc_pl_f = GenderedInflection Accusative Plural Feminine
abl_pl_f = GenderedInflection Ablative Plural Feminine
voc_pl_f = GenderedInflection Vocative Plural Feminine

nom_sg_n = GenderedInflection Nominative Singular Neuter
gen_sg_n = GenderedInflection Genitive Singular Neuter
dat_sg_n = GenderedInflection Dative Singular Neuter
acc_sg_n = GenderedInflection Accusative Singular Neuter
abl_sg_n = GenderedInflection Ablative Singular Neuter
voc_sg_n = GenderedInflection Vocative Singular Neuter
nom_pl_n = GenderedInflection Nominative Plural Neuter
gen_pl_n = GenderedInflection Genitive Plural Neuter
dat_pl_n = GenderedInflection Dative Plural Neuter
acc_pl_n = GenderedInflection Accusative Plural Neuter
abl_pl_n = GenderedInflection Ablative Plural Neuter
voc_pl_n = GenderedInflection Vocative Plural Neuter

nom_sg_m, gen_sg_m, dat_sg_m, acc_sg_m, abl_sg_m, voc_sg_m, nom_pl_m, gen_pl_m, dat_pl_m, acc_pl_m, abl_pl_m, voc_pl_m, nom_sg_f, gen_sg_f, dat_sg_f, acc_sg_f, abl_sg_f, voc_sg_f, nom_pl_f, gen_pl_f, dat_pl_f, acc_pl_f, abl_pl_f, voc_pl_f, nom_sg_n, gen_sg_n, dat_sg_n, acc_sg_n, abl_sg_n, voc_sg_n, nom_pl_n, gen_pl_n, dat_pl_n, acc_pl_n, abl_pl_n, voc_pl_n :: T.Text -> GenderedInflection

withSuffix :: T.Text -> String -> T.Text
withSuffix a b = a `T.append` (T.pack b)

la_decl_12, la_decl_12ius, la_decl_12_plural, la_decl_3_1E, la_decl_3_1E_PAR, la_decl_3_2E, la_decl_3_2E_pl, la_decl_3_3E :: [T.Text] -> Maybe [GenderedInflection]

la_decl_12', la_decl_12ius', la_decl_12_plural', la_decl_3_2E', la_decl_3_2E_pl' :: T.Text -> Maybe [GenderedInflection]

la_decl_3_1E', la_decl_3_1E_PAR', la_decl_3_3E' :: T.Text -> T.Text -> Maybe [GenderedInflection]

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

la_decl_12ius [_,stem] = la_decl_12ius' stem
la_decl_12ius [stem] = la_decl_12ius' stem
la_decl_12ius _ = Nothing

la_decl_12ius' stem = Just [
	nom_sg_m (stem `withSuffix` "us"),
	gen_sg_m (stem `withSuffix` "īus"),
	dat_sg_m (stem `withSuffix` "ī"),
	acc_sg_m (stem `withSuffix` "um"),
	abl_sg_m (stem `withSuffix` "ō"),
	voc_sg_m (stem `withSuffix` "e"),

	nom_sg_f (stem `withSuffix` "a"),
	gen_sg_f (stem `withSuffix` "īus"),
	dat_sg_f (stem `withSuffix` "ī"),
	acc_sg_f (stem `withSuffix` "am"),
	abl_sg_f (stem `withSuffix` "ā"),
	voc_sg_f (stem `withSuffix` "a"),

	nom_sg_n (stem `withSuffix` "um"),
	gen_sg_n (stem `withSuffix` "īus"),
	dat_sg_n (stem `withSuffix` "ī"),
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
