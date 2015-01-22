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

{-# LANGUAGE OverloadedStrings #-}

module Wiki.Latin.Pronouns(pronoun_entries) where

import qualified Data.Text as T

import Latin.Grammar
import Latin.Types

nom_sg_m, gen_sg_m, dat_sg_m, acc_sg_m, abl_sg_m, nom_pl_m, gen_pl_m, dat_pl_m, acc_pl_m, abl_pl_m, nom_sg_f, gen_sg_f, dat_sg_f, acc_sg_f, abl_sg_f, nom_pl_f, gen_pl_f, dat_pl_f, acc_pl_f, abl_pl_f, nom_sg_n, gen_sg_n, dat_sg_n, acc_sg_n, abl_sg_n, nom_pl_n, gen_pl_n, dat_pl_n, acc_pl_n, abl_pl_n :: T.Text -> GenderedInflection

nom_sg_m v = GenderedInflection Nominative Singular Masculine v
gen_sg_m v = GenderedInflection Genitive Singular Masculine v
dat_sg_m v = GenderedInflection Dative Singular Masculine v
acc_sg_m v = GenderedInflection Accusative Singular Masculine v
abl_sg_m v = GenderedInflection Ablative Singular Masculine v
nom_pl_m v = GenderedInflection Nominative Plural Masculine v
gen_pl_m v = GenderedInflection Genitive Plural Masculine v
dat_pl_m v = GenderedInflection Dative Plural Masculine v
acc_pl_m v = GenderedInflection Accusative Plural Masculine v
abl_pl_m v = GenderedInflection Ablative Plural Masculine v
nom_sg_f v = GenderedInflection Nominative Singular Feminine v
gen_sg_f v = GenderedInflection Genitive Singular Feminine v
dat_sg_f v = GenderedInflection Dative Singular Feminine v
acc_sg_f v = GenderedInflection Accusative Singular Feminine v
abl_sg_f v = GenderedInflection Ablative Singular Feminine v
nom_pl_f v = GenderedInflection Nominative Plural Feminine v
gen_pl_f v = GenderedInflection Genitive Plural Feminine v
dat_pl_f v = GenderedInflection Dative Plural Feminine v
acc_pl_f v = GenderedInflection Accusative Plural Feminine v
abl_pl_f v = GenderedInflection Ablative Plural Feminine v
nom_sg_n v = GenderedInflection Nominative Singular Neuter v
gen_sg_n v = GenderedInflection Genitive Singular Neuter v
dat_sg_n v = GenderedInflection Dative Singular Neuter v
acc_sg_n v = GenderedInflection Accusative Singular Neuter v
abl_sg_n v = GenderedInflection Ablative Singular Neuter v
nom_pl_n v = GenderedInflection Nominative Plural Neuter v
gen_pl_n v = GenderedInflection Genitive Plural Neuter v
dat_pl_n v = GenderedInflection Dative Plural Neuter v
acc_pl_n v = GenderedInflection Accusative Plural Neuter v
abl_pl_n v = GenderedInflection Ablative Plural Neuter v

hic_forms :: [GenderedInflection]
hic_forms = [
	nom_sg_m "hic",
	gen_sg_m "huius",
	dat_sg_m "huic",
	acc_sg_m "hunc",
	abl_sg_m "hōc",
	nom_pl_m "hī",
	gen_pl_m "hōrum",
	dat_pl_m "hīs",
	acc_pl_m "hōs",
	abl_pl_m "hīs",
	nom_sg_f "haec",
	gen_sg_f "huius",
	dat_sg_f "huic",
	acc_sg_f "hanc",
	abl_sg_f "hāc",
	nom_pl_f "hae",
	gen_pl_f "hārum",
	dat_pl_f "hīs",
	acc_pl_f "hās",
	abl_pl_f "hīs",
	nom_sg_n "hoc",
	gen_sg_n "huius",
	dat_sg_n "huic",
	acc_sg_n "hoc",
	abl_sg_n "hōc",
	nom_pl_n "haec",
	gen_pl_n "hōrum",
	dat_pl_n "hīs",
	acc_pl_n "haec",
	abl_pl_n "hīs" ]

hic_def :: Definition
hic_def = Definition [ "This, these, used to refer to (a) person(s) or thing(s) close to the speaker." ]

hic :: LatinEntry
hic = PronounEntryG "hic" "hic (m), haec (f), hoc (n)" hic_def hic_forms

ille_forms :: [GenderedInflection]
ille_forms = [
	nom_sg_m "ille",
	gen_sg_m "illīus",
	dat_sg_m "illī",
	acc_sg_m "illum",
	abl_sg_m "illō",
	nom_pl_m "illī",
	gen_pl_m "illorum",
	dat_pl_m "illīs",
	acc_pl_m "illōs",
	abl_pl_m "illīs",
	nom_sg_f "illa",
	gen_sg_f "illīus",
	dat_sg_f "illī",
	acc_sg_f "illam",
	abl_sg_f "illā",
	nom_pl_f "illae",
	gen_pl_f "illārum",
	dat_pl_f "illīs",
	acc_pl_f "illas",
	abl_pl_f "illīs",
	nom_sg_n "illud",
	gen_sg_n "illīus",
	dat_sg_n "illī",
	acc_sg_n "illud",
	abl_sg_n "illō",
	nom_pl_n "illa",
	gen_pl_n "illorum",
	dat_pl_n "illīs",
	acc_pl_n "illa",
	abl_pl_n "illīs" ]

ille_def :: Definition
ille_def = Definition [
	"\"That\" (referring to a person or thing away from both speaker and listener); he, she, it",
	"\"That (renown person)\" (used to cast the referent in a positive light).",
	"(Vulgar Latin) The; used as a definite article." ]

ille :: LatinEntry
ille = PronounEntryG "ille" "ille (m), illa (f), illud (n)" ille_def ille_forms

relative_forms :: [GenderedInflection]
relative_forms = [
	nom_sg_m "qui",
	gen_sg_m "cuius",
	dat_sg_m "cui",
	acc_sg_m "quem",
	abl_sg_m "quō",
	nom_pl_m "quī",
	gen_pl_m "quōrum",
	dat_pl_m "quibus",
	acc_pl_m "quōs",
	abl_pl_m "quibus",
	nom_sg_f "quae",
	gen_sg_f "cuius",
	dat_sg_f "cui",
	acc_sg_f "quam",
	abl_sg_f "quā",
	nom_pl_f "quae",
	gen_pl_f "quārum",
	dat_pl_f "quibus",
	acc_pl_f "quās",
	abl_pl_f "quibus",
	nom_sg_n "quod",
	gen_sg_n "cuius",
	dat_sg_n "cui",
	acc_sg_n "quod",
	abl_sg_n "quō",
	nom_pl_n "quae",
	gen_pl_n "quōrum",
	dat_pl_n "quibus",
	acc_pl_n "quae",
	abl_pl_n "quibus" ]

relative_def :: Definition
relative_def = Definition [ "(relative) who, what, that, which" ]

relative :: LatinEntry
relative = PronounEntryG "qui" "quī (m), quae (f), quod (n)" relative_def relative_forms

interrogative_forms :: [GenderedInflection]
interrogative_forms = [
	nom_sg_m "quis",
	gen_sg_m "cuius",
	dat_sg_m "cui",
	acc_sg_m "quem",
	abl_sg_m "quō",
	nom_pl_m "quī",
	gen_pl_m "quōrum",
	dat_pl_m "quibus",
	acc_pl_m "quōs",
	abl_pl_m "quibus",
	nom_sg_f "quis",
	gen_sg_f "cuius",
	dat_sg_f "cui",
	acc_sg_f "quem",
	abl_sg_f "quō",
	nom_pl_f "quae",
	gen_pl_f "quārum",
	dat_pl_f "quibus",
	acc_pl_f "quās",
	abl_pl_f "quibus",
	nom_sg_n "quid",
	gen_sg_n "cuius",
	dat_sg_n "cui",
	acc_sg_n "quid",
	abl_sg_n "quō",
	nom_pl_n "quae",
	gen_pl_n "quōrum",
	dat_pl_n "quibus",
	acc_pl_n "quae",
	abl_pl_n "quibus" ]

interrogative_def :: Definition
interrogative_def = Definition [
	"Who?, what?",
	"(after si, nisi, ne, num) someone, something, anyone, anything" ]

interrogative :: LatinEntry
interrogative = PronounEntryG "quis" "quis (m), quae (f), quid (n)" interrogative_def interrogative_forms

intensive_forms :: [GenderedInflection]
intensive_forms = [
	nom_sg_m "ipse",
	gen_sg_m "ipsīus",
	dat_sg_m "ipsī",
	acc_sg_m "ipsum",
	abl_sg_m "ipsō",
	nom_pl_m "ipsī",
	gen_pl_m "ipsōrum",
	dat_pl_m "ipsīs",
	acc_pl_m "ipsōs",
	abl_pl_m "ipsīs",
	nom_sg_f "ipsa",
	gen_sg_f "ipsīus",
	dat_sg_f "ipsī",
	acc_sg_f "ipsam",
	abl_sg_f "ipsā",
	nom_pl_f "ipsī",
	gen_pl_f "ipsārum",
	dat_pl_f "ipsīs",
	acc_pl_f "ipsās",
	abl_pl_f "ipsīs",
	nom_sg_n "ipsum",
	gen_sg_n "ipsīus",
	dat_sg_n "ipsī",
	acc_sg_n "ipsum",
	abl_sg_n "ipsō",
	nom_pl_n "ipsa",
	gen_pl_n "ipsōrum",
	dat_pl_n "ipsīs",
	acc_pl_n "ipsa",
	abl_pl_n "ipsīs" ]

intensive_def :: Definition
intensive_def = Definition [ "himself" ]

intensive :: LatinEntry
intensive = PronounEntryG "ipse" "ipse (m), ipsa (f), ipsum (n)" intensive_def intensive_forms

is_forms :: [GenderedInflection]
is_forms = [
	nom_sg_m "is",
	gen_sg_m "eius",
	dat_sg_m "eī",
	acc_sg_m "eum",
	abl_sg_m "eō",
	nom_pl_m "eī",
	gen_pl_m "eōrum",
	dat_pl_m "eīs",
	acc_pl_m "eōs",
	abl_pl_m "eīs",
	nom_sg_f "ea",
	gen_sg_f "eius",
	dat_sg_f "eī",
	acc_sg_f "eam",
	abl_sg_f "eā",
	nom_pl_f "eae",
	gen_pl_f "eārum",
	dat_pl_f "eīs",
	acc_pl_f "eās",
	abl_pl_f "eīs",
	nom_sg_n "id",
	gen_sg_n "eius",
	dat_sg_n "eī",
	acc_sg_n "id",
	abl_sg_n "eō",
	nom_pl_n "ea",
	gen_pl_n "eōrum",
	dat_pl_n "eīs",
	acc_pl_n "ea",
	abl_pl_n "eīs" ]

is_def :: Definition
is_def = Definition [ "(demonstrative) it; he (refers to a masculine word), this, that" ]

is :: LatinEntry
is = PronounEntryG "is" "is (m), ea (f), id (n)" is_def is_forms

idem_forms :: [GenderedInflection]
idem_forms = [
	nom_sg_m "īdem",
	gen_sg_m "eiusdem",
	dat_sg_m "eīdem",
	acc_sg_m "eundem",
	abl_sg_m "eōdem",
	nom_pl_m "eīdem",
	gen_pl_m "eōrundem",
	dat_pl_m "eīsdem",
	acc_pl_m "eōsdem",
	abl_pl_m "eīsdem",
	nom_sg_f "eadem",
	gen_sg_f "eiusdem",
	dat_sg_f "eīdem",
	acc_sg_f "eandem",
	abl_sg_f "eādem",
	nom_pl_f "eaedem",
	gen_pl_f "eārundem",
	dat_pl_f "eīsdem",
	acc_pl_f "eāsdem",
	abl_pl_f "eīsdem",
	nom_sg_n "idem",
	gen_sg_n "eiusdem",
	dat_sg_n "eīdem",
	acc_sg_n "idem",
	abl_sg_n "eōdem",
	nom_pl_n "eadem",
	gen_pl_n "eōrundem",
	dat_pl_n "eīsdem",
	acc_pl_n "eadem",
	abl_pl_n "eīsdem" ]

idem_def :: Definition
idem_def = Definition [ "the same" ]

idem :: LatinEntry
idem = PronounEntryG "idem" "īdem (m), eadem (f), idem (n)" idem_def idem_forms

ego_forms :: [Inflection]
ego_forms = [
	Inflection Nominative Singular "ego",
	Inflection Genitive Singular "meī",
	Inflection Dative Singular "mihi",
	Inflection Accusative Singular "mē",
	Inflection Ablative Singular "mē",
	Inflection Nominative Plural "nōs",
	Inflection Genitive Plural "nostrum",
	Inflection Dative Plural "nōbis",
	Inflection Accusative Plural "nōs",
	Inflection Ablative Plural "nōbis" ]

ego_def :: Definition 
ego_def = Definition [ "I; first person pronoun" ]

ego :: LatinEntry
ego = PronounEntry "ego" "ego (sg), nōs (pl)" ego_def ego_forms

tu_forms = [
	Inflection Nominative Singular "tū",
	Inflection Genitive Singular "tuī",
	Inflection Dative Singular "tibi",
	Inflection Accusative Singular "te",
	Inflection Ablative Singular "te",
	Inflection Nominative Plural "vōs",
	Inflection Genitive Plural "vestrum",
	Inflection Dative Plural "vōbis",
	Inflection Accusative Plural "vōs",
	Inflection Ablative Plural "vōbis" ]

tu_def :: Definition
tu_def = Definition [ "you; second person pronoun" ]

tu :: LatinEntry
tu = PronounEntry "tu" "tu" tu_def tu_forms

sui_forms = [
	Inflection Genitive Singular "suī",
	Inflection Dative Singular "sibi",
	Inflection Accusative Singular "se",
	Inflection Ablative Singular "se",
	Inflection Genitive Plural "suī",
	Inflection Dative Plural "sibi",
	Inflection Accusative Plural "se",
	Inflection Ablative Plural "se" ]

sui_def :: Definition
sui_def = Definition [
	"the genitive of the reflexive pronoun meaning of himself, of herself, of itself, of themselves, one another, each other, etc.",
	"the inflected form of the possessive pronoun meaning his, her/hers, its, their.",
	"the genitive of the possessive pronoun meaning his, her/hers, its, their." ]

sui :: LatinEntry
sui = PronounEntry "sui" "suī" sui_def sui_forms

meus_forms :: [GenderedInflection]
meus_forms = [
	nom_sg_m "meus",
	gen_sg_m "meī",
	dat_sg_m "meō",
	acc_sg_m "meum",
	abl_sg_m "meō",
	nom_sg_f "mea",
	gen_sg_f "meae",
	dat_sg_f "meae",
	acc_sg_f "meam",
	abl_sg_f "meā",
	nom_sg_n "meum",
	gen_sg_n "meī",
	dat_sg_n "meō",
	acc_sg_n "meum",
	abl_sg_n "meō",
	nom_pl_m "meī",
	gen_pl_m "meōrum",
	dat_pl_m "meīs",
	acc_pl_m "meōs",
	abl_pl_m "meīs",
	nom_pl_f "meae",
	gen_pl_f "meārum",
	dat_pl_f "meīs",
	acc_pl_f "meās",
	abl_pl_f "meīs",
	nom_pl_n "mea",
	gen_pl_n "meōrum",
	dat_pl_n "meīs",
	acc_pl_n "mea",
	abl_pl_n "meīs"]

meus_def :: Definition
meus_def = Definition [ "(possessive) my, mine" ]

meus :: LatinEntry
meus = PronounEntryG "meus" "meus" meus_def meus_forms

tuus_forms :: [GenderedInflection]
tuus_forms = [
	nom_sg_m "tuus",
	gen_sg_m "tuī",
	dat_sg_m "tuae",
	acc_sg_m "tuum",
	abl_sg_m "tuō",
	nom_sg_f "tua",
	gen_sg_f "tuae",
	dat_sg_f "tuō",
	acc_sg_f "tuam",
	abl_sg_f "tuā",
	nom_sg_n "tuum",
	gen_sg_n "tuī",
	dat_sg_n "tuīs",
	acc_sg_n "tuum",
	abl_sg_n "tuō",
	nom_pl_m "tuī",
	gen_pl_m "tuōrum",
	dat_pl_m "tuīs",
	acc_pl_m "tuōs",
	abl_pl_m "tuīs",
	nom_pl_f "tuae",
	gen_pl_f "tuārum",
	dat_pl_f "tuīs",
	acc_pl_f "tuās",
	abl_pl_f "tuīs",
	nom_pl_n "tua",
	gen_pl_n "tuōrum",
	dat_pl_n "tua",
	acc_pl_n "tuīs",
	abl_pl_n "tua" ]

tuus_def :: Definition
tuus_def = Definition [ "(possessive) your (singular)" ]

tuus :: LatinEntry
tuus = PronounEntryG "tuus" "tuus" tuus_def tuus_forms

suus_forms :: [GenderedInflection]
suus_forms = [
	nom_sg_m "suus",
	gen_sg_m "suī",
	dat_sg_m "suae",
	acc_sg_m "suum",
	abl_sg_m "suō",
	nom_sg_f "sua",
	gen_sg_f "suae",
	dat_sg_f "suō",
	acc_sg_f "suam",
	abl_sg_f "suā",
	nom_sg_n "suum",
	gen_sg_n "suī",
	dat_sg_n "suīs",
	acc_sg_n "suum",
	abl_sg_n "suō",
	nom_pl_m "suī",
	gen_pl_m "suōrum",
	dat_pl_m "suīs",
	acc_pl_m "suōs",
	abl_pl_m "suīs",
	nom_pl_f "suae",
	gen_pl_f "suārum",
	dat_pl_f "suīs",
	acc_pl_f "suās",
	abl_pl_f "suīs",
	nom_pl_n "sua",
	gen_pl_n "suōrum",
	dat_pl_n "sua",
	acc_pl_n "suīs",
	abl_pl_n "sua" ]

suus_def :: Definition
suus_def = Definition [ "(possessive, reflexive) his, her/hers, its, their" ]

suus :: LatinEntry
suus = PronounEntryG "suus" "suus" suus_def suus_forms

pronoun_entries :: [LatinEntry]
pronoun_entries = [ hic, ille, relative, interrogative, intensive, is, idem, ego, tu, sui, meus, tuus, suus ]
