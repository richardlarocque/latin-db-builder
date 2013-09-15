module Wiki.Latin.VerbConjugation(getVerbConjugations) where

import qualified Data.Text as T
import Data.Maybe

import Latin.Grammar
import Wiki.Types
import Wiki.PageParser

getVerbConjugations :: [TemplateRef] -> Maybe (T.Text, [VerbForm])
getVerbConjugations trefs = listToMaybe $ mapMaybe readVerbConjugationTemplates trefs

verb_template_table :: [(T.Text, [T.Text] -> Maybe [VerbForm])]
verb_template_table = [
	(T.pack "la-conj-1st", la_conj_1st.stripNamedParams),
	(T.pack "la-conj-1st-depon", la_conj_1st_depon.stripNamedParams),
	(T.pack "la-conj-1st-do", la_conj_1st_do.stripNamedParams),
	(T.pack "la-conj-1st-nopass", la_conj_1st_nopass.stripNamedParams),
	(T.pack "la-conj-1st-pass3p", la_conj_1st_pass3p.stripNamedParams),
	(T.pack "la-conj-2nd", la_conj_2nd.stripNamedParams),
	(T.pack "la-conj-2nd-depon", la_conj_2nd_depon.stripNamedParams),
	(T.pack "la-conj-2nd-impers", la_conj_2nd_impers.stripNamedParams),
	(T.pack "la-conj-2nd-libet", la_conj_2nd_libet.stripNamedParams),
	(T.pack "la-conj-2nd-licet", la_conj_2nd_licet.stripNamedParams),
	(T.pack "la-conj-2nd-nopass", la_conj_2nd_nopass.stripNamedParams),
	(T.pack "la-conj-2nd-noperf", la_conj_2nd_noperf.stripNamedParams),
	(T.pack "la-conj-2nd-pass3p", la_conj_2nd_pass3p.stripNamedParams),
	(T.pack "la-conj-2nd-semi", la_conj_2nd_semi.stripNamedParams),
	(T.pack "la-conj-3rd", la_conj_3rd.stripNamedParams),
	(T.pack "la-conj-3rd-dico", la_conj_3rd_dico.stripNamedParams),
	(T.pack "la-conj-3rd-IO", la_conj_3rd_IO.stripNamedParams),
	(T.pack "la-conj-3rd-IO-facio", la_conj_3rd_IO_facio.stripNamedParams),
	(T.pack "la-conj-3rd-IO-depon", la_conj_3rd_IO_depon.stripNamedParams),
	(T.pack "la-conj-3rd-IO-nopass", la_conj_3rd_IO_nopass.stripNamedParams),
	(T.pack "la-conj-3rd-IO-3p", la_conj_3rd_IO_pass3p.stripNamedParams),
	(T.pack "la-conj-3rd-depon", la_conj_3rd_depon.stripNamedParams),
	(T.pack "la-conj-3rd-fero", la_conj_3rd_fero.stripNamedParams),
	(T.pack "la-conj-3rd-no234", la_conj_3rd_no234.stripNamedParams),
	(T.pack "la-conj-3rd-nopass", la_conj_3rd_nopass.stripNamedParams),
	(T.pack "la-conj-3rd-pass3p", la_conj_3rd_pass3p.stripNamedParams),
	(T.pack "la-conj-3rd-semi", la_conj_3rd_semi.stripNamedParams),
	(T.pack "la-conj-3rd-semi-fio", la_conj_3rd_semi_fio.stripNamedParams),
	(T.pack "la-conj-4th", la_conj_4th.stripNamedParams),
	(T.pack "la-conj-4th-depon", la_conj_4th_depon.stripNamedParams),
	(T.pack "la-conj-4th-nopass", la_conj_4th_nopass.stripNamedParams),
	(T.pack "la-conj-4th-pass3p", la_conj_4th_pass3p.stripNamedParams),
	(T.pack "la-conj-irr-volo", la_conj_irr_volo),
	(T.pack "la-conj-irr-eo", la_conj_irr_eo.stripNamedParams),
	(T.pack "la-conj-irr-sum", la_conj_irr_sum.stripNamedParams)]


readVerbConjugationTemplates :: TemplateRef -> Maybe (T.Text, [VerbForm])
readVerbConjugationTemplates t = do
	(name, params) <- case t of
		TemplateRef (name:params) -> Just (name,params)
		TemplateRef _ -> Nothing
	parser <- lookup name verb_template_table
	forms <- parser params
	return (name, forms)

stripNamedParams :: [T.Text] -> [T.Text]
stripNamedParams ts = [ t | t <- ts , not $ (T.singleton '=') `T.isInfixOf` t ]

withSuffix :: T.Text -> String -> T.Text
withSuffix a b = a `T.append` (T.pack b)

withoutPerfectForms :: [VerbForm] -> [VerbForm]
withoutPerfectForms = filter (not.isPerfectForm)
	where isPerfectForm x = case x of
		(Conjugated _ _ Perfect _ _ _) -> True
		(Conjugated _ _ Pluperfect _ _ _) -> True
		(Conjugated _ _ FuturePerfect _ _ _) -> True
		(Imperative _ Perfect _ _) -> True
		(Infinitive _ Perfect _) -> True
		(Participle _ Perfect _ _ _ _) -> True
                -- ban the supine forms, too.
                (Infinitive _ Future _) -> True
		(Participle Active Future _ _ _ _) -> True
		_ -> False

withoutPassiveForms :: [VerbForm] -> [VerbForm]
withoutPassiveForms = filter (not.isPassiveForm)
	where isPassiveForm x = case x of
		(Conjugated _ Passive _ _ _ _) -> True
		(Imperative Passive _ _ _) -> True
		(Infinitive Passive _ _) -> True
		(Participle Passive _ _ _ _ _) -> True
		_ -> False

filter3pPassiveForms :: [VerbForm] -> [VerbForm]
filter3pPassiveForms = filter (\x -> isThirdPerson x || (not (isPassiveForm x)))
	where
	isPassiveForm x = case x of
		(Conjugated _ Passive _ _ _ _) -> True
		(Imperative Passive _ _ _) -> True
		--(Infinitive Passive _ _) -> True -- NOTE: Infinitive passive *are* allowed in this case
		(Participle Passive _ _ _ _ _) -> True
		_ -> False
	isThirdPerson x = case x of
		(Conjugated _ _ _ _ Third _) -> True
		_ -> False

-- Warning: This won't work on deponents or semi-deponents.
withoutSupineForms :: [VerbForm] -> [VerbForm]
withoutSupineForms = filter (not.isSupineForm)
	where
	isSupineForm x = case x of
		(Infinitive Active Future _) -> True
		(Participle Active Present _ _ _ _) -> True
		(Participle Active Future _ _ _ _) -> True
		(Participle Passive Perfect _ _ _ _) -> True
		_ -> False

without234 :: [VerbForm] -> [VerbForm]
without234 = filter (not.is234)
	where
	is234 x = case x of
		(Conjugated _ _ Perfect _ _ _) -> True
		(Conjugated _ _ Pluperfect _ _ _) -> True
		(Conjugated _ _ FuturePerfect _ _ _) -> True
		(Infinitive _ _ _) -> True
		(Participle Active Present _ _ _ _) -> False
		(Participle Passive Future _ _ _ _) -> False
		(Participle _ _ _ _ _ _) -> True
		_ -> False

withImpersonalOnly :: [VerbForm] -> [VerbForm]
withImpersonalOnly = filter isImpersonal
	where
	isImpersonal x = case x of
		(Conjugated _ _ _ _ Third _) -> True
		(Conjugated _ _ _ _ _ _) -> False
		_ -> True

-- Helpers for interpreting Wiki's Latin conjugation tables.
_11, _12, _13, _14, _15, _16, _21, _22, _23, _24, _25, _26, _31, _32, _33, _34, _35, _36, _41, _42, _43, _44, _45, _46, _51, _52, _53, _54, _55, _56, _61, _62, _63, _64, _65, _66, _71, _72, _73, _74, _75, _76, _81, _82, _83, _84, _85, _86, _91, _92, _93, _94, _95, _96, _101, _102, _103, _104, _105, _106, _111, _112, _113, _114, _115, _116, _121, _122, _123, _124, _125, _126, _131, _132, _133, _134, _135, _136, _141, _142, _143, _144, _145, _146, _151, _152, _153, _154, _155, _156, _161, _171, _181, _182, _183, _184, _185, _186 :: T.Text -> VerbForm

_11 = Conjugated Indicative Active Present Singular First
_12 = Conjugated Indicative Active Present Singular Second
_13 = Conjugated Indicative Active Present Singular Third
_14 = Conjugated Indicative Active Present Plural First
_15 = Conjugated Indicative Active Present Plural Second
_16 = Conjugated Indicative Active Present Plural Third

_21 = Conjugated Indicative Active Future Singular First
_22 = Conjugated Indicative Active Future Singular Second
_23 = Conjugated Indicative Active Future Singular Third
_24 = Conjugated Indicative Active Future Plural First
_25 = Conjugated Indicative Active Future Plural Second
_26 = Conjugated Indicative Active Future Plural Third

_31 = Conjugated Indicative Active Imperfect Singular First
_32 = Conjugated Indicative Active Imperfect Singular Second
_33 = Conjugated Indicative Active Imperfect Singular Third
_34 = Conjugated Indicative Active Imperfect Plural First
_35 = Conjugated Indicative Active Imperfect Plural Second
_36 = Conjugated Indicative Active Imperfect Plural Third

_41 = Conjugated Indicative Active Perfect Singular First
_42 = Conjugated Indicative Active Perfect Singular Second
_43 = Conjugated Indicative Active Perfect Singular Third
_44 = Conjugated Indicative Active Perfect Plural First
_45 = Conjugated Indicative Active Perfect Plural Second
_46 = Conjugated Indicative Active Perfect Plural Third

_51 = Conjugated Indicative Active FuturePerfect Singular First
_52 = Conjugated Indicative Active FuturePerfect Singular Second
_53 = Conjugated Indicative Active FuturePerfect Singular Third
_54 = Conjugated Indicative Active FuturePerfect Plural First
_55 = Conjugated Indicative Active FuturePerfect Plural Second
_56 = Conjugated Indicative Active FuturePerfect Plural Third

_61 = Conjugated Indicative Active Pluperfect Singular First
_62 = Conjugated Indicative Active Pluperfect Singular Second
_63 = Conjugated Indicative Active Pluperfect Singular Third
_64 = Conjugated Indicative Active Pluperfect Plural First
_65 = Conjugated Indicative Active Pluperfect Plural Second
_66 = Conjugated Indicative Active Pluperfect Plural Third

_71 = Conjugated Indicative Passive Present Singular First
_72 = Conjugated Indicative Passive Present Singular Second
_73 = Conjugated Indicative Passive Present Singular Third
_74 = Conjugated Indicative Passive Present Plural First
_75 = Conjugated Indicative Passive Present Plural Second
_76 = Conjugated Indicative Passive Present Plural Third

_81 = Conjugated Indicative Passive Future Singular First
_82 = Conjugated Indicative Passive Future Singular Second
_83 = Conjugated Indicative Passive Future Singular Third
_84 = Conjugated Indicative Passive Future Plural First
_85 = Conjugated Indicative Passive Future Plural Second
_86 = Conjugated Indicative Passive Future Plural Third

_91 = Conjugated Indicative Passive Imperfect Singular First
_92 = Conjugated Indicative Passive Imperfect Singular Second
_93 = Conjugated Indicative Passive Imperfect Singular Third
_94 = Conjugated Indicative Passive Imperfect Plural First
_95 = Conjugated Indicative Passive Imperfect Plural Second
_96 = Conjugated Indicative Passive Imperfect Plural Third

_101 = Conjugated Subjunctive Active Present Singular First
_102 = Conjugated Subjunctive Active Present Singular Second
_103 = Conjugated Subjunctive Active Present Singular Third
_104 = Conjugated Subjunctive Active Present Plural First
_105 = Conjugated Subjunctive Active Present Plural Second
_106 = Conjugated Subjunctive Active Present Plural Third

_111 = Conjugated Subjunctive Active Imperfect Singular First
_112 = Conjugated Subjunctive Active Imperfect Singular Second
_113 = Conjugated Subjunctive Active Imperfect Singular Third
_114 = Conjugated Subjunctive Active Imperfect Plural First
_115 = Conjugated Subjunctive Active Imperfect Plural Second
_116 = Conjugated Subjunctive Active Imperfect Plural Third

_121 = Conjugated Subjunctive Active Perfect Singular First
_122 = Conjugated Subjunctive Active Perfect Singular Second
_123 = Conjugated Subjunctive Active Perfect Singular Third
_124 = Conjugated Subjunctive Active Perfect Plural First
_125 = Conjugated Subjunctive Active Perfect Plural Second
_126 = Conjugated Subjunctive Active Perfect Plural Third

_131 = Conjugated Subjunctive Active Pluperfect Singular First
_132 = Conjugated Subjunctive Active Pluperfect Singular Second
_133 = Conjugated Subjunctive Active Pluperfect Singular Third
_134 = Conjugated Subjunctive Active Pluperfect Plural First
_135 = Conjugated Subjunctive Active Pluperfect Plural Second
_136 = Conjugated Subjunctive Active Pluperfect Plural Third

_141 = Conjugated Subjunctive Passive Present Singular First
_142 = Conjugated Subjunctive Passive Present Singular Second
_143 = Conjugated Subjunctive Passive Present Singular Third
_144 = Conjugated Subjunctive Passive Present Plural First
_145 = Conjugated Subjunctive Passive Present Plural Second
_146 = Conjugated Subjunctive Passive Present Plural Third

_151 = Conjugated Subjunctive Passive Imperfect Singular First
_152 = Conjugated Subjunctive Passive Imperfect Singular Second
_153 = Conjugated Subjunctive Passive Imperfect Singular Third
_154 = Conjugated Subjunctive Passive Imperfect Plural First
_155 = Conjugated Subjunctive Passive Imperfect Plural Second
_156 = Conjugated Subjunctive Passive Imperfect Plural Third

-- FIXME: Subjunctive Passive Perfect and Subjunctive Passive Pluperfect

_161 = Imperative Active Present Singular
_171 = Imperative Active Present Plural

_181 = Infinitive Active Present
_182 = Infinitive Active Perfect
_183 = Infinitive Active Future
_184 = Infinitive Passive Present
_185 = Infinitive Passive Perfect
_186 = Infinitive Passive Future

act_pres_participle, act_fut_participle, act_perf_participle, pass_perf_participle, pass_fut_participle :: T.Text -> [VerbForm]

act_pres_participle stem = [
	Participle Active Present Nominative Singular Masculine (stem `withSuffix` "ns"),
	Participle Active Present Genitive Singular Masculine (stem `withSuffix` "ntīs"),
	Participle Active Present Dative Singular Masculine (stem `withSuffix` "ntī"),
	Participle Active Present Accusative Singular Masculine (stem `withSuffix` "ntem"),
	Participle Active Present Ablative Singular Masculine (stem `withSuffix` "ntī"),
	Participle Active Present Nominative Singular Feminine (stem `withSuffix` "ns"),
	Participle Active Present Genitive Singular Feminine (stem `withSuffix` "ntīs"),
	Participle Active Present Dative Singular Feminine (stem `withSuffix` "ntī"),
	Participle Active Present Accusative Singular Feminine (stem `withSuffix` "ntem"),
	Participle Active Present Ablative Singular Feminine (stem `withSuffix` "ntī"),
	Participle Active Present Nominative Singular Neuter (stem `withSuffix` "ns"),
	Participle Active Present Genitive Singular Neuter (stem `withSuffix` "ntīs"),
	Participle Active Present Dative Singular Neuter (stem `withSuffix` "ntī"),
	Participle Active Present Accusative Singular Neuter (stem `withSuffix` "ns"),
	Participle Active Present Ablative Singular Neuter (stem `withSuffix` "ntī"),
	Participle Active Present Nominative Plural Masculine (stem `withSuffix` "ntēs"),
	Participle Active Present Genitive Plural Masculine (stem `withSuffix` "ntium"),
	Participle Active Present Dative Plural Masculine (stem `withSuffix` "ntibus"),
	Participle Active Present Accusative Plural Masculine (stem `withSuffix` "ntēs"),
	Participle Active Present Ablative Plural Masculine (stem `withSuffix` "ntibus"),
	Participle Active Present Nominative Plural Feminine (stem `withSuffix` "ntēs"),
	Participle Active Present Genitive Plural Feminine (stem `withSuffix` "ntium"),
	Participle Active Present Dative Plural Feminine (stem `withSuffix` "ntibus"),
	Participle Active Present Accusative Plural Feminine (stem `withSuffix` "ntēs"),
	Participle Active Present Ablative Plural Feminine (stem `withSuffix` "ntibus"),
	Participle Active Present Nominative Plural Neuter (stem `withSuffix` "ntia"),
	Participle Active Present Genitive Plural Neuter (stem `withSuffix` "ntium"),
	Participle Active Present Dative Plural Neuter (stem `withSuffix` "ntibus"),
	Participle Active Present Accusative Plural Neuter (stem `withSuffix` "ntia"),
	Participle Active Present Ablative Plural Neuter (stem `withSuffix` "ntibus")]

act_fut_participle stem = [
	Participle Active Future Nominative Singular Masculine (stem `withSuffix` "ūrus"),
	Participle Active Future Genitive Singular Masculine (stem `withSuffix` "ūrī"),
	Participle Active Future Dative Singular Masculine (stem `withSuffix` "ūrō"),
	Participle Active Future Accusative Singular Masculine (stem `withSuffix` "ūium"),
	Participle Active Future Ablative Singular Masculine (stem `withSuffix` "ūrō"),
	Participle Active Future Nominative Singular Feminine (stem `withSuffix` "ūra"),
	Participle Active Future Genitive Singular Feminine (stem `withSuffix` "ūrae"),
	Participle Active Future Dative Singular Feminine (stem `withSuffix` "ūrae"),
	Participle Active Future Accusative Singular Feminine (stem `withSuffix` "ūram"),
	Participle Active Future Ablative Singular Feminine (stem `withSuffix` "ūrā"),
	Participle Active Future Nominative Singular Neuter (stem `withSuffix` "ūrum"),
	Participle Active Future Genitive Singular Neuter (stem `withSuffix` "ūrī"),
	Participle Active Future Dative Singular Neuter (stem `withSuffix` "ūrō"),
	Participle Active Future Accusative Singular Neuter (stem `withSuffix` "ūrum"),
	Participle Active Future Ablative Singular Neuter (stem `withSuffix` "ūrō"),
	Participle Active Future Nominative Plural Masculine (stem `withSuffix` "ūrī"),
	Participle Active Future Genitive Plural Masculine (stem `withSuffix` "ūrōrum"),
	Participle Active Future Dative Plural Masculine (stem `withSuffix` "ūrīs"),
	Participle Active Future Accusative Plural Masculine (stem `withSuffix` "ūrōs"),
	Participle Active Future Ablative Plural Masculine (stem `withSuffix` "ūrīs"),
	Participle Active Future Nominative Plural Feminine (stem `withSuffix` "ūrae"),
	Participle Active Future Genitive Plural Feminine (stem `withSuffix` "ūrārum"),
	Participle Active Future Dative Plural Feminine (stem `withSuffix` "ūrīs"),
	Participle Active Future Accusative Plural Feminine (stem `withSuffix` "ūrās"),
	Participle Active Future Ablative Plural Feminine (stem `withSuffix` "ūrīs"),
	Participle Active Future Nominative Plural Neuter (stem `withSuffix` "ūra"),
	Participle Active Future Genitive Plural Neuter (stem `withSuffix` "ūrōrum"),
	Participle Active Future Dative Plural Neuter (stem `withSuffix` "ūrīs"),
	Participle Active Future Accusative Plural Neuter (stem `withSuffix` "ūra"),
	Participle Active Future Ablative Plural Neuter (stem `withSuffix` "ūrīs")]

act_perf_participle stem = [
	Participle Active Perfect Nominative Singular Masculine (stem `withSuffix` "us"),
	Participle Active Perfect Genitive Singular Masculine (stem `withSuffix` "ī"),
	Participle Active Perfect Dative Singular Masculine (stem `withSuffix` "ō"),
	Participle Active Perfect Accusative Singular Masculine (stem `withSuffix` "ūium"),
	Participle Active Perfect Ablative Singular Masculine (stem `withSuffix` "ō"),
	Participle Active Perfect Nominative Singular Feminine (stem `withSuffix` "a"),
	Participle Active Perfect Genitive Singular Feminine (stem `withSuffix` "ae"),
	Participle Active Perfect Dative Singular Feminine (stem `withSuffix` "ae"),
	Participle Active Perfect Accusative Singular Feminine (stem `withSuffix` "am"),
	Participle Active Perfect Ablative Singular Feminine (stem `withSuffix` "ā"),
	Participle Active Perfect Nominative Singular Neuter (stem `withSuffix` "um"),
	Participle Active Perfect Genitive Singular Neuter (stem `withSuffix` "ī"),
	Participle Active Perfect Dative Singular Neuter (stem `withSuffix` "ō"),
	Participle Active Perfect Accusative Singular Neuter (stem `withSuffix` "um"),
	Participle Active Perfect Ablative Singular Neuter (stem `withSuffix` "ō"),
	Participle Active Perfect Nominative Plural Masculine (stem `withSuffix` "ī"),
	Participle Active Perfect Genitive Plural Masculine (stem `withSuffix` "ōrum"),
	Participle Active Perfect Dative Plural Masculine (stem `withSuffix` "īs"),
	Participle Active Perfect Accusative Plural Masculine (stem `withSuffix` "ōs"),
	Participle Active Perfect Ablative Plural Masculine (stem `withSuffix` "īs"),
	Participle Active Perfect Nominative Plural Feminine (stem `withSuffix` "ae"),
	Participle Active Perfect Genitive Plural Feminine (stem `withSuffix` "ārum"),
	Participle Active Perfect Dative Plural Feminine (stem `withSuffix` "īs"),
	Participle Active Perfect Accusative Plural Feminine (stem `withSuffix` "ās"),
	Participle Active Perfect Ablative Plural Feminine (stem `withSuffix` "īs"),
	Participle Active Perfect Nominative Plural Neuter (stem `withSuffix` "a"),
	Participle Active Perfect Genitive Plural Neuter (stem `withSuffix` "ōrum"),
	Participle Active Perfect Dative Plural Neuter (stem `withSuffix` "īs"),
	Participle Active Perfect Accusative Plural Neuter (stem `withSuffix` "a"),
	Participle Active Perfect Ablative Plural Neuter (stem `withSuffix` "īs")]

pass_perf_participle stem = [
	Participle Passive Perfect Nominative Singular Masculine (stem `withSuffix` "us"),
	Participle Passive Perfect Genitive Singular Masculine (stem `withSuffix` "ī"),
	Participle Passive Perfect Dative Singular Masculine (stem `withSuffix` "ō"),
	Participle Passive Perfect Accusative Singular Masculine (stem `withSuffix` "ūium"),
	Participle Passive Perfect Ablative Singular Masculine (stem `withSuffix` "ō"),
	Participle Passive Perfect Nominative Singular Feminine (stem `withSuffix` "a"),
	Participle Passive Perfect Genitive Singular Feminine (stem `withSuffix` "ae"),
	Participle Passive Perfect Dative Singular Feminine (stem `withSuffix` "ae"),
	Participle Passive Perfect Accusative Singular Feminine (stem `withSuffix` "am"),
	Participle Passive Perfect Ablative Singular Feminine (stem `withSuffix` "ā"),
	Participle Passive Perfect Nominative Singular Neuter (stem `withSuffix` "um"),
	Participle Passive Perfect Genitive Singular Neuter (stem `withSuffix` "ī"),
	Participle Passive Perfect Dative Singular Neuter (stem `withSuffix` "ō"),
	Participle Passive Perfect Accusative Singular Neuter (stem `withSuffix` "um"),
	Participle Passive Perfect Ablative Singular Neuter (stem `withSuffix` "ō"),
	Participle Passive Perfect Nominative Plural Masculine (stem `withSuffix` "ī"),
	Participle Passive Perfect Genitive Plural Masculine (stem `withSuffix` "ōrum"),
	Participle Passive Perfect Dative Plural Masculine (stem `withSuffix` "īs"),
	Participle Passive Perfect Accusative Plural Masculine (stem `withSuffix` "ōs"),
	Participle Passive Perfect Ablative Plural Masculine (stem `withSuffix` "īs"),
	Participle Passive Perfect Nominative Plural Feminine (stem `withSuffix` "ae"),
	Participle Passive Perfect Genitive Plural Feminine (stem `withSuffix` "ārum"),
	Participle Passive Perfect Dative Plural Feminine (stem `withSuffix` "īs"),
	Participle Passive Perfect Accusative Plural Feminine (stem `withSuffix` "ās"),
	Participle Passive Perfect Ablative Plural Feminine (stem `withSuffix` "īs"),
	Participle Passive Perfect Nominative Plural Neuter (stem `withSuffix` "a"),
	Participle Passive Perfect Genitive Plural Neuter (stem `withSuffix` "ōrum"),
	Participle Passive Perfect Dative Plural Neuter (stem `withSuffix` "īs"),
	Participle Passive Perfect Accusative Plural Neuter (stem `withSuffix` "a"),
	Participle Passive Perfect Ablative Plural Neuter (stem `withSuffix` "īs")]

pass_fut_participle stem = [
	Participle Passive Future Nominative Singular Masculine (stem `withSuffix` "ndus"),
	Participle Passive Future Genitive Singular Masculine (stem `withSuffix` "ndī"),
	Participle Passive Future Dative Singular Masculine (stem `withSuffix` "ndō"),
	Participle Passive Future Accusative Singular Masculine (stem `withSuffix` "ūium"),
	Participle Passive Future Ablative Singular Masculine (stem `withSuffix` "ndō"),
	Participle Passive Future Nominative Singular Feminine (stem `withSuffix` "nda"),
	Participle Passive Future Genitive Singular Feminine (stem `withSuffix` "ndae"),
	Participle Passive Future Dative Singular Feminine (stem `withSuffix` "ndae"),
	Participle Passive Future Accusative Singular Feminine (stem `withSuffix` "ndam"),
	Participle Passive Future Ablative Singular Feminine (stem `withSuffix` "ndā"),
	Participle Passive Future Nominative Singular Neuter (stem `withSuffix` "ndum"),
	Participle Passive Future Genitive Singular Neuter (stem `withSuffix` "ndī"),
	Participle Passive Future Dative Singular Neuter (stem `withSuffix` "ndō"),
	Participle Passive Future Accusative Singular Neuter (stem `withSuffix` "ndum"),
	Participle Passive Future Ablative Singular Neuter (stem `withSuffix` "ndō"),
	Participle Passive Future Nominative Plural Masculine (stem `withSuffix` "ndī"),
	Participle Passive Future Genitive Plural Masculine (stem `withSuffix` "ndōrum"),
	Participle Passive Future Dative Plural Masculine (stem `withSuffix` "ndīs"),
	Participle Passive Future Accusative Plural Masculine (stem `withSuffix` "ndōs"),
	Participle Passive Future Ablative Plural Masculine (stem `withSuffix` "ndīs"),
	Participle Passive Future Nominative Plural Feminine (stem `withSuffix` "ndae"),
	Participle Passive Future Genitive Plural Feminine (stem `withSuffix` "ndārum"),
	Participle Passive Future Dative Plural Feminine (stem `withSuffix` "ndīs"),
	Participle Passive Future Accusative Plural Feminine (stem `withSuffix` "ndās"),
	Participle Passive Future Ablative Plural Feminine (stem `withSuffix` "ndīs"),
	Participle Passive Future Nominative Plural Neuter (stem `withSuffix` "nda"),
	Participle Passive Future Genitive Plural Neuter (stem `withSuffix` "ndōrum"),
	Participle Passive Future Dative Plural Neuter (stem `withSuffix` "ndīs"),
	Participle Passive Future Accusative Plural Neuter (stem `withSuffix` "nda"),
	Participle Passive Future Ablative Plural Neuter (stem `withSuffix` "ndīs")]

ind_pass_perf, ind_pass_fperf, ind_pass_pperf, subj_pass_perf, subj_pass_pperf, ind_depon_perf, ind_depon_fperf, ind_depon_pperf, subj_depon_perf, subj_depon_pperf :: T.Text -> [VerbForm]

passive_past_forms, depon_past_forms :: T.Text -> [VerbForm]

ind_pass_perf sup = [
	context Singular First (sup `withSuffix` "us sum"),
	context Singular Second (sup `withSuffix` "us es"),
	context Singular Third (sup `withSuffix` "us est"),
	context Plural First (sup `withSuffix` "ī sumus"),
	context Plural Second (sup `withSuffix` "ī estis"),
	context Plural Third (sup `withSuffix` "ī sunt")]
	where context = Conjugated Indicative Passive Perfect
	
ind_pass_fperf sup = [
	context Singular First (sup `withSuffix` "us erō"),
	context Singular Second (sup `withSuffix` "us eris"),
	context Singular Third (sup `withSuffix` "us erit"),
	context Plural First (sup `withSuffix` "ī erimus"),
	context Plural Second (sup `withSuffix` "ī eritis"),
	context Plural Third (sup `withSuffix` "ī erunt")]
	where context = Conjugated Indicative Passive FuturePerfect

ind_pass_pperf sup = [
	context Singular First (sup `withSuffix` "us eram"),
	context Singular Second (sup `withSuffix` "us erās"),
	context Singular Third (sup `withSuffix` "us erat"),
	context Plural First (sup `withSuffix` "ī erāmus"),
	context Plural Second (sup `withSuffix` "ī erātis"),
	context Plural Third (sup `withSuffix` "ī erant")]
	where context = Conjugated Indicative Passive Pluperfect

subj_pass_perf sup = [
	context Singular First (sup `withSuffix` "us sim"),
	context Singular Second (sup `withSuffix` "us sīs"),
	context Singular Third (sup `withSuffix` "us sīt"),
	context Plural First (sup `withSuffix` "ī sīmus"),
	context Plural Second (sup `withSuffix` "ī sītis"),
	context Plural Third (sup `withSuffix` "ī sint")]
	where context = Conjugated Subjunctive Passive Perfect 

subj_pass_pperf sup = [
	context Singular First (sup `withSuffix` "us essem"),
	context Singular Second (sup `withSuffix` "us essēs"),
	context Singular Third (sup `withSuffix` "us esset"),
	context Plural First (sup `withSuffix` "ī essēmus"),
	context Plural Second (sup `withSuffix` "ī essētis"),
	context Plural Third (sup `withSuffix` "ī essent")]
	where context = Conjugated Subjunctive Passive Pluperfect

ind_depon_perf sup = [
	context Singular First (sup `withSuffix` "us sum"),
	context Singular Second (sup `withSuffix` "us es"),
	context Singular Third (sup `withSuffix` "us est"),
	context Plural First (sup `withSuffix` "ī sumus"),
	context Plural Second (sup `withSuffix` "ī estis"),
	context Plural Third (sup `withSuffix` "ī sunt")]
	where context = Conjugated Indicative Active Perfect
	
ind_depon_fperf sup = [
	context Singular First (sup `withSuffix` "us erō"),
	context Singular Second (sup `withSuffix` "us eris"),
	context Singular Third (sup `withSuffix` "us erit"),
	context Plural First (sup `withSuffix` "ī erimus"),
	context Plural Second (sup `withSuffix` "ī eritis"),
	context Plural Third (sup `withSuffix` "ī erunt")]
	where context = Conjugated Indicative Active FuturePerfect

ind_depon_pperf sup = [
	context Singular First (sup `withSuffix` "us eram"),
	context Singular Second (sup `withSuffix` "us erās"),
	context Singular Third (sup `withSuffix` "us erat"),
	context Plural First (sup `withSuffix` "ī erāmus"),
	context Plural Second (sup `withSuffix` "ī erātis"),
	context Plural Third (sup `withSuffix` "ī erant")]
	where context = Conjugated Indicative Active Pluperfect

subj_depon_perf sup = [
	context Singular First (sup `withSuffix` "us sim"),
	context Singular Second (sup `withSuffix` "us sīs"),
	context Singular Third (sup `withSuffix` "us sīt"),
	context Plural First (sup `withSuffix` "ī sīmus"),
	context Plural Second (sup `withSuffix` "ī sītis"),
	context Plural Third (sup `withSuffix` "ī sint")]
	where context = Conjugated Subjunctive Active Perfect 

subj_depon_pperf sup = [
	context Singular First (sup `withSuffix` "us essem"),
	context Singular Second (sup `withSuffix` "us essēs"),
	context Singular Third (sup `withSuffix` "us esset"),
	context Plural First (sup `withSuffix` "ī essēmus"),
	context Plural Second (sup `withSuffix` "ī essētis"),
	context Plural Third (sup `withSuffix` "ī essent")]
	where context = Conjugated Subjunctive Active Pluperfect

passive_past_forms sup = concat [
	ind_pass_perf sup,
	ind_pass_fperf sup,
	ind_pass_pperf sup,
	subj_pass_perf sup,
	subj_pass_pperf sup]

depon_past_forms sup = concat [
	ind_depon_perf sup,
	ind_depon_fperf sup,
	ind_depon_pperf sup,
	subj_depon_perf sup,
	subj_depon_pperf sup]

la_conj_1st, la_conj_1st_depon, la_conj_1st_do, la_conj_1st_nopass, la_conj_1st_pass3p, la_conj_2nd, la_conj_2nd_depon, la_conj_2nd_impers, la_conj_2nd_libet, la_conj_2nd_licet, la_conj_2nd_nopass, la_conj_2nd_noperf, la_conj_2nd_pass3p, la_conj_2nd_semi, la_conj_3rd, la_conj_3rd_dico, la_conj_3rd_IO, la_conj_3rd_IO_facio, la_conj_3rd_IO_depon, la_conj_3rd_IO_nopass, la_conj_3rd_IO_pass3p, la_conj_3rd_depon, la_conj_3rd_fero, la_conj_3rd_no234, la_conj_3rd_nopass, la_conj_3rd_pass3p, la_conj_3rd_semi, la_conj_3rd_semi_fio, la_conj_4th, la_conj_4th_depon, la_conj_4th_nopass, la_conj_4th_pass3p :: [T.Text] -> Maybe [VerbForm]

la_conj_1st (pres:perf:sup:_) = Just $ [
	_11 (pres `withSuffix` "ō"),
	_12 (pres `withSuffix` "ās"),
	_13 (pres `withSuffix` "at"),
	_14 (pres `withSuffix` "āmus"),
	_15 (pres `withSuffix` "ātis"),
	_16 (pres `withSuffix` "ant"),
	_21 (pres `withSuffix` "ābō"),
	_22 (pres `withSuffix` "ābis"),
	_23 (pres `withSuffix` "ābit"),
	_24 (pres `withSuffix` "ābimus"),
	_25 (pres `withSuffix` "ābitis"),
	_26 (pres `withSuffix` "ābunt"),
	_31 (pres `withSuffix` "ābam"),
	_32 (pres `withSuffix` "ābās"),
	_33 (pres `withSuffix` "ābat"),
	_34 (pres `withSuffix` "ābāmus"),
	_35 (pres `withSuffix` "ābātis"),
	_36 (pres `withSuffix` "ābant"),
	_41 (perf `withSuffix` "ī"),
	_42 (perf `withSuffix` "istī"),
	_43 (perf `withSuffix` "it"),
	_44 (perf `withSuffix` "imus"),
	_45 (perf `withSuffix` "istis"),
	_46 (perf `withSuffix` "ērunt"),
	_51 (perf `withSuffix` "erō"),
	_52 (perf `withSuffix` "eris"),
	_53 (perf `withSuffix` "erit"),
	_54 (perf `withSuffix` "erimus"),
	_55 (perf `withSuffix` "eritis"),
	_56 (perf `withSuffix` "erint"),
	_61 (perf `withSuffix` "eram"),
	_62 (perf `withSuffix` "erās"),
	_63 (perf `withSuffix` "erat"),
	_64 (perf `withSuffix` "erāmus"),
	_65 (perf `withSuffix` "erātis"),
	_66 (perf `withSuffix` "erant"),
	_71 (pres `withSuffix` "or"),
	_72 (pres `withSuffix` "āris"),
	_73 (pres `withSuffix` "ātur"),
	_74 (pres `withSuffix` "āmur"),
	_75 (pres `withSuffix` "āminī"),
	_76 (pres `withSuffix` "antur"),
	_81 (pres `withSuffix` "ābor"),
	_82 (pres `withSuffix` "āberis"),
	_83 (pres `withSuffix` "ābitur"),
	_84 (pres `withSuffix` "ābimur"),
	_85 (pres `withSuffix` "ābiminī"),
	_86 (pres `withSuffix` "ābuntur"),
	_91 (pres `withSuffix` "ābar"),
	_92 (pres `withSuffix` "ābāris"),
	_93 (pres `withSuffix` "ābātur"),
	_94 (pres `withSuffix` "ābāmur"),
	_95 (pres `withSuffix` "ābāminī"),
	_96 (pres `withSuffix` "ābantur"),
	_101 (pres `withSuffix` "em"),
	_102 (pres `withSuffix` "ēs"),
	_103 (pres `withSuffix` "et"),
	_104 (pres `withSuffix` "ēmus"),
	_105 (pres `withSuffix` "ētis"),
	_106 (pres `withSuffix` "ent"),
	_111 (pres `withSuffix` "ārem"),
	_112 (pres `withSuffix` "ārēs"),
	_113 (pres `withSuffix` "āret"),
	_114 (pres `withSuffix` "ārēmus"),
	_115 (pres `withSuffix` "ārētis"),
	_116 (pres `withSuffix` "ārent"),
	_121 (perf `withSuffix` "erim"),
	_122 (perf `withSuffix` "erīs"),
	_123 (perf `withSuffix` "erit"),
	_124 (perf `withSuffix` "erīmus"),
	_125 (perf `withSuffix` "erītis"),
	_126 (perf `withSuffix` "erint"),
	_131 (perf `withSuffix` "issem"),
	_132 (perf `withSuffix` "issēs"),
	_133 (perf `withSuffix` "isset"),
	_134 (perf `withSuffix` "issēmus"),
	_135 (perf `withSuffix` "issētis"),
	_136 (perf `withSuffix` "issent"),
	_141 (pres `withSuffix` "er"),
	_142 (pres `withSuffix` "ēris"),
	_143 (pres `withSuffix` "ētur"),
	_144 (pres `withSuffix` "ēmur"),
	_145 (pres `withSuffix` "ēminī"),
	_146 (pres `withSuffix` "entur"),
	_151 (pres `withSuffix` "ārer"),
	_152 (pres `withSuffix` "ārēris"),
	_153 (pres `withSuffix` "ārētur"),
	_154 (pres `withSuffix` "ārēmur"),
	_155 (pres `withSuffix` "ārēminī"),
	_156 (pres `withSuffix` "ārentur"),
	_161 (pres `withSuffix` "ā"),
	_171 (pres `withSuffix` "āte"),
	_181 (pres `withSuffix` "āre"),
	_182 (perf `withSuffix` "isse"),
	_183 (sup `withSuffix` "ūrus esse"),
	_184 (pres `withSuffix` "ārī"),
	_185 (sup `withSuffix` "us esse"),
	_186 (sup `withSuffix` "um")]
	++ (act_pres_participle (pres `withSuffix` "ā"))
	++ (act_fut_participle sup)
	++ (pass_perf_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "a"))
	++ (passive_past_forms sup)
la_conj_1st _ = Nothing

la_conj_1st_depon (pres:sup:_) = Just $ [
	_11 (pres `withSuffix` "or"),
	_12 (pres `withSuffix` "āris"),
	_13 (pres `withSuffix` "ātur"),
	_14 (pres `withSuffix` "āmur"),
	_15 (pres `withSuffix` "āminī"),
	_16 (pres `withSuffix` "antur"),
	_21 (pres `withSuffix` "ābor"),
	_22 (pres `withSuffix` "āberis"),
	_23 (pres `withSuffix` "ābitur"),
	_24 (pres `withSuffix` "ābimur"),
	_25 (pres `withSuffix` "ābiminī"),
	_26 (pres `withSuffix` "ābuntur"),
	_31 (pres `withSuffix` "ābar"),
	_32 (pres `withSuffix` "ābāris"),
	_33 (pres `withSuffix` "ābātur"),
	_34 (pres `withSuffix` "ābāmur"),
	_35 (pres `withSuffix` "ābāminī"),
	_36 (pres `withSuffix` "ābantur"),
	_101 (pres `withSuffix` "er"),
	_102 (pres `withSuffix` "ēris"),
	_103 (pres `withSuffix` "ētur"),
	_104 (pres `withSuffix` "ēmur"),
	_105 (pres `withSuffix` "ēminī"),
	_106 (pres `withSuffix` "entur"),
	_111 (pres `withSuffix` "ārer"),
	_112 (pres `withSuffix` "ārēris"),
	_113 (pres `withSuffix` "ārētur"),
	_114 (pres `withSuffix` "ārēmur"),
	_115 (pres `withSuffix` "ārēminī"),
	_116 (pres `withSuffix` "ārentur"),
	_161 (pres `withSuffix` "āre"),
	_171 (pres `withSuffix` "āminī"),
	_181 (pres `withSuffix` "ārī"),
	_182 (sup `withSuffix` "us"),
	_183 (sup `withSuffix` "ūrus esse")]
	++ (act_pres_participle (pres `withSuffix` "ā"))
	++ (act_fut_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "a"))
	++ (depon_past_forms sup)
la_conj_1st_depon _ = Nothing

la_conj_1st_do (pres:perf:sup:_) = Just $ [
	_11 (pres `withSuffix` "ō"),
	_12 (pres `withSuffix` "ās"),
	_13 (pres `withSuffix` "at"),
	_14 (pres `withSuffix` "amus"),
	_15 (pres `withSuffix` "atis"),
	_16 (pres `withSuffix` "ant"),
	_21 (pres `withSuffix` "abō"),
	_22 (pres `withSuffix` "abis"),
	_23 (pres `withSuffix` "abit"),
	_24 (pres `withSuffix` "abimus"),
	_25 (pres `withSuffix` "abitis"),
	_26 (pres `withSuffix` "abunt"),
	_31 (pres `withSuffix` "abam"),
	_32 (pres `withSuffix` "abās"),
	_33 (pres `withSuffix` "abat"),
	_34 (pres `withSuffix` "abāmus"),
	_35 (pres `withSuffix` "abātis"),
	_36 (pres `withSuffix` "abant"),
	_41 (perf `withSuffix` "ī"),
	_42 (perf `withSuffix` "istī"),
	_43 (perf `withSuffix` "it"),
	_44 (perf `withSuffix` "imus"),
	_45 (perf `withSuffix` "istis"),
	_46 (perf `withSuffix` "ērunt"),
	_51 (perf `withSuffix` "erō"),
	_52 (perf `withSuffix` "eris"),
	_53 (perf `withSuffix` "erit"),
	_54 (perf `withSuffix` "erimus"),
	_55 (perf `withSuffix` "eritis"),
	_56 (perf `withSuffix` "erint"),
	_61 (perf `withSuffix` "eram"),
	_62 (perf `withSuffix` "erās"),
	_63 (perf `withSuffix` "erat"),
	_64 (perf `withSuffix` "erāmus"),
	_65 (perf `withSuffix` "erātis"),
	_66 (perf `withSuffix` "erant"),
	_71 (pres `withSuffix` "or"),
	_72 (pres `withSuffix` "aris"),
	_73 (pres `withSuffix` "atur"),
	_74 (pres `withSuffix` "amur"),
	_75 (pres `withSuffix` "aminī"),
	_76 (pres `withSuffix` "antur"),
	_81 (pres `withSuffix` "abor"),
	_82 (pres `withSuffix` "aberis"),
	_83 (pres `withSuffix` "abitur"),
	_84 (pres `withSuffix` "abimur"),
	_85 (pres `withSuffix` "abiminī"),
	_86 (pres `withSuffix` "abuntur"),
	_91 (pres `withSuffix` "abar"),
	_92 (pres `withSuffix` "abāris"),
	_93 (pres `withSuffix` "abātur"),
	_94 (pres `withSuffix` "abāmur"),
	_95 (pres `withSuffix` "abāminī"),
	_96 (pres `withSuffix` "abantur"),
	_101 (pres `withSuffix` "em"),
	_102 (pres `withSuffix` "ēs"),
	_103 (pres `withSuffix` "et"),
	_104 (pres `withSuffix` "ēmus"),
	_105 (pres `withSuffix` "ētis"),
	_106 (pres `withSuffix` "ent"),
	_111 (pres `withSuffix` "arem"),
	_112 (pres `withSuffix` "arēs"),
	_113 (pres `withSuffix` "aret"),
	_114 (pres `withSuffix` "arēmus"),
	_115 (pres `withSuffix` "arētis"),
	_116 (pres `withSuffix` "arent"),
	_121 (perf `withSuffix` "erim"),
	_122 (perf `withSuffix` "erīs"),
	_123 (perf `withSuffix` "erit"),
	_124 (perf `withSuffix` "erīmus"),
	_125 (perf `withSuffix` "erītis"),
	_126 (perf `withSuffix` "erint"),
	_131 (perf `withSuffix` "issem"),
	_132 (perf `withSuffix` "issēs"),
	_133 (perf `withSuffix` "isset"),
	_134 (perf `withSuffix` "issēmus"),
	_135 (perf `withSuffix` "issētis"),
	_136 (perf `withSuffix` "issent"),
	_141 (pres `withSuffix` "er"),
	_142 (pres `withSuffix` "ēris"),
	_143 (pres `withSuffix` "ētur"),
	_144 (pres `withSuffix` "ēmur"),
	_145 (pres `withSuffix` "ēminī"),
	_146 (pres `withSuffix` "entur"),
	_151 (pres `withSuffix` "arer"),
	_152 (pres `withSuffix` "arēris"),
	_153 (pres `withSuffix` "arētur"),
	_154 (pres `withSuffix` "arēmur"),
	_155 (pres `withSuffix` "arēminī"),
	_156 (pres `withSuffix` "arentur"),
	_161 (pres `withSuffix` "ā"),
	_171 (pres `withSuffix` "ate"),
	_181 (pres `withSuffix` "are"),
	_182 (perf `withSuffix` "isse"),
	_183 (sup `withSuffix` "ūrus esse"),
	_184 (pres `withSuffix` "ārī"),
	_185 (sup `withSuffix` "us esse"),
	_186 (sup `withSuffix` "um")]
	++ (act_pres_participle (pres `withSuffix` "a"))
	++ (act_fut_participle sup)
	++ (pass_perf_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "a"))
	++ (passive_past_forms sup)
la_conj_1st_do _ = Nothing

la_conj_1st_nopass (pres:perf:sup:_) =
	la_conj_1st [pres,perf,sup] >>= return.withoutPassiveForms
la_conj_1st_nopass (pres:perf:_) =
	la_conj_1st [pres,perf,(T.pack "XXX")] >>= return.withoutPassiveForms.withoutSupineForms
la_conj_1st_nopass _ = Nothing

la_conj_1st_pass3p (pres:perf:sup:_) =
	la_conj_1st [pres,perf,sup] >>= return.filter3pPassiveForms
la_conj_1st_pass3p _ = Nothing

la_conj_2nd (pres:perf:sup:_) = Just $ [
	_11 (pres `withSuffix` "eō"),
	_12 (pres `withSuffix` "ēs"),
	_13 (pres `withSuffix` "et"),
	_14 (pres `withSuffix` "ēmus"),
	_15 (pres `withSuffix` "ētis"),
	_16 (pres `withSuffix` "ent"),
	_21 (pres `withSuffix` "ēbō"),
	_22 (pres `withSuffix` "ēbis"),
	_23 (pres `withSuffix` "ēbit"),
	_24 (pres `withSuffix` "ēbimus"),
	_25 (pres `withSuffix` "ēbitis"),
	_26 (pres `withSuffix` "ēbunt"),
	_31 (pres `withSuffix` "ēbam"),
	_32 (pres `withSuffix` "ēbās"),
	_33 (pres `withSuffix` "ēbat"),
	_34 (pres `withSuffix` "ēbāmus"),
	_35 (pres `withSuffix` "ēbātis"),
	_36 (pres `withSuffix` "ēbant"),
	_41 (perf `withSuffix` "ī"),
	_42 (perf `withSuffix` "istī"),
	_43 (perf `withSuffix` "it"),
	_44 (perf `withSuffix` "imus"),
	_45 (perf `withSuffix` "istis"),
	_46 (perf `withSuffix` "ērunt"),
	_51 (perf `withSuffix` "erō"),
	_52 (perf `withSuffix` "eris"),
	_53 (perf `withSuffix` "erit"),
	_54 (perf `withSuffix` "erimus"),
	_55 (perf `withSuffix` "eritis"),
	_56 (perf `withSuffix` "erint"),
	_61 (perf `withSuffix` "eram"),
	_62 (perf `withSuffix` "erās"),
	_63 (perf `withSuffix` "erat"),
	_64 (perf `withSuffix` "erāmus"),
	_65 (perf `withSuffix` "erātis"),
	_66 (perf `withSuffix` "erant"),
	_71 (pres `withSuffix` "eor"),
	_72 (pres `withSuffix` "ēris"),
	_73 (pres `withSuffix` "ētur"),
	_74 (pres `withSuffix` "ēmur"),
	_75 (pres `withSuffix` "ēminī"),
	_76 (pres `withSuffix` "entur"),
	_81 (pres `withSuffix` "ēbor"),
	_82 (pres `withSuffix` "ēberis"),
	_83 (pres `withSuffix` "ēbitur"),
	_84 (pres `withSuffix` "ēbimur"),
	_85 (pres `withSuffix` "ēbiminī"),
	_86 (pres `withSuffix` "ēbuntur"),
	_91 (pres `withSuffix` "ēbar"),
	_92 (pres `withSuffix` "ēbāris"),
	_93 (pres `withSuffix` "ēbātur"),
	_94 (pres `withSuffix` "ēbāmur"),
	_95 (pres `withSuffix` "ēbāminī"),
	_96 (pres `withSuffix` "ēbantur"),
	_101 (pres `withSuffix` "eam"),
	_102 (pres `withSuffix` "eās"),
	_103 (pres `withSuffix` "eat"),
	_104 (pres `withSuffix` "eāmus"),
	_105 (pres `withSuffix` "eātis"),
	_106 (pres `withSuffix` "eant"),
	_111 (pres `withSuffix` "ērem"),
	_112 (pres `withSuffix` "ērēs"),
	_113 (pres `withSuffix` "ēret"),
	_114 (pres `withSuffix` "ērēmus"),
	_115 (pres `withSuffix` "ērētis"),
	_116 (pres `withSuffix` "ērent"),
	_121 (perf `withSuffix` "erim"),
	_122 (perf `withSuffix` "erīs"),
	_123 (perf `withSuffix` "erit"),
	_124 (perf `withSuffix` "erīmus"),
	_125 (perf `withSuffix` "erītis"),
	_126 (perf `withSuffix` "erint"),
	_131 (perf `withSuffix` "issem"),
	_132 (perf `withSuffix` "issēs"),
	_133 (perf `withSuffix` "isset"),
	_134 (perf `withSuffix` "issēmus"),
	_135 (perf `withSuffix` "issētis"),
	_136 (perf `withSuffix` "issent"),
	_141 (pres `withSuffix` "ear"),
	_142 (pres `withSuffix` "eāris"),
	_143 (pres `withSuffix` "eātur"),
	_144 (pres `withSuffix` "eāmur"),
	_145 (pres `withSuffix` "eāminī"),
	_146 (pres `withSuffix` "eantur"),
	_151 (pres `withSuffix` "ērer"),
	_152 (pres `withSuffix` "ērēris"),
	_153 (pres `withSuffix` "ērētur"),
	_154 (pres `withSuffix` "ērēmur"),
	_155 (pres `withSuffix` "ērēminī"),
	_156 (pres `withSuffix` "ērentur"),
	_161 (pres `withSuffix` "ē"),
	_171 (pres `withSuffix` "ēte"),
	_181 (pres `withSuffix` "ēre"),
	_182 (perf `withSuffix` "isse"),
	_183 (sup `withSuffix` "ūrus esse"),
	_184 (pres `withSuffix` "ērī"),
	_185 (sup `withSuffix` "us esse"),
	_186 (sup `withSuffix` "um")]
	++ (act_pres_participle (pres `withSuffix` "ē"))
	++ (act_fut_participle sup)
	++ (pass_perf_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "e"))
	++ (passive_past_forms sup)
la_conj_2nd _ = Nothing

la_conj_2nd_depon (pres:sup:_) = Just $ [
	_11 (pres `withSuffix` "eor"),
	_12 (pres `withSuffix` "ēris"),
	_13 (pres `withSuffix` "ētur"),
	_14 (pres `withSuffix` "ēmur"),
	_15 (pres `withSuffix` "ēminī"),
	_16 (pres `withSuffix` "entur"),
	_21 (pres `withSuffix` "ēbor"),
	_22 (pres `withSuffix` "ēberis"),
	_23 (pres `withSuffix` "ēbitur"),
	_24 (pres `withSuffix` "ēbimur"),
	_25 (pres `withSuffix` "ēbiminī"),
	_26 (pres `withSuffix` "ēbuntur"),
	_31 (pres `withSuffix` "ēbar"),
	_32 (pres `withSuffix` "ēbāris"),
	_33 (pres `withSuffix` "ēbātur"),
	_34 (pres `withSuffix` "ēbāmur"),
	_35 (pres `withSuffix` "ēbāminī"),
	_36 (pres `withSuffix` "ēbantur"),
-- TODO: perfect, future perfect, pluperfect
	_101 (pres `withSuffix` "ear"),
	_102 (pres `withSuffix` "eāris"),
	_103 (pres `withSuffix` "eātur"),
	_104 (pres `withSuffix` "eāmur"),
	_105 (pres `withSuffix` "eāminī"),
	_106 (pres `withSuffix` "eantur"),
	_111 (pres `withSuffix` "ērer"),
	_112 (pres `withSuffix` "ērēris"),
	_113 (pres `withSuffix` "ērētur"),
	_114 (pres `withSuffix` "ērēmur"),
	_115 (pres `withSuffix` "ērēminī"),
	_116 (pres `withSuffix` "ērentur"),
	_161 (pres `withSuffix` "ēre"),
	_171 (pres `withSuffix` "ēminī"),
	_181 (pres `withSuffix` "ērī"),
	_182 (sup `withSuffix` "us esse"),
	_183 (sup `withSuffix` "ūrus esse")]
	++ (act_pres_participle (pres `withSuffix` "ē"))
	++ (act_perf_participle sup)
	++ (act_fut_participle sup)
	++ (pass_fut_participle (sup `withSuffix` "e"))
	++ (depon_past_forms sup)
la_conj_2nd_depon _ = Nothing

la_conj_2nd_impers (pres:perf:sup:_) =
	la_conj_2nd [pres,perf,sup] >>= return.withImpersonalOnly
la_conj_2nd_impers _ = Nothing

la_conj_2nd_libet _ = Just $ [
	_13 (T.pack "libet"),
	_23 (T.pack "libēbit"),
	_33 (T.pack "libēbat"),
	_43 (T.pack "libuit"),
	_53 (T.pack "libuerit"),
	_63 (T.pack "libuerat"),
	_103 (T.pack "libea"),
	_113 (T.pack "libēret"),
	_123 (T.pack "libuerit"),
	_133 (T.pack "libuisset"),
	_136 (T.pack "libuissent"),
	_181 (T.pack "libēre"),
	_182 (T.pack "libuisse")]
	++ (act_pres_participle (T.pack "libē"))
	++ (act_perf_participle (T.pack "libit"))

la_conj_2nd_licet _ = Just $ [
	_13 (T.pack "licet"),
	_16 (T.pack "licent"),
	_23 (T.pack "licēbit"),
	_33 (T.pack "licēbat"),
	_36 (T.pack "licēbant"),
	_43 (T.pack "licuit"),
	_53 (T.pack "licuerit"),
	_63 (T.pack "licuerat"),
	_103 (T.pack "liceat"),
	_106 (T.pack "liceant"),
	_113 (T.pack "licēret"),
	_123 (T.pack "licuerit"),
	_133 (T.pack "licuisset"),
	_181 (T.pack "licēre"),
	_182 (T.pack "licuisse"),
	_183 (T.pack "licitūrum esse")]
	++ (act_pres_participle (T.pack "licē"))
	++ (act_perf_participle (T.pack "licit"))
	++ (act_fut_participle (T.pack "licit"))

la_conj_2nd_nopass (pres:perf:sup:_) = 
	la_conj_2nd [pres,perf,sup] >>= return.withoutPassiveForms
la_conj_2nd_nopass (pres:perf:_) =
	la_conj_2nd [pres,perf,(T.pack "XX")] >>= return.withoutPassiveForms.withoutSupineForms
la_conj_2nd_nopass _ = Nothing

la_conj_2nd_noperf (pres:_) =
	la_conj_2nd [pres, (T.pack "XX"), (T.pack "YY")] >>= return.withoutPerfectForms
la_conj_2nd_noperf _ = Nothing

la_conj_2nd_pass3p (pres:perf:sup:_) =
	la_conj_2nd [pres,perf,sup] >>= return.filter3pPassiveForms
la_conj_2nd_pass3p _ = Nothing

la_conj_2nd_semi (pres:sup:_) = Just $ [
	_11 (pres `withSuffix` "eō"),
	_12 (pres `withSuffix` "ēs"),
	_13 (pres `withSuffix` "et"),
	_14 (pres `withSuffix` "ēmus"),
	_15 (pres `withSuffix` "ētis"),
	_16 (pres `withSuffix` "ent"),
	_21 (pres `withSuffix` "ēbō"),
	_22 (pres `withSuffix` "ēbis"),
	_23 (pres `withSuffix` "ēbit"),
	_24 (pres `withSuffix` "ēbimus"),
	_25 (pres `withSuffix` "ēbitis"),
	_26 (pres `withSuffix` "ēbunt"),
	_31 (pres `withSuffix` "ēbam"),
	_32 (pres `withSuffix` "ēbās"),
	_33 (pres `withSuffix` "ēbat"),
	_34 (pres `withSuffix` "ēbāmus"),
	_35 (pres `withSuffix` "ēbātis"),
	_36 (pres `withSuffix` "ēbant"),
	_101 (pres `withSuffix` "eam"),
	_102 (pres `withSuffix` "eās"),
	_103 (pres `withSuffix` "eat"),
	_104 (pres `withSuffix` "eāmus"),
	_105 (pres `withSuffix` "eātis"),
	_106 (pres `withSuffix` "eant"),
	_111 (pres `withSuffix` "ērem"),
	_112 (pres `withSuffix` "ērēs"),
	_113 (pres `withSuffix` "ēret"),
	_114 (pres `withSuffix` "ērēmus"),
	_115 (pres `withSuffix` "ērētis"),
	_116 (pres `withSuffix` "ērent"),
	_161 (pres `withSuffix` "ē"),
	_171 (pres `withSuffix` "ēte"),
	_181 (pres `withSuffix` "ēre"),
	_182 (pres `withSuffix` "us esse"),
	_183 (sup `withSuffix` "ūrus esse")]
	++ (act_pres_participle (pres `withSuffix` "ē"))
	++ (act_perf_participle sup)
	++ (act_fut_participle sup)
	++ (depon_past_forms sup)
la_conj_2nd_semi _ = Nothing

la_conj_3rd (pres:perf:sup:_) = Just $ [
	_11 (pres `withSuffix` "ō"),
	_12 (pres `withSuffix` "is"),
	_13 (pres `withSuffix` "it"),
	_14 (pres `withSuffix` "imus"),
	_15 (pres `withSuffix` "itis"),
	_16 (pres `withSuffix` "unt"),
	_21 (pres `withSuffix` "am"),
	_22 (pres `withSuffix` "ēs"),
	_23 (pres `withSuffix` "et"),
	_24 (pres `withSuffix` "ēmus"),
	_25 (pres `withSuffix` "ētis"),
	_26 (pres `withSuffix` "ent"),
	_31 (pres `withSuffix` "ēbam"),
	_32 (pres `withSuffix` "ēbās"),
	_33 (pres `withSuffix` "ēbat"),
	_34 (pres `withSuffix` "ēbāmus"),
	_35 (pres `withSuffix` "ēbātis"),
	_36 (pres `withSuffix` "ēbant"),
	_41 (perf `withSuffix` "ī"),
	_42 (perf `withSuffix` "istī"),
	_43 (perf `withSuffix` "it"),
	_44 (perf `withSuffix` "imus"),
	_45 (perf `withSuffix` "istis"),
	_46 (perf `withSuffix` "ērunt"),
	_51 (perf `withSuffix` "erō"),
	_52 (perf `withSuffix` "eris"),
	_53 (perf `withSuffix` "erit"),
	_54 (perf `withSuffix` "erimus"),
	_55 (perf `withSuffix` "eritis"),
	_56 (perf `withSuffix` "erint"),
	_61 (perf `withSuffix` "eram"),
	_62 (perf `withSuffix` "erās"),
	_63 (perf `withSuffix` "erat"),
	_64 (perf `withSuffix` "erāmus"),
	_65 (perf `withSuffix` "erātis"),
	_66 (perf `withSuffix` "erant"),
	_71 (pres `withSuffix` "or"),
	_72 (pres `withSuffix` "eris"),
	_73 (pres `withSuffix` "itur"),
	_74 (pres `withSuffix` "imur"),
	_75 (pres `withSuffix` "iminī"),
	_76 (pres `withSuffix` "untur"),
	_81 (pres `withSuffix` "ar"),
	_82 (pres `withSuffix` "ēris"),
	_83 (pres `withSuffix` "ētur"),
	_84 (pres `withSuffix` "ēmur"),
	_85 (pres `withSuffix` "ēminī"),
	_86 (pres `withSuffix` "entur"),
	_91 (pres `withSuffix` "ēbar"),
	_92 (pres `withSuffix` "ēbāris"),
	_93 (pres `withSuffix` "ēbātur"),
	_94 (pres `withSuffix` "ēbāmur"),
	_95 (pres `withSuffix` "ēbāminī"),
	_96 (pres `withSuffix` "ēbantur"),
	_101 (pres `withSuffix` "am"),
	_102 (pres `withSuffix` "ās"),
	_103 (pres `withSuffix` "at"),
	_104 (pres `withSuffix` "āmus"),
	_105 (pres `withSuffix` "ātis"),
	_106 (pres `withSuffix` "ant"),
	_111 (pres `withSuffix` "erem"),
	_112 (pres `withSuffix` "erēs"),
	_113 (pres `withSuffix` "eret"),
	_114 (pres `withSuffix` "erēmus"),
	_115 (pres `withSuffix` "erētis"),
	_116 (pres `withSuffix` "erent"),
	_121 (perf `withSuffix` "erim"),
	_122 (perf `withSuffix` "erīs"),
	_123 (perf `withSuffix` "erit"),
	_124 (perf `withSuffix` "erīmus"),
	_125 (perf `withSuffix` "erītis"),
	_126 (perf `withSuffix` "erint"),
	_131 (perf `withSuffix` "issem"),
	_132 (perf `withSuffix` "issēs"),
	_133 (perf `withSuffix` "isset"),
	_134 (perf `withSuffix` "issēmus"),
	_135 (perf `withSuffix` "issētis"),
	_136 (perf `withSuffix` "issent"),
	_141 (pres `withSuffix` "ar"),
	_142 (pres `withSuffix` "āris"),
	_143 (pres `withSuffix` "ātur"),
	_144 (pres `withSuffix` "āmur"),
	_145 (pres `withSuffix` "āminī"),
	_146 (pres `withSuffix` "antur"),
	_151 (pres `withSuffix` "erer"),
	_152 (pres `withSuffix` "erēris"),
	_153 (pres `withSuffix` "erētur"),
	_154 (pres `withSuffix` "erēmur"),
	_155 (pres `withSuffix` "erēminī"),
	_156 (pres `withSuffix` "erentur"),
	_161 (pres `withSuffix` "e"),
	_171 (pres `withSuffix` "ite"),
	_181 (pres `withSuffix` "ere"),
	_182 (perf `withSuffix` "isse"),
	_183 (sup `withSuffix` "ūrus esse"),
	_184 (pres `withSuffix` "ī"),
	_185 (sup `withSuffix` "us esse"),
	_186 (sup `withSuffix` "um")]
	++ (act_pres_participle (pres `withSuffix` "ē"))
	++ (act_fut_participle sup)
	++ (pass_perf_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "e"))
	++ (passive_past_forms sup)
la_conj_3rd _ = Nothing

la_conj_3rd_dico (pres:perf:sup:_) = Just $ [
	_11 (pres `withSuffix` "ō"),
	_12 (pres `withSuffix` "is"),
	_13 (pres `withSuffix` "it"),
	_14 (pres `withSuffix` "imus"),
	_15 (pres `withSuffix` "itis"),
	_16 (pres `withSuffix` "unt"),
	_21 (pres `withSuffix` "am"),
	_22 (pres `withSuffix` "ēs"),
	_23 (pres `withSuffix` "et"),
	_24 (pres `withSuffix` "ēmus"),
	_25 (pres `withSuffix` "ētis"),
	_26 (pres `withSuffix` "ent"),
	_31 (pres `withSuffix` "ēbam"),
	_32 (pres `withSuffix` "ēbās"),
	_33 (pres `withSuffix` "ēbat"),
	_34 (pres `withSuffix` "ēbāmus"),
	_35 (pres `withSuffix` "ēbātis"),
	_36 (pres `withSuffix` "ēbant"),
	_41 (perf `withSuffix` "ī"),
	_42 (perf `withSuffix` "istī"),
	_43 (perf `withSuffix` "it"),
	_44 (perf `withSuffix` "imus"),
	_45 (perf `withSuffix` "istis"),
	_46 (perf `withSuffix` "ērunt"),
	_51 (perf `withSuffix` "erō"),
	_52 (perf `withSuffix` "eris"),
	_53 (perf `withSuffix` "erit"),
	_54 (perf `withSuffix` "erimus"),
	_55 (perf `withSuffix` "eritis"),
	_56 (perf `withSuffix` "erint"),
	_61 (perf `withSuffix` "eram"),
	_62 (perf `withSuffix` "erās"),
	_63 (perf `withSuffix` "erat"),
	_64 (perf `withSuffix` "erāmus"),
	_65 (perf `withSuffix` "erātis"),
	_66 (perf `withSuffix` "erant"),
	_71 (pres `withSuffix` "or"),
	_72 (pres `withSuffix` "eris"),
	_73 (pres `withSuffix` "itur"),
	_74 (pres `withSuffix` "imur"),
	_75 (pres `withSuffix` "iminī"),
	_76 (pres `withSuffix` "untur"),
	_81 (pres `withSuffix` "ar"),
	_82 (pres `withSuffix` "ēris"),
	_83 (pres `withSuffix` "ētur"),
	_84 (pres `withSuffix` "ēmur"),
	_85 (pres `withSuffix` "ēminī"),
	_86 (pres `withSuffix` "entur"),
	_91 (pres `withSuffix` "ēbar"),
	_92 (pres `withSuffix` "ēbāris"),
	_93 (pres `withSuffix` "ēbātur"),
	_94 (pres `withSuffix` "ēbāmur"),
	_95 (pres `withSuffix` "ēbāminī"),
	_96 (pres `withSuffix` "ēbantur"),
	_101 (pres `withSuffix` "am"),
	_102 (pres `withSuffix` "ās"),
	_103 (pres `withSuffix` "at"),
	_104 (pres `withSuffix` "āmus"),
	_105 (pres `withSuffix` "ātis"),
	_106 (pres `withSuffix` "ant"),
	_111 (pres `withSuffix` "erem"),
	_112 (pres `withSuffix` "erēs"),
	_113 (pres `withSuffix` "eret"),
	_114 (pres `withSuffix` "erēmus"),
	_115 (pres `withSuffix` "erētis"),
	_116 (pres `withSuffix` "erent"),
	_121 (perf `withSuffix` "erim"),
	_122 (perf `withSuffix` "erīs"),
	_123 (perf `withSuffix` "erit"),
	_124 (perf `withSuffix` "erīmus"),
	_125 (perf `withSuffix` "erītis"),
	_126 (perf `withSuffix` "erint"),
	_131 (perf `withSuffix` "issem"),
	_132 (perf `withSuffix` "issēs"),
	_133 (perf `withSuffix` "isset"),
	_134 (perf `withSuffix` "issēmus"),
	_135 (perf `withSuffix` "issētis"),
	_136 (perf `withSuffix` "issent"),
	_141 (pres `withSuffix` "ar"),
	_142 (pres `withSuffix` "āris"),
	_143 (pres `withSuffix` "ātur"),
	_144 (pres `withSuffix` "āmur"),
	_145 (pres `withSuffix` "āminī"),
	_146 (pres `withSuffix` "antur"),
	_151 (pres `withSuffix` "erer"),
	_152 (pres `withSuffix` "erēris"),
	_153 (pres `withSuffix` "erētur"),
	_154 (pres `withSuffix` "erēmur"),
	_155 (pres `withSuffix` "erēminī"),
	_156 (pres `withSuffix` "erentur"),
	_161 pres,
	_171 (pres `withSuffix` "ite"),
	_181 (pres `withSuffix` "ere"),
	_182 (perf `withSuffix` "isse"),
	_183 (sup `withSuffix` "ūrus esse"),
	_184 (pres `withSuffix` "ī"),
	_185 (sup `withSuffix` "us esse"),
	_186 (sup `withSuffix` "um")]
	++ (act_pres_participle (pres `withSuffix` "ē"))
	++ (act_fut_participle sup)
	++ (pass_perf_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "e"))
	++ (passive_past_forms sup)
la_conj_3rd_dico _ = Nothing

la_conj_3rd_IO (pres:perf:sup:_) = Just $ [
	_11 (pres `withSuffix` "iō"),
	_12 (pres `withSuffix` "is"),
	_13 (pres `withSuffix` "it"),
	_14 (pres `withSuffix` "imus"),
	_15 (pres `withSuffix` "itis"),
	_16 (pres `withSuffix` "iunt"),
	_21 (pres `withSuffix` "iam"),
	_22 (pres `withSuffix` "iēs"),
	_23 (pres `withSuffix` "iet"),
	_24 (pres `withSuffix` "iēmus"),
	_25 (pres `withSuffix` "iētis"),
	_26 (pres `withSuffix` "ient"),
	_31 (pres `withSuffix` "iēbam"),
	_32 (pres `withSuffix` "iēbās"),
	_33 (pres `withSuffix` "iēbat"),
	_34 (pres `withSuffix` "iēbāmus"),
	_35 (pres `withSuffix` "iēbātis"),
	_36 (pres `withSuffix` "iēbant"),
	_41 (perf `withSuffix` "ī"),
	_42 (perf `withSuffix` "istī"),
	_43 (perf `withSuffix` "it"),
	_44 (perf `withSuffix` "imus"),
	_45 (perf `withSuffix` "istis"),
	_46 (perf `withSuffix` "ērunt"),
	_51 (perf `withSuffix` "erō"),
	_52 (perf `withSuffix` "eris"),
	_53 (perf `withSuffix` "erit"),
	_54 (perf `withSuffix` "erimus"),
	_55 (perf `withSuffix` "eritis"),
	_56 (perf `withSuffix` "erint"),
	_61 (perf `withSuffix` "eram"),
	_62 (perf `withSuffix` "erās"),
	_63 (perf `withSuffix` "erat"),
	_64 (perf `withSuffix` "erāmus"),
	_65 (perf `withSuffix` "erātis"),
	_66 (perf `withSuffix` "erant"),
	_71 (pres `withSuffix` "ior"),
	_72 (pres `withSuffix` "eris"),
	_73 (pres `withSuffix` "itur"),
	_74 (pres `withSuffix` "imur"),
	_75 (pres `withSuffix` "iminī"),
	_76 (pres `withSuffix` "iuntur"),
	_81 (pres `withSuffix` "iar"),
	_82 (pres `withSuffix` "iēris"),
	_83 (pres `withSuffix` "iētur"),
	_84 (pres `withSuffix` "iēmur"),
	_85 (pres `withSuffix` "iēminī"),
	_86 (pres `withSuffix` "ientur"),
	_91 (pres `withSuffix` "iēbar"),
	_92 (pres `withSuffix` "iēbāris"),
	_93 (pres `withSuffix` "iēbātur"),
	_94 (pres `withSuffix` "iēbāmur"),
	_95 (pres `withSuffix` "iēbāminī"),
	_96 (pres `withSuffix` "iēbantur"),
	_101 (pres `withSuffix` "iam"),
	_102 (pres `withSuffix` "iās"),
	_103 (pres `withSuffix` "iat"),
	_104 (pres `withSuffix` "iāmus"),
	_105 (pres `withSuffix` "iātis"),
	_106 (pres `withSuffix` "iant"),
	_111 (pres `withSuffix` "erem"),
	_112 (pres `withSuffix` "erēs"),
	_113 (pres `withSuffix` "eret"),
	_114 (pres `withSuffix` "erēmus"),
	_115 (pres `withSuffix` "erētis"),
	_116 (pres `withSuffix` "erent"),
	_121 (perf `withSuffix` "erim"),
	_122 (perf `withSuffix` "erīs"),
	_123 (perf `withSuffix` "erit"),
	_124 (perf `withSuffix` "erīmus"),
	_125 (perf `withSuffix` "erītis"),
	_126 (perf `withSuffix` "erint"),
	_131 (perf `withSuffix` "issem"),
	_132 (perf `withSuffix` "issēs"),
	_133 (perf `withSuffix` "isset"),
	_134 (perf `withSuffix` "issēmus"),
	_135 (perf `withSuffix` "issētis"),
	_136 (perf `withSuffix` "issent"),
	_141 (pres `withSuffix` "iar"),
	_142 (pres `withSuffix` "iāris"),
	_143 (pres `withSuffix` "iātur"),
	_144 (pres `withSuffix` "iāmur"),
	_145 (pres `withSuffix` "iāminī"),
	_146 (pres `withSuffix` "iantur"),
	_151 (pres `withSuffix` "erer"),
	_152 (pres `withSuffix` "erēris"),
	_153 (pres `withSuffix` "erētur"),
	_154 (pres `withSuffix` "erēmur"),
	_155 (pres `withSuffix` "erēminī"),
	_156 (pres `withSuffix` "erentur"),
	_161 (pres `withSuffix` "e"),
	_171 (pres `withSuffix` "ite"),
	_181 (pres `withSuffix` "ere"),
	_182 (perf `withSuffix` "isse"),
	_183 (sup `withSuffix` "ūrus esse"),
	_184 (pres `withSuffix` "ī"),
	_185 (sup `withSuffix` "us esse"),
	_186 (sup `withSuffix` "um")]
	++ (act_pres_participle (pres `withSuffix` "iē"))
	++ (act_fut_participle sup)
	++ (pass_perf_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "ie"))
	++ (passive_past_forms sup)
la_conj_3rd_IO _ = Nothing

la_conj_3rd_IO_depon (pres:sup:special:_) = Just $ la_conj_3rd_IO_depon' pres sup special
la_conj_3rd_IO_depon (pres:sup:_) = Just $ la_conj_3rd_IO_depon' pres sup sup
la_conj_3rd_IO_depon _ = Nothing

la_conj_3rd_IO_depon' :: T.Text -> T.Text -> T.Text -> [VerbForm]
la_conj_3rd_IO_depon' pres sup special = [
	_11 (pres `withSuffix` "ior"),
	_12 (pres `withSuffix` "eris"),
	_13 (pres `withSuffix` "itur"),
	_14 (pres `withSuffix` "imur"),
	_15 (pres `withSuffix` "iminī"),
	_16 (pres `withSuffix` "iuntur"),
	_21 (pres `withSuffix` "iar"),
	_22 (pres `withSuffix` "iēris"),
	_23 (pres `withSuffix` "iētur"),
	_24 (pres `withSuffix` "iēmur"),
	_25 (pres `withSuffix` "iēminī"),
	_26 (pres `withSuffix` "ientur"),
	_31 (pres `withSuffix` "iēbar"),
	_32 (pres `withSuffix` "iēbāris"),
	_33 (pres `withSuffix` "iēbātur"),
	_34 (pres `withSuffix` "iēbāmur"),
	_35 (pres `withSuffix` "iēbāminī"),
	_36 (pres `withSuffix` "iēbantur"),
	_101 (pres `withSuffix` "iar"),
	_102 (pres `withSuffix` "iāris"),
	_103 (pres `withSuffix` "iātur"),
	_104 (pres `withSuffix` "iāmur"),
	_105 (pres `withSuffix` "iāminī"),
	_106 (pres `withSuffix` "iantur"),
	_111 (pres `withSuffix` "erer"),
	_112 (pres `withSuffix` "erēris"),
	_113 (pres `withSuffix` "erētur"),
	_114 (pres `withSuffix` "erēmur"),
	_115 (pres `withSuffix` "erēminī"),
	_116 (pres `withSuffix` "erentur"),
	_161 (pres `withSuffix` "ere"),
	_171 (pres `withSuffix` "iminī"),
	_181 (pres `withSuffix` "ī"),
	_182 (sup `withSuffix` "us esse"),
	_183 (special `withSuffix` "ūrus esse")]
	++ (act_pres_participle (pres `withSuffix` "iē"))
	++ (act_perf_participle sup)
	++ (act_fut_participle special)
	++ (pass_fut_participle (special `withSuffix` "ie"))
	++ (depon_past_forms sup)

la_conj_3rd_IO_nopass (pres:perf:sup:_) =
	la_conj_3rd [pres,perf,sup] >>= return.withoutPassiveForms
la_conj_3rd_IO_nopass (pres:perf:_) =
	la_conj_3rd [pres,perf, (T.pack "XXX")] >>= return.withoutPassiveForms.withoutSupineForms
la_conj_3rd_IO_nopass _ = Nothing

la_conj_3rd_IO_pass3p (pres:perf:sup:_) =
	la_conj_3rd [pres,perf,sup] >>= return.filter3pPassiveForms
la_conj_3rd_IO_pass3p _ = Nothing

la_conj_3rd_depon (pres:sup:_) = Just $ [
	_11 (pres `withSuffix` "or"),
	_12 (pres `withSuffix` "eris"),
	_13 (pres `withSuffix` "itur"),
	_14 (pres `withSuffix` "imur"),
	_15 (pres `withSuffix` "iminī"),
	_16 (pres `withSuffix` "untur"),
	_21 (pres `withSuffix` "ar"),
	_22 (pres `withSuffix` "ēris"),
	_23 (pres `withSuffix` "ētur"),
	_24 (pres `withSuffix` "ēmur"),
	_25 (pres `withSuffix` "ēminī"),
	_26 (pres `withSuffix` "entur"),
	_31 (pres `withSuffix` "ēbar"),
	_32 (pres `withSuffix` "ēbāris"),
	_33 (pres `withSuffix` "ēbātur"),
	_34 (pres `withSuffix` "ēbāmur"),
	_35 (pres `withSuffix` "ēbāminī"),
	_36 (pres `withSuffix` "ēbantur"),
-- TODO deponent pasts
	_101 (pres `withSuffix` "ar"),
	_102 (pres `withSuffix` "āris"),
	_103 (pres `withSuffix` "ātur"),
	_104 (pres `withSuffix` "āmur"),
	_105 (pres `withSuffix` "āminī"),
	_106 (pres `withSuffix` "antur"),
-- TODO deponent pasts
	_111 (pres `withSuffix` "erer"),
	_112 (pres `withSuffix` "erēris"),
	_113 (pres `withSuffix` "erētur"),
	_114 (pres `withSuffix` "erēmur"),
	_115 (pres `withSuffix` "erēminī"),
	_116 (pres `withSuffix` "erentur"),
	_161 (pres `withSuffix` "ere"),
	_171 (pres `withSuffix` "iminī"),
	_181 (pres `withSuffix` "ī"),
	_182 (sup `withSuffix` "us esse"),
	_183 (sup `withSuffix` "ūrus esse")]
	++ (act_pres_participle (pres `withSuffix` "ē"))
	++ (act_perf_participle sup)
	++ (act_fut_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "e"))
	++ (depon_past_forms sup)
la_conj_3rd_depon _ = Nothing

la_conj_3rd_IO_facio (stem:_) = Just $ [
	_11 (stem `withSuffix` "aciō"),
	_12 (stem `withSuffix` "acis"),
	_13 (stem `withSuffix` "acit"),
	_14 (stem `withSuffix` "acimus"),
	_15 (stem `withSuffix` "acitis"),
	_16 (stem `withSuffix` "aciunt"),
	_21 (stem `withSuffix` "aciam"),
	_22 (stem `withSuffix` "aciēs"),
	_23 (stem `withSuffix` "aciet"),
	_24 (stem `withSuffix` "aciēmus"),
	_25 (stem `withSuffix` "aciētis"),
	_26 (stem `withSuffix` "acient"),
	_31 (stem `withSuffix` "aciēbam"),
	_32 (stem `withSuffix` "aciēbās"),
	_33 (stem `withSuffix` "aciēbat"),
	_34 (stem `withSuffix` "aciēbāmus"),
	_35 (stem `withSuffix` "aciēbātis"),
	_36 (stem `withSuffix` "aciēbant"),
	_41 (stem `withSuffix` "ēcī"),
	_42 (stem `withSuffix` "ēcistī"),
	_43 (stem `withSuffix` "ēcit"),
	_44 (stem `withSuffix` "ēcimus"),
	_45 (stem `withSuffix` "ēcistis"),
	_46 (stem `withSuffix` "ēcērunt"),
	_51 (stem `withSuffix` "ēcerō"),
	_52 (stem `withSuffix` "ēceris"),
	_53 (stem `withSuffix` "ēcerit"),
	_54 (stem `withSuffix` "ēcerimus"),
	_55 (stem `withSuffix` "ēceritis"),
	_56 (stem `withSuffix` "ēcerint"),
	_61 (stem `withSuffix` "ēceram"),
	_62 (stem `withSuffix` "ēcerās"),
	_63 (stem `withSuffix` "ēcerat"),
	_64 (stem `withSuffix` "ēcerāmus"),
	_65 (stem `withSuffix` "ēcerātis"),
	_66 (stem `withSuffix` "ēcerant"),
	_71 (stem `withSuffix` "īō"),
	_72 (stem `withSuffix` "īs"),
	_73 (stem `withSuffix` "it"),
	_74 (stem `withSuffix` "īmus"),
	_75 (stem `withSuffix` "ītis"),
	_76 (stem `withSuffix` "īunt"),
	_81 (stem `withSuffix` "īam"),
	_82 (stem `withSuffix` "īēs"),
	_83 (stem `withSuffix` "īet"),
	_84 (stem `withSuffix` "īēmus"),
	_85 (stem `withSuffix` "īētis"),
	_86 (stem `withSuffix` "īent"),
	_91 (stem `withSuffix` "īēbam"),
	_92 (stem `withSuffix` "īēbās"),
	_93 (stem `withSuffix` "īēbat"),
	_94 (stem `withSuffix` "īēbāmus"),
	_95 (stem `withSuffix` "īēbātis"),
	_96 (stem `withSuffix` "īēbant"),
	_101 (stem `withSuffix` "aciam"),
	_102 (stem `withSuffix` "aciās"),
	_103 (stem `withSuffix` "aciat"),
	_104 (stem `withSuffix` "aciāmus"),
	_105 (stem `withSuffix` "aciātis"),
	_106 (stem `withSuffix` "aciant"),
	_111 (stem `withSuffix` "acerem"),
	_112 (stem `withSuffix` "acerēs"),
	_113 (stem `withSuffix` "aceret"),
	_114 (stem `withSuffix` "acerēmus"),
	_115 (stem `withSuffix` "acerētis"),
	_116 (stem `withSuffix` "acerent"),
	_121 (stem `withSuffix` "ēcerim"),
	_122 (stem `withSuffix` "ēcerīs"),
	_123 (stem `withSuffix` "ēcerit"),
	_124 (stem `withSuffix` "ēcerīmus"),
	_125 (stem `withSuffix` "ēcerītis"),
	_126 (stem `withSuffix` "ēcerint"),
	_131 (stem `withSuffix` "ēcissem"),
	_132 (stem `withSuffix` "ēcissēs"),
	_133 (stem `withSuffix` "ēcisset"),
	_134 (stem `withSuffix` "ēcissēmus"),
	_135 (stem `withSuffix` "ēcissētis"),
	_136 (stem `withSuffix` "ēcissent"),
	_141 (stem `withSuffix` "īam"),
	_142 (stem `withSuffix` "īās"),
	_143 (stem `withSuffix` "īat"),
	_144 (stem `withSuffix` "īāmus"),
	_145 (stem `withSuffix` "īātis"),
	_146 (stem `withSuffix` "īant"),
	_151 (stem `withSuffix` "ierem"),
	_152 (stem `withSuffix` "ierēs"),
        _153 (stem `withSuffix` "ieret"),
        _154 (stem `withSuffix` "ierēmus"),
        _155 (stem `withSuffix` "ierētis"),
	_156 (stem `withSuffix` "ierent"),
	_161 (stem `withSuffix` "ac"),
	_171 (stem `withSuffix` "acite"),
	_181 (stem `withSuffix` "acere"),
	_182 (stem `withSuffix` "ecisse"),
	_183 (stem `withSuffix` "actūrus esse"),
	_184 (stem `withSuffix` "ierī"),
	_185 (stem `withSuffix` "actus esse"),
	_186 (stem `withSuffix` "actum")]
	++ (act_pres_participle (stem `withSuffix` "aciē"))
	++ (act_fut_participle (stem `withSuffix` "act"))
	++ (pass_perf_participle (stem `withSuffix` "act"))
	++ (passive_past_forms (stem `withSuffix` "act"))
la_conj_3rd_IO_facio _ = Nothing

la_conj_3rd_fero (pres:perf:sup:_) = Just $ [
	_11 (pres `withSuffix` "ō"),
	_12 (pres `withSuffix` "s"),
	_13 (pres `withSuffix` "t"),
	_14 (pres `withSuffix` "imus"),
	_15 (pres `withSuffix` "tis"),
	_16 (pres `withSuffix` "unt"),
	_21 (pres `withSuffix` "am"),
	_22 (pres `withSuffix` "ēs"),
	_23 (pres `withSuffix` "et"),
	_24 (pres `withSuffix` "ēmus"),
	_25 (pres `withSuffix` "ētis"),
	_26 (pres `withSuffix` "ent"),
	_31 (pres `withSuffix` "ēbam"),
	_32 (pres `withSuffix` "ēbās"),
	_33 (pres `withSuffix` "ēbat"),
	_34 (pres `withSuffix` "ēbāmus"),
	_35 (pres `withSuffix` "ēbātis"),
	_36 (pres `withSuffix` "ēbant"),
	_41 (perf `withSuffix` "ī"),
	_42 (perf `withSuffix` "istī"),
	_43 (perf `withSuffix` "it"),
	_44 (perf `withSuffix` "imus"),
	_45 (perf `withSuffix` "istis"),
	_46 (perf `withSuffix` "ērunt"),
	_51 (perf `withSuffix` "erō"),
	_52 (perf `withSuffix` "eris"),
	_53 (perf `withSuffix` "erit"),
	_54 (perf `withSuffix` "erimus"),
	_55 (perf `withSuffix` "eritis"),
	_56 (perf `withSuffix` "erint"),
	_61 (perf `withSuffix` "eram"),
	_62 (perf `withSuffix` "erās"),
	_63 (perf `withSuffix` "erat"),
	_64 (perf `withSuffix` "erāmus"),
	_65 (perf `withSuffix` "erātis"),
	_66 (perf `withSuffix` "erant"),
	_71 (pres `withSuffix` "or"),
	_72 (pres `withSuffix` "ris"),
	_73 (pres `withSuffix` "tur"),
	_74 (pres `withSuffix` "imur"),
	_75 (pres `withSuffix` "iminī"),
	_76 (pres `withSuffix` "untur"),
	_81 (pres `withSuffix` "ar"),
	_82 (pres `withSuffix` "ēris"),
	_83 (pres `withSuffix` "ētur"),
	_84 (pres `withSuffix` "ēmur"),
	_85 (pres `withSuffix` "ēminī"),
	_86 (pres `withSuffix` "entur"),
	_91 (pres `withSuffix` "ēbar"),
	_92 (pres `withSuffix` "ēbāris"),
	_93 (pres `withSuffix` "ēbātur"),
	_94 (pres `withSuffix` "ēbāmur"),
	_95 (pres `withSuffix` "ēbāminī"),
	_96 (pres `withSuffix` "ēbantur"),
	_101 (pres `withSuffix` "am"),
	_102 (pres `withSuffix` "ās"),
	_103 (pres `withSuffix` "at"),
	_104 (pres `withSuffix` "āmus"),
	_105 (pres `withSuffix` "ātis"),
	_106 (pres `withSuffix` "ant"),
	_111 (pres `withSuffix` "rem"),
	_112 (pres `withSuffix` "rēs"),
	_113 (pres `withSuffix` "ret"),
	_114 (pres `withSuffix` "rēmus"),
	_115 (pres `withSuffix` "rētis"),
	_116 (pres `withSuffix` "rent"),
	_121 (perf `withSuffix` "erim"),
	_122 (perf `withSuffix` "erīs"),
	_123 (perf `withSuffix` "erit"),
	_124 (perf `withSuffix` "erīmus"),
	_125 (perf `withSuffix` "erītis"),
	_126 (perf `withSuffix` "erint"),
	_131 (perf `withSuffix` "issem"),
	_132 (perf `withSuffix` "issēs"),
	_133 (perf `withSuffix` "isset"),
	_134 (perf `withSuffix` "issēmus"),
	_135 (perf `withSuffix` "issētis"),
	_136 (perf `withSuffix` "issent"),
	_141 (pres `withSuffix` "ar"),
	_142 (pres `withSuffix` "āris"),
	_143 (pres `withSuffix` "ātur"),
	_144 (pres `withSuffix` "āmur"),
	_145 (pres `withSuffix` "āminī"),
	_146 (pres `withSuffix` "antur"),
	_151 (pres `withSuffix` "rer"),
	_152 (pres `withSuffix` "rēris"),
	_153 (pres `withSuffix` "rētur"),
	_154 (pres `withSuffix` "rēmur"),
	_155 (pres `withSuffix` "rēminī"),
	_156 (pres `withSuffix` "rentur"),
	_161 pres,
	_171 (pres `withSuffix` "te"),
	_181 (pres `withSuffix` "re"),
	_182 (perf `withSuffix` "isse"),
	_183 (sup `withSuffix` "ūrus esse"),
	_184 (pres `withSuffix` "rī"),
	_185 (sup `withSuffix` "us esse"),
	_186 (sup `withSuffix` "um")]
	++ (act_pres_participle (pres `withSuffix` "ē"))
	++ (act_fut_participle sup)
	++ (pass_perf_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "e"))
	++ (passive_past_forms sup)
la_conj_3rd_fero _ = Nothing

la_conj_3rd_no234 (pres:_) = 
	la_conj_3rd [pres,(T.pack "XX"), (T.pack "YY")] >>= return.without234
la_conj_3rd_no234 _ = Nothing

la_conj_3rd_nopass (pres:perf:sup:_) = 
	la_conj_3rd [pres,perf,sup] >>= return.withoutPassiveForms
la_conj_3rd_nopass (pres:perf:_) =
	la_conj_3rd [pres,perf,(T.pack "XX")] >>= return.withoutPassiveForms.withoutSupineForms
la_conj_3rd_nopass _ = Nothing

la_conj_3rd_pass3p (pres:perf:sup:_) =
	la_conj_3rd [pres,perf,sup] >>= return.filter3pPassiveForms
la_conj_3rd_pass3p _ = Nothing

-- TODO: semi perfect forms
la_conj_3rd_semi (pres:sup:_) = Just $ [
	_11 (pres `withSuffix` "ō"),
	_12 (pres `withSuffix` "is"),
	_13 (pres `withSuffix` "it"),
	_14 (pres `withSuffix` "imus"),
	_15 (pres `withSuffix` "itis"),
	_16 (pres `withSuffix` "unt"),
	_21 (pres `withSuffix` "am"),
	_22 (pres `withSuffix` "ēs"),
	_23 (pres `withSuffix` "et"),
	_24 (pres `withSuffix` "ēmus"),
	_25 (pres `withSuffix` "ētis"),
	_26 (pres `withSuffix` "ent"),
	_31 (pres `withSuffix` "ēbam"),
	_32 (pres `withSuffix` "ēbās"),
	_33 (pres `withSuffix` "ēbat"),
	_34 (pres `withSuffix` "ēbāmus"),
	_35 (pres `withSuffix` "ēbātis"),
	_36 (pres `withSuffix` "ēbant"),
	_101 (pres `withSuffix` "am"),
	_102 (pres `withSuffix` "ās"),
	_103 (pres `withSuffix` "at"),
	_104 (pres `withSuffix` "āmus"),
	_105 (pres `withSuffix` "ātis"),
	_106 (pres `withSuffix` "ant"),
	_111 (pres `withSuffix` "erem"),
	_112 (pres `withSuffix` "erēs"),
	_113 (pres `withSuffix` "eret"),
	_114 (pres `withSuffix` "erēmus"),
	_115 (pres `withSuffix` "erētis"),
	_116 (pres `withSuffix` "erent"),
	_161 (pres `withSuffix` "e"),
	_171 (pres `withSuffix` "ite"),
	_181 (pres `withSuffix` "ere"),
	_182 (sup `withSuffix` "us esse"),
	_183 (sup `withSuffix` "ūrus esse")]
	++ (act_pres_participle (pres `withSuffix` "ē"))
	++ (act_perf_participle sup)
	++ (act_fut_participle sup)
	++ (depon_past_forms sup)
la_conj_3rd_semi _ = Nothing

-- TODO: semi perfect forms
la_conj_3rd_semi_fio (pres:_) = Just $ [
	_11 (pres `withSuffix` "īō"),
	_12 (pres `withSuffix` "īs"),
	_13 (pres `withSuffix` "it"),
	_14 (pres `withSuffix` "īmus"),
	_15 (pres `withSuffix` "ītis"),
	_16 (pres `withSuffix` "īunt"),
	_21 (pres `withSuffix` "īam"),
	_22 (pres `withSuffix` "īēs"),
	_23 (pres `withSuffix` "īet"),
	_24 (pres `withSuffix` "īēmus"),
	_25 (pres `withSuffix` "īētis"),
	_26 (pres `withSuffix` "īent"),
	_31 (pres `withSuffix` "īēbam"),
	_32 (pres `withSuffix` "īēbās"),
	_33 (pres `withSuffix` "īēbat"),
	_34 (pres `withSuffix` "īēbāmus"),
	_35 (pres `withSuffix` "īēbātis"),
	_36 (pres `withSuffix` "īebant"),
	_101 (pres `withSuffix` "am"),
	_102 (pres `withSuffix` "ās"),
	_103 (pres `withSuffix` "at"),
	_104 (pres `withSuffix` "āmus"),
	_105 (pres `withSuffix` "ātis"),
	_106 (pres `withSuffix` "ant"),
	_111 (pres `withSuffix` "erem"),
	_112 (pres `withSuffix` "erēs"),
	_113 (pres `withSuffix` "eret"),
	_114 (pres `withSuffix` "erēmus"),
	_115 (pres `withSuffix` "erētis"),
	_116 (pres `withSuffix` "erent"),
	_161 (pres `withSuffix` "ī"),
	_171 (pres `withSuffix` "īte"),
	_181 (pres `withSuffix` "erī"),
	_182 (pres `withSuffix` "actus esse"),
	_183 (pres `withSuffix` "actum īrī")]
	++ act_perf_participle (pres `withSuffix` "act")
	++ depon_past_forms (pres `withSuffix` "actus")
la_conj_3rd_semi_fio _ = Nothing

la_conj_4th (pres:perf:sup:_) = Just $ [
	_11 (pres `withSuffix` "iō"),
	_12 (pres `withSuffix` "īs"),
	_13 (pres `withSuffix` "it"),
	_14 (pres `withSuffix` "īmus"),
	_15 (pres `withSuffix` "ītis"),
	_16 (pres `withSuffix` "iunt"),
	_21 (pres `withSuffix` "iam"),
	_22 (pres `withSuffix` "iēs"),
	_23 (pres `withSuffix` "iet"),
	_24 (pres `withSuffix` "iēmus"),
	_25 (pres `withSuffix` "iētis"),
	_26 (pres `withSuffix` "ient"),
	_31 (pres `withSuffix` "iēbam"),
	_32 (pres `withSuffix` "iēbās"),
	_33 (pres `withSuffix` "iēbat"),
	_34 (pres `withSuffix` "iēbāmus"),
	_35 (pres `withSuffix` "iēbātis"),
	_36 (pres `withSuffix` "iēbant"),
	_41 (perf `withSuffix` "ī"),
	_42 (perf `withSuffix` "istī"),
	_43 (perf `withSuffix` "it"),
	_44 (perf `withSuffix` "imus"),
	_45 (perf `withSuffix` "istis"),
	_46 (perf `withSuffix` "ērunt"),
	_51 (perf `withSuffix` "erō"),
	_52 (perf `withSuffix` "eris"),
	_53 (perf `withSuffix` "erit"),
	_54 (perf `withSuffix` "erimus"),
	_55 (perf `withSuffix` "eritis"),
	_56 (perf `withSuffix` "erint"),
	_61 (perf `withSuffix` "eram"),
	_62 (perf `withSuffix` "erās"),
	_63 (perf `withSuffix` "erat"),
	_64 (perf `withSuffix` "erāmus"),
	_65 (perf `withSuffix` "erātis"),
	_66 (perf `withSuffix` "erant"),
	_71 (pres `withSuffix` "ior"),
	_72 (pres `withSuffix` "īris"),
	_73 (pres `withSuffix` "ītur"),
	_74 (pres `withSuffix` "īmur"),
	_75 (pres `withSuffix` "īminī"),
	_76 (pres `withSuffix` "iuntur"),
	_81 (pres `withSuffix` "iar"),
	_82 (pres `withSuffix` "iēris"),
	_83 (pres `withSuffix` "iētur"),
	_84 (pres `withSuffix` "iēmur"),
	_85 (pres `withSuffix` "iēminī"),
	_86 (pres `withSuffix` "ientur"),
	_91 (pres `withSuffix` "iēbar"),
	_92 (pres `withSuffix` "iēbāris"),
	_93 (pres `withSuffix` "iēbātur"),
	_94 (pres `withSuffix` "iēbāmur"),
	_95 (pres `withSuffix` "iēbāminī"),
	_96 (pres `withSuffix` "iēbantur"),
	_101 (pres `withSuffix` "iam"),
	_102 (pres `withSuffix` "iās"),
	_103 (pres `withSuffix` "iat"),
	_104 (pres `withSuffix` "iāmus"),
	_105 (pres `withSuffix` "iātis"),
	_106 (pres `withSuffix` "iant"),
	_111 (pres `withSuffix` "īrem"),
	_112 (pres `withSuffix` "īrēs"),
	_113 (pres `withSuffix` "īret"),
	_114 (pres `withSuffix` "īrēmus"),
	_115 (pres `withSuffix` "īrētis"),
	_116 (pres `withSuffix` "īrent"),
	_121 (perf `withSuffix` "erim"),
	_122 (perf `withSuffix` "erīs"),
	_123 (perf `withSuffix` "erit"),
	_124 (perf `withSuffix` "erīmus"),
	_125 (perf `withSuffix` "erītis"),
	_126 (perf `withSuffix` "erint"),
	_131 (perf `withSuffix` "issem"),
	_132 (perf `withSuffix` "issēs"),
	_133 (perf `withSuffix` "isset"),
	_134 (perf `withSuffix` "issēmus"),
	_135 (perf `withSuffix` "issētis"),
	_136 (perf `withSuffix` "issent"),
	_141 (pres `withSuffix` "iar"),
	_142 (pres `withSuffix` "iāris"),
	_143 (pres `withSuffix` "iātur"),
	_144 (pres `withSuffix` "iāmur"),
	_145 (pres `withSuffix` "iāminī"),
	_146 (pres `withSuffix` "iantur"),
	_151 (pres `withSuffix` "īrer"),
	_152 (pres `withSuffix` "īrēris"),
	_153 (pres `withSuffix` "īrētur"),
	_154 (pres `withSuffix` "īrēmur"),
	_155 (pres `withSuffix` "īrēminī"),
	_156 (pres `withSuffix` "īrentur"),
	_161 (pres `withSuffix` "ī"),
	_171 (pres `withSuffix` "īte"),
	_181 (pres `withSuffix` "īre"),
	_182 (perf `withSuffix` "isse"),
	_183 (sup `withSuffix` "ūrus esse"),
	_184 (pres `withSuffix` "īrī"),
	_185 (sup `withSuffix` "us esse"),
	_186 (sup `withSuffix` "um")]
	++ (act_pres_participle (pres `withSuffix` "iē"))
	++ (act_fut_participle sup)
	++ (pass_perf_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "ie"))
	++ (passive_past_forms sup)
la_conj_4th _ = Nothing

-- TODO deponent perfect forms
la_conj_4th_depon (pres:sup:_) = Just $ [
	_11 (pres `withSuffix` "ior"),
	_12 (pres `withSuffix` "īris"),
	_13 (pres `withSuffix` "ītur"),
	_14 (pres `withSuffix` "īmur"),
	_15 (pres `withSuffix` "īminī"),
	_16 (pres `withSuffix` "iuntur"),
	_21 (pres `withSuffix` "iar"),
	_22 (pres `withSuffix` "iēris"),
	_23 (pres `withSuffix` "iētur"),
	_24 (pres `withSuffix` "iēmur"),
	_25 (pres `withSuffix` "iēminī"),
	_26 (pres `withSuffix` "ientur"),
	_31 (pres `withSuffix` "iēbar"),
	_32 (pres `withSuffix` "iēbāris"),
	_33 (pres `withSuffix` "iēbātur"),
	_34 (pres `withSuffix` "iēbāmur"),
	_35 (pres `withSuffix` "iēbāminī"),
	_36 (pres `withSuffix` "iēbantur"),
	_101 (pres `withSuffix` "iar"),
	_102 (pres `withSuffix` "iāris"),
	_103 (pres `withSuffix` "iātur"),
	_104 (pres `withSuffix` "iāmur"),
	_105 (pres `withSuffix` "iāminī"),
	_106 (pres `withSuffix` "iantur"),
	_111 (pres `withSuffix` "īrer"),
	_112 (pres `withSuffix` "īrēris"),
	_113 (pres `withSuffix` "īrētur"),
	_114 (pres `withSuffix` "īrēmur"),
	_115 (pres `withSuffix` "īrēminī"),
	_116 (pres `withSuffix` "īrentur"),
	_161 (pres `withSuffix` "īre"),
	_171 (pres `withSuffix` "īminī"),
	_181 (pres `withSuffix` "īrī"),
	_182 (sup `withSuffix` "us esse"),
	_183 (sup `withSuffix` "ūrus esse")]
	++ (act_pres_participle (pres `withSuffix` "iē"))
	++ (act_perf_participle (sup `withSuffix` "us"))
	++ (act_fut_participle sup)
	++ (pass_fut_participle (pres `withSuffix` "ie"))
	++ (depon_past_forms sup)
la_conj_4th_depon _ = Nothing

la_conj_4th_nopass (pres:perf:sup:_) = 
	la_conj_4th [pres,perf,sup] >>= return.withoutPassiveForms
la_conj_4th_nopass (pres:perf:_) =
	la_conj_4th [pres,perf,(T.pack "XX")] >>= return.withoutPassiveForms.withoutSupineForms
la_conj_4th_nopass _ = Nothing

la_conj_4th_pass3p (pres:perf:sup:_) =
	la_conj_4th [pres,perf,sup] >>= return.filter3pPassiveForms
la_conj_4th_pass3p _ = Nothing

la_conj_irr_volo :: [T.Text] -> Maybe [VerbForm]
la_conj_irr_volo args =
	case args of
		[] -> Just la_conj_irr_volo'
		[arg1,arg2] | (arg1 == (T.pack "no")) && (arg2 == (T.pack "11=no")) -> Just la_conj_irr_nolo'
		[arg1,arg2] | (T.pack "11=") `T.isPrefixOf` arg2 -> Just $ la_conj_irr_volo_11' arg1
		[arg1,arg2] | (T.pack "12=") `T.isPrefixOf` arg2 -> Just $ la_conj_irr_volo_12' arg1
		_ -> Nothing

la_conj_irr_volo' :: [VerbForm]
la_conj_irr_volo' = [
	_11 (T.pack "volō"),
	_12 (T.pack "vīs"),
	_13 (T.pack "vult"),
	_14 (T.pack "volumus"),
	_15 (T.pack "vultis"),
	_16 (T.pack "volunt"),
	_21 (T.pack "volam"),
	_22 (T.pack "volēs"),
	_23 (T.pack "volet"),
	_24 (T.pack "volēmus"),
	_25 (T.pack "volētis"),
	_26 (T.pack "volent"),
	_31 (T.pack "volēbam"),
	_32 (T.pack "volēbās"),
	_33 (T.pack "volēbat"),
	_34 (T.pack "volēbāmus"),
	_35 (T.pack "volēbātis"),
	_36 (T.pack "volēbant"),
	_41 (T.pack "voluī"),
	_42 (T.pack "voluistī"),
	_43 (T.pack "voluit"),
	_44 (T.pack "voluimus"),
	_45 (T.pack "voluistis"),
	_46 (T.pack "voluērunt"),
	_51 (T.pack "voluerō"),
	_52 (T.pack "volueris"),
	_53 (T.pack "voluerit"),
	_54 (T.pack "voluerimus"),
	_55 (T.pack "volueritis"),
	_56 (T.pack "voluerint"),
	_61 (T.pack "volueram"),
	_62 (T.pack "voluerās"),
	_63 (T.pack "voluerat"),
	_64 (T.pack "voluerāmus"),
	_65 (T.pack "voluerātis"),
	_66 (T.pack "voluerant"),
	_101 (T.pack "velim"),
	_102 (T.pack "velīs"),
	_103 (T.pack "velit"),
	_104 (T.pack "velīmus"),
	_105 (T.pack "velītis"),
	_106 (T.pack "velint"),
	_111 (T.pack "vellem"),
	_112 (T.pack "vellēs"),
	_113 (T.pack "vellet"),
	_114 (T.pack "vellēmus"),
	_115 (T.pack "vellētis"),
	_116 (T.pack "vellent"),
	_121 (T.pack "voluerim"),
	_122 (T.pack "voluerīs"),
	_123 (T.pack "voluerit"),
	_124 (T.pack "voluerīmus"),
	_125 (T.pack "voluerītis"),
	_126 (T.pack "voluerint"),
	_131 (T.pack "voluissem"),
	_132 (T.pack "voluissēs"),
	_133 (T.pack "voluisset"),
	_134 (T.pack "voluissēmus"),
	_135 (T.pack "voluissētis"),
	_136 (T.pack "voluissent"),
	_181 (T.pack "velle"),
	_182 (T.pack "voluisse")]
	++ act_pres_participle (T.pack "vole")

la_conj_irr_volo_11' :: T.Text -> [VerbForm]
la_conj_irr_volo_11' a11 = [
	_11 (a11 `withSuffix` "lō"),
	_12 (a11 `withSuffix` "vīs"),
	_13 (a11 `withSuffix` "vult"),
	_14 (a11 `withSuffix` "lumus"),
	_15 (a11 `withSuffix` "vultis"),
	_16 (a11 `withSuffix` "lunt"),
	_21 (a11 `withSuffix` "lam"),
	_22 (a11 `withSuffix` "lēs"),
	_23 (a11 `withSuffix` "let"),
	_24 (a11 `withSuffix` "lēmus"),
	_25 (a11 `withSuffix` "lētis"),
	_26 (a11 `withSuffix` "lent"),
	_31 (a11 `withSuffix` "lēbam"),
	_32 (a11 `withSuffix` "lēbās"),
	_33 (a11 `withSuffix` "lēbat"),
	_34 (a11 `withSuffix` "lēbāmus"),
	_35 (a11 `withSuffix` "lēbātis"),
	_36 (a11 `withSuffix` "lēbant"),
	_41 (a11 `withSuffix` "luī"),
	_42 (a11 `withSuffix` "luistī"),
	_43 (a11 `withSuffix` "luit"),
	_44 (a11 `withSuffix` "luimus"),
	_45 (a11 `withSuffix` "luistis"),
	_46 (a11 `withSuffix` "luērunt"),
	_51 (a11 `withSuffix` "luerō"),
	_52 (a11 `withSuffix` "lueris"),
	_53 (a11 `withSuffix` "luerit"),
	_54 (a11 `withSuffix` "luerimus"),
	_55 (a11 `withSuffix` "lueritis"),
	_56 (a11 `withSuffix` "luerint"),
	_61 (a11 `withSuffix` "lueram"),
	_62 (a11 `withSuffix` "luerās"),
	_63 (a11 `withSuffix` "luerat"),
	_64 (a11 `withSuffix` "luerāmus"),
	_65 (a11 `withSuffix` "luerātis"),
	_66 (a11 `withSuffix` "luerant"),
	_101 (a11 `withSuffix` "lim"),
	_102 (a11 `withSuffix` "līs"),
	_103 (a11 `withSuffix` "lit"),
	_104 (a11 `withSuffix` "līmus"),
	_105 (a11 `withSuffix` "lītis"),
	_106 (a11 `withSuffix` "lint"),
	_111 (a11 `withSuffix` "llem"),
	_112 (a11 `withSuffix` "llēs"),
	_113 (a11 `withSuffix` "llet"),
	_114 (a11 `withSuffix` "llēmus"),
	_115 (a11 `withSuffix` "llētis"),
	_116 (a11 `withSuffix` "llent"),
	_121 (a11 `withSuffix` "luerim"),
	_122 (a11 `withSuffix` "luerīs"),
	_123 (a11 `withSuffix` "luerit"),
	_124 (a11 `withSuffix` "luerīmus"),
	_125 (a11 `withSuffix` "luerītis"),
	_126 (a11 `withSuffix` "luerint"),
	_131 (a11 `withSuffix` "luissem"),
	_132 (a11 `withSuffix` "luissēs"),
	_133 (a11 `withSuffix` "luisset"),
	_134 (a11 `withSuffix` "luissēmus"),
	_135 (a11 `withSuffix` "luissētis"),
	_136 (a11 `withSuffix` "luissent"),
	_181 (a11 `withSuffix` "lle"),
	_182 (a11 `withSuffix` "luisse") ]

la_conj_irr_nolo' :: [VerbForm]
la_conj_irr_nolo' = [
	_11 (T.pack "nolō"),
	_12 (T.pack "non vīs"),
	_13 (T.pack "non vult"),
	_14 (T.pack "nolumus"),
	_15 (T.pack "non vultis"),
	_16 (T.pack "nolunt"),
	_21 (T.pack "nolam"),
	_22 (T.pack "nolēs"),
	_23 (T.pack "nolet"),
	_24 (T.pack "nolēmus"),
	_25 (T.pack "nolētis"),
	_26 (T.pack "nolent"),
	_31 (T.pack "nolēbam"),
	_32 (T.pack "nolēbās"),
	_33 (T.pack "nolēbat"),
	_34 (T.pack "nolēbāmus"),
	_35 (T.pack "nolēbātis"),
	_36 (T.pack "nolēbant"),
	_41 (T.pack "noluī"),
	_42 (T.pack "noluistī"),
	_43 (T.pack "noluit"),
	_44 (T.pack "noluimus"),
	_45 (T.pack "noluistis"),
	_46 (T.pack "noluērunt"),
	_51 (T.pack "noluerō"),
	_52 (T.pack "nolueris"),
	_53 (T.pack "noluerit"),
	_54 (T.pack "noluerimus"),
	_55 (T.pack "nolueritis"),
	_56 (T.pack "noluerint"),
	_61 (T.pack "nolueram"),
	_62 (T.pack "noluerās"),
	_63 (T.pack "noluerat"),
	_64 (T.pack "noluerāmus"),
	_65 (T.pack "noluerātis"),
	_66 (T.pack "noluerant"),
	_101 (T.pack "nolim"),
	_102 (T.pack "nolīs"),
	_103 (T.pack "nolit"),
	_104 (T.pack "nolīmus"),
	_105 (T.pack "nolītis"),
	_106 (T.pack "nolint"),
	_111 (T.pack "nollem"),
	_112 (T.pack "nollēs"),
	_113 (T.pack "nollet"),
	_114 (T.pack "nollēmus"),
	_115 (T.pack "nollētis"),
	_116 (T.pack "nollent"),
	_121 (T.pack "noluerim"),
	_122 (T.pack "noluerīs"),
	_123 (T.pack "noluerit"),
	_124 (T.pack "noluerīmus"),
	_125 (T.pack "noluerītis"),
	_126 (T.pack "noluerint"),
	_131 (T.pack "noluissem"),
	_132 (T.pack "noluissēs"),
	_133 (T.pack "noluisset"),
	_134 (T.pack "noluissēmus"),
	_135 (T.pack "noluissētis"),
	_136 (T.pack "noluissent"),
	_161 (T.pack "nōlī"),
	_171 (T.pack "nōlīte"),
	_181 (T.pack "nolle"),
	_182 (T.pack "noluisse")]
	++ act_pres_participle (T.pack "nole")

la_conj_irr_volo_12' :: T.Text -> [VerbForm]
la_conj_irr_volo_12' a12 = [
	_11 (a12 `withSuffix` "volō"),
	_12 (a12 `withSuffix` "vīs"),
	_13 (a12 `withSuffix` "vult"),
	_14 (a12 `withSuffix` "volumus"),
	_15 (a12 `withSuffix` "vultis"),
	_16 (a12 `withSuffix` "volunt"),
	_21 (a12 `withSuffix` "volam"),
	_22 (a12 `withSuffix` "volēs"),
	_23 (a12 `withSuffix` "volet"),
	_24 (a12 `withSuffix` "volēmus"),
	_25 (a12 `withSuffix` "volētis"),
	_26 (a12 `withSuffix` "volent"),
	_31 (a12 `withSuffix` "volēbam"),
	_32 (a12 `withSuffix` "volēbās"),
	_33 (a12 `withSuffix` "volēbat"),
	_34 (a12 `withSuffix` "volēbāmus"),
	_35 (a12 `withSuffix` "volēbātis"),
	_36 (a12 `withSuffix` "volēbant"),
	_41 (a12 `withSuffix` "voluī"),
	_42 (a12 `withSuffix` "voluistī"),
	_43 (a12 `withSuffix` "voluit"),
	_44 (a12 `withSuffix` "voluimus"),
	_45 (a12 `withSuffix` "voluistis"),
	_46 (a12 `withSuffix` "voluērunt"),
	_51 (a12 `withSuffix` "voluerō"),
	_52 (a12 `withSuffix` "volueris"),
	_53 (a12 `withSuffix` "voluerit"),
	_54 (a12 `withSuffix` "voluerimus"),
	_55 (a12 `withSuffix` "volueritis"),
	_56 (a12 `withSuffix` "voluerint"),
	_61 (a12 `withSuffix` "volueram"),
	_62 (a12 `withSuffix` "voluerās"),
	_63 (a12 `withSuffix` "voluerat"),
	_64 (a12 `withSuffix` "voluerāmus"),
	_65 (a12 `withSuffix` "voluerātis"),
	_66 (a12 `withSuffix` "voluerant"),
	_101 (a12 `withSuffix` "velim"),
	_102 (a12 `withSuffix` "velīs"),
	_103 (a12 `withSuffix` "velit"),
	_104 (a12 `withSuffix` "velīmus"),
	_105 (a12 `withSuffix` "velītis"),
	_106 (a12 `withSuffix` "velint"),
	_111 (a12 `withSuffix` "vellem"),
	_112 (a12 `withSuffix` "vellēs"),
	_113 (a12 `withSuffix` "vellet"),
	_114 (a12 `withSuffix` "vellēmus"),
	_115 (a12 `withSuffix` "vellētis"),
	_116 (a12 `withSuffix` "vellent"),
	_121 (a12 `withSuffix` "voluerim"),
	_122 (a12 `withSuffix` "voluerīs"),
	_123 (a12 `withSuffix` "voluerit"),
	_124 (a12 `withSuffix` "voluerīmus"),
	_125 (a12 `withSuffix` "voluerītis"),
	_126 (a12 `withSuffix` "voluerint"),
	_131 (a12 `withSuffix` "voluissem"),
	_132 (a12 `withSuffix` "voluissēs"),
	_133 (a12 `withSuffix` "voluisset"),
	_134 (a12 `withSuffix` "voluissēmus"),
	_135 (a12 `withSuffix` "voluissētis"),
	_136 (a12 `withSuffix` "voluissent"),
	_181 (a12 `withSuffix` "velle"),
	_182 (a12 `withSuffix` "voluisse")]
	++ (act_pres_participle (a12 `withSuffix` "vole"))

la_conj_irr_eo :: [T.Text] -> Maybe [VerbForm]
la_conj_irr_eo [] = Just $ filter3pPassiveForms $ la_conj_irr_eo' T.empty
la_conj_irr_eo (arg:_) = Just $ la_conj_irr_eo' arg

la_conj_irr_eo' :: T.Text -> [VerbForm]
la_conj_irr_eo' pre = [
	_11 (pre `withSuffix` "eō"),
	_12 (pre `withSuffix` "īs"),
	_13 (pre `withSuffix` "it"),
	_14 (pre `withSuffix` "īmus"),
	_15 (pre `withSuffix` "ītis"),
	_16 (pre `withSuffix` "eunt"),
	_21 (pre `withSuffix` "ībō"),
	_22 (pre `withSuffix` "ībis"),
	_23 (pre `withSuffix` "ībit"),
	_24 (pre `withSuffix` "ībimus"),
	_25 (pre `withSuffix` "ībitis"),
	_26 (pre `withSuffix` "ībunt"),
	_31 (pre `withSuffix` "ībam"),
	_32 (pre `withSuffix` "ībās"),
	_33 (pre `withSuffix` "ībat"),
	_34 (pre `withSuffix` "ībāmus"),
	_35 (pre `withSuffix` "ībātis"),
	_36 (pre `withSuffix` "ībant"),
	_41 (pre `withSuffix` "iī"),
	_42 (pre `withSuffix` "istī"),
	_43 (pre `withSuffix` "iit"),
	_44 (pre `withSuffix` "iimus"),
	_45 (pre `withSuffix` "istis"),
	_46 (pre `withSuffix` "iērunt"),
	_51 (pre `withSuffix` "ierō"),
	_52 (pre `withSuffix` "ieris"),
	_53 (pre `withSuffix` "ierit"),
	_54 (pre `withSuffix` "ierimus"),
	_55 (pre `withSuffix` "ieritis"),
	_56 (pre `withSuffix` "ierint"),
	_61 (pre `withSuffix` "ieram"),
	_62 (pre `withSuffix` "ierās"),
	_63 (pre `withSuffix` "ierat"),
	_64 (pre `withSuffix` "ierāmus"),
	_65 (pre `withSuffix` "ierātis"),
	_66 (pre `withSuffix` "ierant"),
	_71 (pre `withSuffix` "eor"),
	_72 (pre `withSuffix` "īris"),
	_73 (pre `withSuffix` "ītur"),
	_74 (pre `withSuffix` "īmur"),
	_75 (pre `withSuffix` "īminī"),
	_76 (pre `withSuffix` "euntur"),
	_81 (pre `withSuffix` "ībor"),
	_82 (pre `withSuffix` "īberis"),
	_83 (pre `withSuffix` "ībitur"),
	_84 (pre `withSuffix` "ībimur"),
	_85 (pre `withSuffix` "ībiminī"),
	_86 (pre `withSuffix` "ībuntur"),
	_91 (pre `withSuffix` "ībar"),
	_92 (pre `withSuffix` "ībāris"),
	_93 (pre `withSuffix` "ībātur"),
	_94 (pre `withSuffix` "ībāmur"),
	_95 (pre `withSuffix` "ībāminī"),
	_96 (pre `withSuffix` "ībantur"),
	_101 (pre `withSuffix` "eam"),
	_102 (pre `withSuffix` "eās"),
	_103 (pre `withSuffix` "eat"),
	_104 (pre `withSuffix` "eāmus"),
	_105 (pre `withSuffix` "eātis"),
	_106 (pre `withSuffix` "eant"),
	_111 (pre `withSuffix` "īrem"),
	_112 (pre `withSuffix` "īrēs"),
	_113 (pre `withSuffix` "īret"),
	_114 (pre `withSuffix` "īrēmus"),
	_115 (pre `withSuffix` "īrētis"),
	_116 (pre `withSuffix` "īrent"),
	_121 (pre `withSuffix` "ierim"),
	_122 (pre `withSuffix` "ierīs"),
	_123 (pre `withSuffix` "ierit"),
	_124 (pre `withSuffix` "ierīmus"),
	_125 (pre `withSuffix` "ierītis"),
	_126 (pre `withSuffix` "ierint"),
	_131 (pre `withSuffix` "issem"),
	_132 (pre `withSuffix` "issēs"),
	_133 (pre `withSuffix` "isset"),
	_134 (pre `withSuffix` "issēmus"),
	_135 (pre `withSuffix` "issētis"),
	_136 (pre `withSuffix` "issent"),
	_141 (pre `withSuffix` "ear"),
	_142 (pre `withSuffix` "eāris"),
	_143 (pre `withSuffix` "eātur"),
	_144 (pre `withSuffix` "eāmur"),
	_145 (pre `withSuffix` "eāminī"),
	_146 (pre `withSuffix` "eantur"),
	_151 (pre `withSuffix` "īrer"),
	_152 (pre `withSuffix` "īrēris"),
	_153 (pre `withSuffix` "īrētur"),
	_154 (pre `withSuffix` "īrēmur"),
	_155 (pre `withSuffix` "īrēminī"),
	_156 (pre `withSuffix` "īrentur"),
	_161 (pre `withSuffix` "ī"),
	_171 (pre `withSuffix` "īte"),
	_181 (pre `withSuffix` "īre"),
	_182 (pre `withSuffix` "isse"),
	_183 (pre `withSuffix` "itūrus esse"),
	_184 (pre `withSuffix` "īrī"),
	_185 (pre `withSuffix` "itus esse"),
	_186 (pre `withSuffix` "itum īrī")]
	++ (act_pres_participle (pre `withSuffix` "iē"))
	++ (act_fut_participle (pre `withSuffix` "it"))
	++ (pass_perf_participle (pre `withSuffix` "it"))
	++ (pass_fut_participle (pre `withSuffix` "eu"))
	++ (passive_past_forms (pre `withSuffix` "itus"))

la_conj_irr_sum :: [T.Text] -> Maybe [VerbForm]
la_conj_irr_sum [] = Just $ la_conj_irr_sum' T.empty
la_conj_irr_sum (arg:_) = Just $ la_conj_irr_sum' arg

la_conj_irr_sum' :: T.Text -> [VerbForm]
la_conj_irr_sum' pre = [
	_11 (pre `withSuffix` "sum"),
	_12 (pre `withSuffix` "es"),
	_13 (pre `withSuffix` "est"),
	_14 (pre `withSuffix` "sumus"),
	_15 (pre `withSuffix` "estis"),
	_16 (pre `withSuffix` "sunt"),
	_21 (pre `withSuffix` "erō"),
	_22 (pre `withSuffix` "eris"),
	_23 (pre `withSuffix` "erit"),
	_24 (pre `withSuffix` "erimus"),
	_25 (pre `withSuffix` "eritis"),
	_26 (pre `withSuffix` "erunt"),
	_31 (pre `withSuffix` "eram"),
	_32 (pre `withSuffix` "erās"),
	_33 (pre `withSuffix` "erat"),
	_34 (pre `withSuffix` "erāmus"),
	_35 (pre `withSuffix` "erātis"),
	_36 (pre `withSuffix` "erant"),
	_41 (pre `withSuffix` "fuī"),
	_42 (pre `withSuffix` "fuistī"),
	_43 (pre `withSuffix` "fuit"),
	_44 (pre `withSuffix` "fuimus"),
	_45 (pre `withSuffix` "fuistis"),
	_46 (pre `withSuffix` "fuērunt"),
	_51 (pre `withSuffix` "fuerō"),
	_52 (pre `withSuffix` "fueris"),
	_53 (pre `withSuffix` "fuerit"),
	_54 (pre `withSuffix` "fuerimus"),
	_55 (pre `withSuffix` "fueritis"),
	_56 (pre `withSuffix` "fuerint"),
	_61 (pre `withSuffix` "fueram"),
	_62 (pre `withSuffix` "fuerās"),
	_63 (pre `withSuffix` "fuerat"),
	_64 (pre `withSuffix` "fuerāmus"),
	_65 (pre `withSuffix` "fuerātis"),
	_66 (pre `withSuffix` "fuerant"),
	_101 (pre `withSuffix` "sim"),
	_102 (pre `withSuffix` "sīs"),
	_103 (pre `withSuffix` "sit"),
	_104 (pre `withSuffix` "sīmus"),
	_105 (pre `withSuffix` "sītis"),
	_106 (pre `withSuffix` "sint"),
	_111 (pre `withSuffix` "essem"),
	_112 (pre `withSuffix` "essēs"),
	_113 (pre `withSuffix` "esset"),
	_114 (pre `withSuffix` "essēmus"),
	_115 (pre `withSuffix` "essētis"),
	_116 (pre `withSuffix` "essent"),
	_121 (pre `withSuffix` "fuerim"),
	_122 (pre `withSuffix` "fuerīs"),
	_123 (pre `withSuffix` "fuerit"),
	_124 (pre `withSuffix` "fuerīmus"),
	_125 (pre `withSuffix` "fuerītis"),
	_126 (pre `withSuffix` "fuerint"),
	_131 (pre `withSuffix` "fuissem"),
	_132 (pre `withSuffix` "fuissēs"),
	_133 (pre `withSuffix` "fuisset"),
	_134 (pre `withSuffix` "fuissēmus"),
	_135 (pre `withSuffix` "fuissētis"),
	_136 (pre `withSuffix` "fuissent"),
	_161 (pre `withSuffix` "es"),
	_171 (pre `withSuffix` "este"),
	_181 (pre `withSuffix` "esse"),
	_182 (pre `withSuffix` "fuisse"),
	_183 (pre `withSuffix` "fore")]
	++ (act_fut_participle (pre `withSuffix` "fut"))
