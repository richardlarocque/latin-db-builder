module Latin.DatabaseWriter where

import qualified Data.Text as T
import Database.HDBC

import Latin.Types
import Latin.PartsOfSpeech
import Latin.Grammar

import Util.Normalize

import Debug.Trace

data Inserters = Inserters {
	word :: Statement,
	def :: Statement,
        form :: Statement
}

createTables :: IConnection conn => conn -> IO ()
createTables conn = do
	runRaw conn $ "CREATE TABLE parts_of_speech ("
			++ "id INTEGER PRIMARY KEY,"
			++ "name TEXT);"
	runRaw conn $ "CREATE TABLE words ("
			++ "id INTEGER PRIMARY KEY,"
			++ "part_id REFERENCES parts_of_speech(ID),"
			++ "lemma TEXT,"
			++ "header TEXT);"
	runRaw conn $ "CREATE TABLE definitions ("
			++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
			++ "word_id REFERENCES words(ID),"
			++ "definition TEXT);"
        runRaw conn $ "CREATE TABLE word_form_names ("
                        ++ "part_id REFERENCES part_of_speech(ID),"
                        ++ "form_id INTEGER,"
                        ++ "name TEXT,"
                        ++ "PRIMARY KEY(part_id, form_id));"
	runRaw conn $ "CREATE TABLE word_forms ("
			++ "word_id INTEGER REFERENCES words(ID),"
			++ "part_id INTEGER,"
			++ "form_id INTEGER,"
			++ "value TEXT,"
                        ++ "norm_value TEXT,"
                        ++ "FOREIGN KEY(part_id, form_id) REFERENCES word_form_names(part_id, form_id),"
			++ "PRIMARY KEY (word_id,part_id,form_id));"

createIndices :: IConnection conn => conn -> IO ()
createIndices conn = do
	runRaw conn $ "CREATE INDEX word_form_index ON word_forms(norm_value);"

partID :: PartOfSpeech -> SqlValue
partID = toSql.fromEnum

inflectionID :: Case -> Number -> SqlValue
inflectionID c n = toSql $ (100 * (fromEnum n)) + (10 * (fromEnum c))

genderedInflectionID :: Case -> Number -> Gender -> SqlValue
genderedInflectionID c n g = toSql $ 1000000 + (100 * (fromEnum n)) + (10 * (fromEnum c)) + fromEnum g

conjVerbID :: Mood -> Voice -> Tense -> Number -> Person -> SqlValue
conjVerbID m v t n p = toSql $
	(10000 * (fromEnum m)) +
	(1000 * (fromEnum v)) +
	(100 * (fromEnum t)) +
	(10 * (fromEnum n)) +
	(fromEnum p)

impVerbID :: Number -> SqlValue
impVerbID n = toSql $
	50000 +
	(10 * (fromEnum n))

infVerbID :: Voice -> Tense -> SqlValue
infVerbID v t = toSql $
	60000 +
	(1000 * (fromEnum v)) +
	(100 * (fromEnum t))

partVerbID :: Voice -> Tense -> Case -> Number -> Gender -> SqlValue
partVerbID v t c n g = toSql $
	700000 +
	(10000 * (fromEnum v)) +
	(1000 * (fromEnum t)) +
        (100 * (fromEnum c)) +
        (10 * (fromEnum n)) +
        (fromEnum g)

verbConjID :: VerbForm -> SqlValue
verbConjID v = case v of
	Conjugated m v t n p _ -> conjVerbID m v t n p
	Imperative _ _ n _ -> impVerbID n
	Infinitive v t _ -> infVerbID v t
	Participle v t c n g _ -> partVerbID v t c n g

adverbFormID :: AdverbForm -> SqlValue
adverbFormID a = case a of
	Normal _ -> toSql (1 :: Int)
	Comparative _ -> toSql (2 :: Int)
	Superlative _ -> toSql (3 :: Int)

null_part_id :: Int
null_part_id = 2^29-1

populateTables :: IConnection conn => conn -> IO ()
populateTables conn = do
	populatePosTable conn
	populateNounForms conn
	populateAdjectiveForms conn
	populateVerbForms conn
	populateAdverbForms conn
	populatePronounForms conn
        populateRemainingForms conn

populatePosTable :: IConnection conn => conn -> IO ()
populatePosTable conn = do
	stmt <- prepare conn "INSERT INTO parts_of_speech (id,name) VALUES (?,?)"
	executeMany stmt [ [partID p, partName p] | p <- latin_parts_of_speech ]
	where
		partName = toSql.show

populateNounForms :: IConnection conn => conn -> IO ()
populateNounForms conn = do
	stmt <- prepare conn "INSERT INTO word_form_names (part_id,form_id,name) VALUES (?,?,?)"
	executeMany stmt [ [ partID Noun, inflectionID c n , prettyName c n ] | c <- cases, n <- numbers ]
	where
		cases = [ Nominative, Genitive, Dative, Accusative, Ablative, Vocative, Locative ]
		numbers = [ Singular, Plural ]
		prettyName c n = toSql $ (show c) ++ " " ++ (show n)

populateAdjectiveForms :: IConnection conn => conn -> IO ()
populateAdjectiveForms conn = do
	stmt <- prepare conn "INSERT INTO word_form_names (part_id,form_id,name) VALUES (?,?,?)"
	executeMany stmt [ [ partID Adjective, genderedInflectionID c n g , prettyName c n g ] | c <- cases, n <- numbers, g <- genders ]
	where
		cases = [ Nominative, Genitive, Dative, Accusative, Ablative, Vocative, Locative ]
		numbers = [ Singular, Plural ]
		genders = [ Masculine, Feminine, Neuter ]
		prettyName c n g = toSql $ (show c) ++ " " ++ (show n) ++ " " ++ (show g)

populateAdverbForms :: IConnection conn => conn -> IO ()
populateAdverbForms conn = do
	stmt <- prepare conn "INSERT INTO word_form_names (part_id,form_id,name) VALUES (?,?,?)"
	executeMany stmt [ [ partID Adverb, toSql x , prettyName x ] | x <- [ 1..3 ] ]
	where
		prettyName :: Int -> SqlValue
		prettyName 1 = toSql $ "Positive"
		prettyName 2 = toSql $ "Comparative"
		prettyName 3 = toSql $ "Superlative"

populatePronounForms :: IConnection conn => conn -> IO ()
populatePronounForms conn = do
	stmt <- prepare conn "INSERT INTO word_form_names (part_id,form_id,name) VALUES (?,?,?)"
	executeMany stmt [ [ partID Pronoun, genderedInflectionID c n g , prettyName c n g ] | c <- cases, n <- numbers, g <- genders ]
	executeMany stmt [ [ partID Pronoun, inflectionID c n , prettyNameNG c n ] | c <- cases, n <- numbers ]
	where
		cases = [ Nominative, Genitive, Dative, Accusative, Ablative ]
		numbers = [ Singular, Plural ]
		genders = [ Masculine, Feminine, Neuter ]
		prettyName c n g = toSql $ (show c) ++ " " ++ (show n) ++ " " ++ (show g)
		prettyNameNG c n = toSql $ (show c) ++ " " ++ (show n)

populateRemainingForms :: IConnection conn => conn -> IO ()
populateRemainingForms conn = do
	stmt <- prepare conn "INSERT INTO word_form_names (part_id,form_id,name) VALUES (?,?,?)"
	executeMany stmt [ [ partID part, toSql null_part_id, toSql "" ] | part <- latin_parts_of_speech ]

-- TODO: Don't populate impossible combinations.
populateVerbForms :: IConnection conn => conn -> IO ()
populateVerbForms conn = do
	stmt <- prepare conn "INSERT INTO word_form_names (part_id,form_id,name) VALUES (?,?,?)"
	executeMany stmt [ [ partID Verb, conjVerbID Indicative v t n p, prettyConj Indicative v t n p ] | v <- voices, t <- ind_tenses, n <- numbers, p <- persons ]
	executeMany stmt [ [ partID Verb, conjVerbID Subjunctive v t n p, prettyConj Subjunctive v t n p ] | v <- voices, t <- subj_tenses, n <- numbers, p <- persons ]
	executeMany stmt [ [ partID Verb, impVerbID n, prettyImp n ] | n <- numbers ]
	executeMany stmt [ [ partID Verb, infVerbID v t, prettyInf v t ] | v <- voices, t <- inf_tenses ]
	executeMany stmt [ [ partID Verb, partVerbID v t c n g, prettyParticiple v t c n g ] | ((v,t),(c,n,g)) <- participles ]
	where
		moods = [ Indicative, Subjunctive ]
		voices = [ Active, Passive ]
		ind_tenses = [ Present, Imperfect, Future, Perfect, Pluperfect, FuturePerfect ]
		subj_tenses = [ Present, Imperfect, Perfect, Pluperfect ]
		inf_tenses = [ Present, Perfect, Future ]
		persons = [ First, Second, Third ]
		participles = [ (pt, pcng) | pt <- part_tenses, pcng <- part_cng ]
		-- (Active, Perfect) is a way of dealing with deponent verbs.
		part_tenses = [ (Active,Present), (Active,Perfect), (Active,Future), (Passive,Perfect), (Passive,Future) ]
		part_cng = [ (c,n,g) | c <- cases, n <- numbers, g <- genders ]
		cases = [ Nominative, Genitive, Dative, Accusative, Ablative ]
		numbers = [ Singular, Plural ]
		genders = [ Masculine, Feminine, Neuter ]
		prettyConj m v t n p = toSql $ (show m) ++ " " ++  (show v) ++ " " ++  (show t) ++ " " ++  (show n) ++ " " ++ (show p)
		prettyImp n = toSql $ "Imperative " ++ (show n)
		prettyInf v t = toSql $ (show v) ++ " " ++ (show t) ++ " Infinitive"
		prettyParticiple v t c n g =
			toSql $ (show c) ++ " " ++  (show n) ++ " " ++  (show g) ++ " "
				++  (show v) ++ " " ++ (show t) ++ " Participle"

insertEntries :: IConnection conn => conn -> [LatinEntry] -> IO ()
insertEntries conn entries = do
	word_inserter <- prepare conn "INSERT INTO words(id,part_id,lemma,header) VALUES (?,?,?,?)"
	def_inserter <- prepare conn "INSERT INTO definitions(word_id,definition) VALUES (?,?)"
	form_inserter <- prepare conn "INSERT INTO word_forms(word_id,part_id,form_id,value,norm_value) VALUES (?,?,?,?,?)"
	let insert_stmts = Inserters word_inserter def_inserter form_inserter
	let inserter = insertEntry insert_stmts
	sequence_ [ inserter id entry | (id,entry) <- zip [1..] entries ]

insertEntry :: Inserters -> Integer -> LatinEntry -> IO ()
insertEntry inserters word_id entry = do
	let p = getPart entry
	let l = getLemma entry
	let h = getHeader entry
	let defs = getDefs entry
	let word_ins = word inserters
	let def_ins = def inserters
	_ <- execute word_ins [ toSql word_id, partID p, toSql l, toSql h ]
	_ <- executeMany def_ins [ [toSql word_id, toSql def] | def <- defs ]
	case entry of
		(NounEntry _ _ _ inflections) -> insertGenderlessInflections inserters word_id Noun inflections
		(AdjectiveEntry _ _ _ inflections) -> insertGenderedInflections inserters word_id Adjective inflections
		(DeterminerEntry _ _ _ inflections) -> insertGenderedInflections inserters word_id Determiner inflections
		(VerbEntry _ _ _ conjugations) -> insertVerbConjugations inserters word_id conjugations
		(AdverbEntry _ _ _ forms) -> insertAdverbForms inserters word_id forms
		(PronounEntry _ _ _ forms) -> insertGenderlessInflections inserters word_id Pronoun forms
		(PronounEntryG _ _ _ forms) -> insertGenderedInflections inserters word_id Pronoun forms
		e -> insertFormFromHeadword inserters word_id e
	return ()

insertGenderlessInflections :: Inserters -> Integer -> PartOfSpeech -> [Inflection] -> IO ()
insertGenderlessInflections inserters word_id part inflections =
	let noun_infl_ins = form inserters in
	executeMany noun_infl_ins $
	[ [ toSql word_id, (toSql.partID) part, inflectionID c n, toSql inflected, (toSql.toNormalForm) inflected ]
			| Inflection c n inflected <- inflections ]

insertGenderedInflections :: Inserters -> Integer -> PartOfSpeech -> [GenderedInflection] -> IO ()
insertGenderedInflections inserters word_id part inflections =
	let det_infl_ins = form inserters in
	executeMany det_infl_ins $
	[ [ toSql word_id, (toSql.partID) part, genderedInflectionID c n g, toSql inflected, (toSql.toNormalForm) inflected ]
			| GenderedInflection c n g inflected <- inflections ]

insertVerbConjugations :: Inserters -> Integer -> [VerbForm] -> IO ()
insertVerbConjugations inserters word_id conjugations =
	let verb_conj_ins = form inserters in
	executeMany verb_conj_ins $
	[ [ toSql word_id, (toSql.partID) Verb, verbConjID c, (toSql.getValue) c, (toSql.toNormalForm.getValue) c ] | c <- conjugations ]
	where
		getValue (Conjugated _ _ _ _ _ v) = v
		getValue (Imperative _ _ _ v) = v
		getValue (Infinitive _ _ v) = v
		getValue (Participle _ _ _ _ _ v) = v

insertAdverbForms :: Inserters -> Integer -> [AdverbForm] -> IO ()
insertAdverbForms inserters word_id conjugations =
	let adverb_form_ins = form inserters in
	executeMany adverb_form_ins $
	[ [ toSql word_id, (toSql.partID) Adverb, adverbFormID a, (toSql.getValue) a, (toSql.toNormalForm.getValue) a ] | a <- conjugations ]
	where
		getValue (Normal f) = f
		getValue (Comparative f) = f
		getValue (Superlative f) = f

insertFormFromHeadword :: Inserters -> Integer -> LatinEntry -> IO ()
insertFormFromHeadword inserters word_id e =
	let form_ins = form inserters in
	(execute form_ins $
		[ toSql word_id, (partID.getPart) e, toSql null_part_id, (toSql.getHeader) e, (toSql.toNormalForm.getHeader) e ]) >> return ()
