module Latin.DatabaseWriter where

import qualified Data.Text as T
import Database.HDBC

import Latin.PartsOfSpeech

createTables :: IConnection conn => conn -> IO ()
createTables conn = do
	runRaw conn $ "CREATE TABLE parts_of_speech ("
			++ "id PRIMARY KEY,"
			++ "name TEXT);"
	runRaw conn $ "CREATE TABLE words ("
			++ "id PRIMARY KEY,"
			++ "part_id REFERENCES parts_of_speech(ID),"
			++ "lemma TXT);"

partID :: PartOfSpeech -> SqlValue
partID = toSql.fromEnum

populateTables :: IConnection conn => conn -> IO ()
populateTables conn = do
	stmt <- prepare conn "INSERT INTO parts_of_speech (id,name) VALUES (?,?)"
	executeMany stmt [ [partID p, partName p] | p <- latin_parts_of_speech ]
	where
		partName = toSql.show

insertBasicWord :: IConnection conn => conn -> IO ()
insertBasicWord conn = do
	stmt <- prepare conn "INSERT INTO words(id,part_id,lemma) VALUES (?,?,?)"
	_ <- execute stmt $ basicWordInsertArgs (T.pack "cogito")  Verb
	return ()

basicWordInsertArgs :: T.Text -> PartOfSpeech -> [SqlValue]
basicWordInsertArgs lemma part = [ iToSql 1, partID part, toSql lemma ]
