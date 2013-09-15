module Main where

import System.Environment

import Database.HDBC
import Database.HDBC.Sqlite3

import Wiki.DumpReader
import Wiki.Latin.PageParser

import Latin.Types
import Latin.DatabaseWriter

main = do
	args <- getArgs

	let db_name = (args !! 1)
	conn <- connectSqlite3 db_name
	createTables conn
	populateTables conn
	commit conn

	let input_file = (args !! 0)
	pages <- getPages input_file
	let page_entries = concatMap extractLatinEntries pages :: [LatinEntry]
	let static_entries = getStaticLatinEntries
	let entries = static_entries ++ page_entries

	let limit = read (args !! 2)
	let some_entries = take limit entries
	insertEntries conn $ some_entries

	createIndices conn
	commit conn
	disconnect conn
