module Main where

import System.Environment

import Database.HDBC
import Database.HDBC.Sqlite3

import Wiki.DumpReader
import Wiki.Latin.PageParser

import Latin.DatabaseWriter

main = do
	conn <- connectSqlite3 "latindb.sqlite3"
	createTables conn
	populateTables conn
	insertBasicWord conn
	commit conn
	disconnect conn

	args <- getArgs

	pages <- getPages (args !! 0)
	let entries = concatMap extractLatinEntries pages
	mapM_ print entries
