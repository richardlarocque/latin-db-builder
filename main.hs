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
