#!/bin/bash

set -ex

rm latindb.sqlite3 || true
cabal build
./dist/build/latin-db/latin-db enwiktionary-20130814-pages-articles.xml
sqlite3 latindb.sqlite3 << EOF
.tables
select * from parts_of_speech;
select * from words;
EOF
