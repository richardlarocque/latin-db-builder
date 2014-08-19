#!/bin/bash

set -ex

#DB=sample.sqlite3
#INPUT=sample.xml

DB=latin-db.sqlite3
INPUT=enwiktionary-20140728-pages-articles.xml
#INPUT=enwiktionary-20131002-pages-articles.xml
#INPUT=enwiktionary-20130814-pages-articles.xml

rm $DB || true
cabal build
./dist/build/latin-db/latin-db $INPUT $DB 1000000
sqlite3 $DB << EOF
--.tables
--select * from parts_of_speech;
select * from words;
--select * from definitions;
--select * from word_form_names;
--select * from word_forms;
EOF

./process_aeneid.sh
./calculate_word_popularity.py
