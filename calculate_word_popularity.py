#!/usr/bin/env python3

import sqlite3
import re

def main():
    conn = sqlite3.connect('latin-db.sqlite3')

    cur = conn.cursor()
    cur.execute("SELECT id FROM words;")
    word_ids = cur.fetchall()

    count_map = {}
    for id in word_ids:
        count_map[id[0]] = 0

    f = open('aeneid_counts.txt', 'r')
    regex = re.compile(r"\s*(\d+)\s*(\w+)");
    for line in f:
        match = regex.match(line)
        if not match:
            next
        count = int(match.group(1))
        word = match.group(2)

        cur = conn.cursor()
        cur.execute(
                "SELECT DISTINCT word_id FROM word_forms WHERE norm_value = ?;",
                (word,))
        word_ids = cur.fetchall();
        print ((word, len(word_ids)))
        for id in word_ids:
            count_map[id[0]] += count

    cur.execute("DROP TABLE IF EXISTS word_popularity");
    cur.execute("CREATE TABLE word_popularity(word_id INTEGER REFERENCES word(ID), rank INTEGER)");
    id_count_list = count_map.items()
    sorted_id_count_list = reversed(sorted(id_count_list, key=lambda x: x[1]))

    ranked_ids = []
    current_count = 10000000
    rank = 0
    for (i, (id,count)) in enumerate(sorted_id_count_list):
        if (count < current_count):
            current_count = count
            rank = i+1
        ranked_ids.append((id,rank))

    conn.executemany("INSERT INTO word_popularity(word_id, rank) VALUES (?, ?)",
                     ranked_ids)
    conn.commit()

main()
