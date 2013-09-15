#!/bin/bash

cat aeneid.txt \
| tr '[:upper:]' '[:lower:]' \
| tr -cs 'a-zA-Z0-9' '[\n*]' \
| sed '/^$/d' \
| sort \
| uniq -c \
| sort -n \
> aeneid_counts.txt
