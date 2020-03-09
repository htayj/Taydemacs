#!/usr/bin/env bash

echo "starting signald"
rm signalfull.log
touch signalfull.log
zsh -c 'signald | tee signalfull.log' &
inotifywait -m -e modify signalfull.log |
    while read path _ file; do
        if grep -F --quiet "Null" "signalfull.log"
        then
            echo "defunct"
            exit 1
            kill $$
        fi
    done
