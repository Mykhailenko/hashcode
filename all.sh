#!/usr/bin/env bash

gradle clean build

for filepath in ./dataset/*; do
    filename=`basename "$filepath"`

    ./run.sh --in $filepath --out ./best/$filename $@

done

