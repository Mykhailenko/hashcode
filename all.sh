#!/usr/bin/env bash

gradle clean build

for filename in ./dataset/*; do

    ./run.sh --in $filename  $@

done

