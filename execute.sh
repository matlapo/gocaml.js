#!/bin/bash

outfile=$(echo "$1" | sed -e 's/\.go/\.js/g')
node "$outfile"
