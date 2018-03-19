#!/bin/bash

outfile=$(echo "$1" | cut -d. -f1)
node "$outfile.js"
