#!/bin/bash

# Delete all generated javascript programs
rm -f programs/**/*.js

# Delete build artefacts
cd src
make clean
