#!/bin/bash
# Counts the number of lines of code in the project

cloc --exclude-dir=_build,past-teams .
