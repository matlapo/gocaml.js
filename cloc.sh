#!/bin/bash
# Counts the number of lines of code in the project
# Requires the cloc tool: http://cloc.sourceforge.net/

cloc --exclude-dir=_build,past-teams .
