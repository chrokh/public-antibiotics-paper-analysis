#!/bin/bash

# Runs all plot scripts with the supplied directory as the R working directory.
#
# USAGE:
# plot.sh <dir>
#   where <dir> will be used as the R working directory.
#

if [ "$#" -ne 1 ]; then
  echo "Please supply working directory as argument."
  exit 1
fi

REL_SCRIPT_PATH="`dirname \"$0\"`"
ABS_SCRIPT_PATH="`( cd \"$REL_SCRIPT_PATH\" && pwd )`"
RSCRIPTS="$ABS_SCRIPT_PATH/../src/plot"
WORKING_DIR_PATH=$1

# Exit if any of the following commands fail
set -e

# Change to working directory
cd "$WORKING_DIR_PATH"

# Execute all scripts
for f in `find $RSCRIPTS -name '*.R'`; do
  echo "Working directory"
  echo "  `pwd`"
  echo
  echo "Executing R script:"
  echo "  $f"
  echo
  Rscript $f
done
