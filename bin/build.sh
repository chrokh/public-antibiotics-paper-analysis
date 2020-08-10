#!/bin/bash

osascript -e 'display notification "Started" with title "Analysis"'
rm _main.Rmd; time Rscript -e "bookdown::render_book('index.Rmd')"
osascript -e 'display notification "Finished" with title "Analysis"'
