#!/bin/bash

rm _main.Rmd; time Rscript -e "bookdown::render_book('index.Rmd')"
