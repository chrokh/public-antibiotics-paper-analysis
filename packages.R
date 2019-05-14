r = getOption('repos')
r['CRAN'] = 'http://cran.us.r-project.org'
options(repos = r)

install.packages('tidyr')
install.packages('ggplot2')
install.packages('gridExtra')
install.packages('triangle')
install.packages('ggpubr')
install.packages('remotes')
remotes::install_github('rstudio/rmarkdown')

install.packages('bindrcpp')
# Solves:
#   Error in library(p, character.only = TRUE) :
#     there is no package called 'bindrcpp'
#   Calls: render ... suppressPackageStartupMessages -> withCallingHandlers -> library
# But I'm not sure why that's a problem.
