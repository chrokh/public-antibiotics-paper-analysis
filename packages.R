r = getOption('repos')
r['CRAN'] = 'http://cran.us.r-project.org'
options(repos = r)

install.packages('tidyr')
install.packages('ggplot2')
install.packages('broom')
install.packages('forcats')
install.packages('gridExtra')
install.packages('triangle')
install.packages('ggpubr')
install.packages('remotes')
remotes::install_github('rstudio/rmarkdown')
install.packages('bookdown')
install.packages('viridis')

# For better nls (nlsLM)
# See: https://rmazing.wordpress.com/2012/07/05/a-better-nls/
# See: https://www.rdocumentation.org/packages/minpack.lm/versions/1.2-1/topics/nlsLM
install.packages('minpack.lm')

# Solves:
#   Error in library(p, character.only = TRUE) :
#     there is no package called 'bindrcpp'
#   Calls: render ... suppressPackageStartupMessages -> withCallingHandlers -> library
# But I'm not sure why that's a problem.
install.packages('bindrcpp')
