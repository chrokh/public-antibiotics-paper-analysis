library(dplyr)
library(ggplot2)
library(gridExtra)

# Config
INPUT  <- 'output/data/years.csv'
OUTPUT <- 'output/plots/interventions.pdf'


# I/O
phase_years <- read.csv(INPUT)
pdf(OUTPUT)


# Convert phases to ordered factor
phase_levels <- c('PC','P1','P2','P3','P4','MP')
phase_years$phase <- factor(phase_years$phase, levels=phase_levels, ordered=TRUE)


as_tibble(phase_years)
