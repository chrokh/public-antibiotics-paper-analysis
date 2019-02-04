# Config
INPUT  <- 'output/data/phases.csv'
OUTPUT <- 'output/data/treated_phases.csv'

# I/O
phases <- read.csv(INPUT)
N      <- nrow(phases)

# Convert phases to ordered factor
phase_levels <- c('PC','P1','P2','P3','P4','MP')
phases$phase <- factor(phases$phase, levels=phase_levels, ordered=TRUE)

# Possible interventions
intervention_levels <- c('NONE', 'PCER', 'P1ER', 'P2ER', 'P3ER', 'P4ER', 'PDMER', 'FDMER')

# Set default intervention
phases$intervention <- factor('NONE', levels=intervention_levels)

# Log sampling function
log10_sample <- function (min, max, magnitude_min, magnitude_max) {
  runif(N, min, max) * (10 ^ runif(N, magnitude_min, magnitude_max))
}

# Intervention: PCER
PCER <- data.frame(phases)
PCER$intervention <- 'PCER'
PCER$revenue   <- ifelse(PCER$phase=='PC', PCER$revenue + log10_sample(1, 9, 0, 3), PCER$revenue)

# Intervention: P1ER
P1ER <- data.frame(phases)
P1ER$intervention <- 'P1ER'
P1ER$revenue   <- ifelse(P1ER$phase=='P1', P1ER$revenue + log10_sample(1, 9, 0, 3), P1ER$revenue)

# Intervention: P2ER
P2ER <- data.frame(phases)
P2ER$intervention <- 'P2ER'
P2ER$revenue   <- ifelse(P2ER$phase=='P2', P2ER$revenue + log10_sample(1, 9, 0, 3), P2ER$revenue)

# Intervention: P3ER
P3ER <- data.frame(phases)
P3ER$intervention <- 'P3ER'
P3ER$revenue   <- ifelse(P3ER$phase=='P3', P3ER$revenue + log10_sample(1, 9, 0, 3), P3ER$revenue)

# Intervention: P4ER
P4ER <- data.frame(phases)
P4ER$intervention <- 'P4ER'
P4ER$revenue   <- ifelse(P4ER$phase=='P4', P4ER$revenue + log10_sample(1, 9, 0, 3), P4ER$revenue)

# Intervention: FDMER
# TODO: This will not work properly later since the MER will be spread over
# market years in interpolation step.
# NOTE: Replaces old revenue.
FDMER <- data.frame(phases)
FDMER$intervention <- 'FDMER'
FDMER$revenue   <- ifelse(FDMER$phase=='MP', log10_sample(1, 9, 0, 3), FDMER$revenue)

# Intervention: PDMER
# TODO: This will not work properly later since the MER will be spread over
# market years in interpolation step.
# NOTE: Adds to old revenue.
PDMER <- data.frame(phases)
PDMER$intervention <- 'PDMER'
PDMER$revenue   <- ifelse(PDMER$phase=='MP', PDMER$revenue + log10_sample(1, 9, 0, 3), PDMER$revenue)

# Merge all datasets
treated <- rbind(phases, PCER, P1ER, P2ER, P3ER, P4ER, FDMER, PDMER)

# Write to disk
write.csv(treated, row.names=FALSE, file=OUTPUT)

