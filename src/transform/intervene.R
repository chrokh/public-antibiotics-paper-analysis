# Config
INPUT  <- 'output/data/phases.csv'
OUTPUT <- 'output/data/treated_phases.csv'

# I/O
source('src/shared.R')
phases <- read.csv(INPUT)
N      <- nrow(phases)

# Convert factors to factors
phases$phase <- factor(phases$phase, levels=PHASE_LEVELS, ordered=TRUE)
phases$intervention <- factor('NONE', levels=INTERVENTION_LEVELS)

# Log sampling function
log10_sample <- function (min, max, magnitude_min, magnitude_max) {
  runif(N, min, max) * (10 ^ runif(N, magnitude_min, magnitude_max))
}

# Intervention: P1ER
P1ER <- data.frame(phases)
P1ER$intervention <- 'P1ER'
P1ER$revenue   <- ifelse(P1ER$phase=='P1', P1ER$revenue + log10_sample(1, 9, 1, 3), P1ER$revenue)

# Intervention: P2ER
P2ER <- data.frame(phases)
P2ER$intervention <- 'P2ER'
P2ER$revenue   <- ifelse(P2ER$phase=='P2', P2ER$revenue + log10_sample(1, 9, 1, 3), P2ER$revenue)

# Intervention: P3ER
P3ER <- data.frame(phases)
P3ER$intervention <- 'P3ER'
P3ER$revenue   <- ifelse(P3ER$phase=='P3', P3ER$revenue + log10_sample(1, 9, 1, 3), P3ER$revenue)

# Intervention: P4ER
P4ER <- data.frame(phases)
P4ER$intervention <- 'P4ER'
P4ER$revenue   <- ifelse(P4ER$phase=='P4', P4ER$revenue + log10_sample(1, 9, 1, 3), P4ER$revenue)

# Intervention: PDMER
# TODO: This will not work properly later since the MER will be spread over
# market years in interpolation step.
# NOTE: Adds to old revenue. I.e. partial delinkage.
PDMER <- data.frame(phases)
PDMER$intervention <- 'PDMER'
PDMER$revenue   <- ifelse(PDMER$phase=='MP', PDMER$revenue + log10_sample(1, 9, 1, 3), PDMER$revenue)

# Merge all datasets
treated <- rbind(phases, P1ER, P2ER, P3ER, P4ER, PDMER)

# Write to disk
write.csv(treated, row.names=FALSE, file=OUTPUT)

