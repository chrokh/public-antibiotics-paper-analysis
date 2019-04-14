source('shared.R')

intervene <- function(phases) {

  N <- nrow(phases)

  # Convert factors to ordered factors
  phases$phase <- factor(phases$phase, levels=PHASE_LEVELS, ordered=TRUE)
  phases$intervention <- factor('NONE', levels=INTERVENTION_LEVELS)

  # Log sampling function
  log10_sample <- function (min, max, magnitude_min, magnitude_max) {
    runif(N, min, max) * (10 ^ runif(N, magnitude_min, magnitude_max))
  }

  # Intervention: P1ER
  P1ER <- data.frame(phases)
  P1ER$intervention <- 'P1ER'
  P1ER$grants   <- ifelse(P1ER$phase=='P1', P1ER$grants + log10_sample(1, 9, 1, 3), P1ER$grants)

  # Intervention: P2ER
  P2ER <- data.frame(phases)
  P2ER$intervention <- 'P2ER'
  P2ER$grants   <- ifelse(P2ER$phase=='P2', P2ER$grants + log10_sample(1, 9, 1, 3), P2ER$grants)

  # Intervention: P3ER
  P3ER <- data.frame(phases)
  P3ER$intervention <- 'P3ER'
  P3ER$grants   <- ifelse(P3ER$phase=='P3', P3ER$grants + log10_sample(1, 9, 1, 3), P3ER$grants)

  # Intervention: P4ER
  P4ER <- data.frame(phases)
  P4ER$intervention <- 'P4ER'
  P4ER$grants   <- ifelse(P4ER$phase=='P4', P4ER$grants + log10_sample(1, 9, 1, 3), P4ER$grants)

  # Intervention: PDMER (partial delinkage)
  PDMER <- data.frame(phases)
  PDMER$intervention <- 'PDMER'
  PDMER$grants   <- ifelse(PDMER$phase=='MP', PDMER$grants + log10_sample(1, 9, 1, 3), PDMER$grants)

  # Merge all datasets, and return
  rbind(phases, P1ER, P2ER, P3ER, P4ER, PDMER)
}
