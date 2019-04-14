library(dplyr)
library(ggplot2)
library(gridExtra)

# ------------------------------------------------------------------------
# Prelude
# ------------------------------------------------------------------------

# Seed randomizer for predictable output
set.seed(1)

# Factors
PHASE_LEVELS <- c('PC','P1','P2','P3','P4','MP')
INTERVENTION_LEVELS <- c('NONE', 'P1ER', 'P2ER', 'P3ER', 'P4ER', 'PDMER')


# ------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------

#' Stochastically generate a dataframe with samples from the hard-coded
#' distributions.
#'
#' @param N Number
#' @return Dataframe
sample <- function(N) {

  phases <- data.frame()

  # ============ SERTKAYA ET. AL (2014) ===============
  # NOTE: Does not model Sertkaya exactly.

  # Same across all phases
  discount.rate <- runif(N, 0.09, 0.24)

  # Samples phases
  pc <- data.frame(subject=1:N, phase=factor('PC'))
  pc$time             <- runif(N, min=4.3, max=6)
  pc$cost             <- runif(N, min=19, max=23.2)
  pc$sales            <- rep(0, N)
  pc$prizes           <- rep(0, N)
  pc$prob             <- runif(N, min=0.175, max=0.69)
  pc$discount.rate    <- discount.rate
  p1 <- data.frame(subject=1:N, phase=factor('P1'))
  p1$time             <- runif(N, min=0.75, max=1.8)
  p1$prob             <- runif(N, min=0.25, max=0.837)
  p1$cost             <- runif(N, min=7.3, max=12)
  p1$sales            <- rep(0, N)
  p1$prizes           <- rep(0, N)
  p1$discount.rate    <- discount.rate
  p2 <- data.frame(subject=1:N, phase=factor('P2'))
  p2$time             <- runif(N, min=0.75, max=2.5)
  p2$sales            <- rep(0, N)
  p2$prizes           <- rep(0, N)
  p2$cost             <- runif(N, min=7.12, max=18.72)
  p2$prob             <- runif(N, min=0.34, max=0.74)
  p2$discount.rate    <- discount.rate
  p3 <- data.frame(subject=1:N, phase=factor('P3'))
  p3$cost             <- runif(N, min=26.88, max=121.68)
  p3$prob             <- runif(N, min=0.314, max=0.786)
  p3$time             <- runif(N, min=0.83, max=3.9)
  p3$sales            <- rep(0, N)
  p3$prizes           <- rep(0, N)
  p3$discount.rate    <- discount.rate
  p4 <- data.frame(subject=1:N, phase=factor('P4'))
  p4$time             <- runif(N, min=0.5, max=1.04)
  p4$prob             <- runif(N, min=0.83, max=0.99)
  p4$cost             <- rep(98.297168, N)
  p4$sales            <- rep(0, N)
  p4$prizes           <- rep(0, N)
  p4$discount.rate    <- discount.rate
  mp <- data.frame(subject=1:N, phase=factor('MP'))
  mp$prob          <- 1
  mp$cost          <- 0
  mp$sales         <- runif(N, min=218, max=2500)
  mp$prizes        <- 0
  mp$time          <- 10
  mp$discount.rate <- discount.rate

  # Combine all phases into single dataset
  phases <- rbind(phases, pc, p1, p2, p3, p4, mp)
  # ===================================================

  # Return all data sets
  phases
}


#' Adds various stochastic interventions to a data frame containing phase
#' observations.
#'
#' @param A dataframe containing phase-based samples without interventions.
#' @return A dataframe containing phase-based samples repeated once per
#'   intervention.
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
  P1ER$prizes   <- ifelse(P1ER$phase=='P1', P1ER$prizes + log10_sample(1, 9, 1, 3), P1ER$prizes)

  # Intervention: P2ER
  P2ER <- data.frame(phases)
  P2ER$intervention <- 'P2ER'
  P2ER$prizes   <- ifelse(P2ER$phase=='P2', P2ER$prizes + log10_sample(1, 9, 1, 3), P2ER$prizes)

  # Intervention: P3ER
  P3ER <- data.frame(phases)
  P3ER$intervention <- 'P3ER'
  P3ER$prizes   <- ifelse(P3ER$phase=='P3', P3ER$prizes + log10_sample(1, 9, 1, 3), P3ER$prizes)

  # Intervention: P4ER
  P4ER <- data.frame(phases)
  P4ER$intervention <- 'P4ER'
  P4ER$prizes   <- ifelse(P4ER$phase=='P4', P4ER$prizes + log10_sample(1, 9, 1, 3), P4ER$prizes)

  # Intervention: PDMER (partial delinkage)
  PDMER <- data.frame(phases)
  PDMER$intervention <- 'PDMER'
  PDMER$prizes   <- ifelse(PDMER$phase=='MP', PDMER$prizes + log10_sample(1, 9, 1, 3), PDMER$prizes)

  # Merge all datasets, and return
  rbind(phases, P1ER, P2ER, P3ER, P4ER, PDMER)
}


#' Converts a data set in phase-form to year-form.
#'
#' @param phases Dataframe in phase-form.
#' @return Dataframe in year-form
to_years <- function (phases) {

  # Convert phases to ordered factor
  phases$phase <- factor(phases$phase, levels=PHASE_LEVELS, ordered=TRUE)

  # Add timestamps to every observation
  phases <- phases %>%
    group_by(intervention, subject) %>%
    arrange(intervention, subject, phase) %>%
    mutate(t = cumsum(time) - time)

  # Compute cost steps
  phases$cost_step <- (phases$cost / phases$time)
  phases$cost_remainder <- phases$cost - phases$cost_step * floor(phases$time)

  # Compute prob steps
  phases$prob_step <- phases$prob ^ (1 / phases$time)
  phases$prob_remainder <- phases$prob / (phases$prob_step ^ floor(phases$time))

  # Compute sales slopes
  phases$sales_slope <- (phases$sales * 2 / (phases$time + 1)) / phases$time

  # Transform: To cashflows over time (phase yearly)
  phase_years <- tibble()
  for (x in 1:ceiling(max(phases$time) + 1)) {

    # Compute step-based properties
    whole_step <- x <= phases$time
    cost <- ifelse(whole_step, phases$cost_step, phases$cost_remainder)
    sales <- ifelse(whole_step, phases$sales_slope * x, 0)
    prob <- ifelse(whole_step, phases$prob_step, phases$prob_remainder)
    prizes <- if (x == 1) phases$prizes else 0 # Immediate lump-sum

    # Make tibble
    has_decimals <- phases$time - floor(phases$time) != 0
    part_of_phase <- x <= phases$time | (x <= phases$time + 1 & has_decimals)
    year <- tibble(part_of_phase,
                   intervention = phases$intervention,
                   subject = phases$subject,
                   phase.year = x,
                   t = phases$t + x - 1,
                   phase = phases$phase,
                   cost,
                   revenue = sales + prizes,
                   prob,
                   time = phases$time,
                   discount.rate = phases$discount.rate
                   ) %>%
    filter(x <= time | (x <= time + 1 & has_decimals)) %>% # Only keep years that are part of the phase
    select(-c('part_of_phase')) # Remove temporary column

    # Append
    phase_years <- bind_rows(phase_years, year)
  }

  phase_years
}


# ------------------------------------------------------------------------
# Simulate (main entry point)
# ------------------------------------------------------------------------
phases <- sample(1000)
intervened <- intervene(phases)
years <- to_years(intervened)
dir.create('output', recursive=TRUE)
write.csv(phases, row.names=FALSE, file='output/phases.csv')
write.csv(intervened, row.names=FALSE, file='output/treated_phases.csv')
write.csv(years, row.names=FALSE, file='output/years.csv')
