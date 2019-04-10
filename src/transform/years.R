# DESCRIPTION:
# This script converts a data set in phase-form to year-form.

library(dplyr)
library(ggplot2)
library(gridExtra)

# Config
INPUT  <- 'output/data/treated_phases.csv'
OUTPUT <- 'output/data/years.csv'


# I/O
source('src/shared.R')
phases <- read.csv(INPUT)


# Convert phases to ordered factor
phases$phase <- factor(phases$phase, levels=PHASE_LEVELS, ordered=TRUE)

# Add timestamps to every observation
phases <- phases %>%
  group_by(intervention, subject) %>%
  arrange(intervention, subject, phase) %>%
  mutate(t = cumsum(time) - time)

# TODO: This file is a mess and while it seems to work it needs to be cleaned up!

# TODO: Can be simplified and moved into the loop (to improve readability) now
# that we've separated grants from sales. This means that the
# easing/distribution function does not vary over rows but rather over cols.
# ======
# Compute: Sales slopes and offsets to allow computing discrete
phases$cost.a     <- 0
phases$cost.b     <- phases$cost / phases$time
phases$sales.a    <- (phases$sales * 2 / (phases$time + 1)) / phases$time
phases$sales.b    <- 0
# Compute: Discrete prob steps
phases$prob_full_step <- phases$prob ^ (1 / phases$time) # NOTE: Not applicable if time < 1
phases$prob_remainder <- phases$prob / (phases$prob_full_step ^ floor(phases$time))

# Transform: To cashflows over time (phase yearly)
phase_years <- tibble()
for (x in 1:ceiling(max(phases$time) + 1)) {

  # Prepare
  within_phase   <- x <= phases$time
  not_whole_year <- x-phases$time>0 & x-phases$time<1

  # cost
  base_cost          <- ifelse(within_phase, phases$cost.a * x + phases$cost.b, 0)
  remainder_cost     <- ifelse(not_whole_year, (phases$time-floor(phases$time))*(phases$cost/phases$time), 0)
  # NOTE: The remainder is computed by assuming that the value is evenly
  # distributed over the whole phase (i.e. constantly). If this is not true
  # then the remainder will be incorrectly computed.
  # NOTE: This computation is a bit odd since there will never be a base
  # cashflow and a remainder at the same time.
  # Make data frame
  discount.rate  <- phases$discount.rate
  time           <- phases$time
  cost           <- base_cost + remainder_cost

  # Sales
  # NOTE: We're iterating with + 1 to capture fractional years, but this means
  # that we can reach a year for which there should be no sales.
  sales <- ifelse(within_phase, phases$sales.a * x + phases$sales.b, 0)

  # Grants
  if (x == 1) {
    grants <- phases$grants
  } else {
    grants <- 0
  }

  # Revenue
  revenue        <- sales + grants

  # Prob
  prob_in_full <- ifelse(within_phase, phases$prob_full_step, 0)
  prob_in_partial <- ifelse(not_whole_year, phases$prob_remainder, 0)
  prob <- prob_in_full + prob_in_partial

  intervention   <- phases$intervention
  subject        <- phases$subject
  phase.year     <- x
  phase          <- phases$phase
  t              <- phases$t + phase.year - 1
  df             <- tibble(intervention, subject, phase.year, t, phase, cost, revenue, prob, time, discount.rate)
  df             <- df[cost != 0 | revenue != 0 | prob > 0, ] # No need to keep years without cashflow
  phase_years    <- bind_rows(phase_years, df)
}

write.csv(phase_years, row.names=FALSE, file=OUTPUT)
