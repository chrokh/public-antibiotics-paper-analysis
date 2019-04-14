library(dplyr)
library(ggplot2)
library(gridExtra)

source('shared.R')

# Converts a data set in phase-form to year-form.
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
    grants <- if (x == 1) phases$grants else 0 # Immediate lump-sum

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
                   revenue = sales + grants,
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
