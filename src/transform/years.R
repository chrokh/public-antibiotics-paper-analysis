# DESCRIPTION:
# This script converts a data set in phase-form to year-form.

library(dplyr)
library(ggplot2)
library(gridExtra)

# Config
INPUT  <- 'output/data/treated_phases.csv'
OUTPUT <- 'output/data/years.csv'


# I/O
phases <- read.csv(INPUT)


# Convert phases to ordered factor
phase_levels <- c('PC','P1','P2','P3','P4','MP')
phases$phase <- factor(phases$phase, levels=phase_levels, ordered=TRUE)

# Add timestamps to every observation
phases <- phases %>%
  group_by(intervention, subject) %>%
  arrange(intervention, subject, phase) %>%
  mutate(t = cumsum(time) - time)



# Compute: Cost/revenue slopes and offsets to allow computing discrete
# cashflows for years TODO: Separate market into separate dataset and then do
# the yearly2 calc for two datasets so that we don't have to deal with
# exceptions and instead simply assume that everything is either constantly or
# linearly distributed in the development and market data set respectively.
# TODO: Actually, shouldn't this market stuff really be handled in the
# generate.R file so that every 'dataset' is independently responsible for
# determining how the market is spread over years?
phases$cost.a     <- ifelse(phases$phase=='MP', ((phases$cost * 2 / (phases$time + 1)) / phases$time), 0)
phases$cost.b     <- ifelse(phases$phase=='MP', 0, (phases$cost / phases$time))
phases$revenue.a  <- ifelse(phases$phase=='MP', ((phases$revenue * 2 / (phases$time + 1)) / phases$time), 0)
phases$revenue.b  <- ifelse(phases$phase=='MP', 0, (phases$revenue / phases$time))
phases$prob.a     <- 0
phases$prob.b     <- phases$prob ^ (1/phases$time)


# Transform: To cashflows over time (phase yearly)
phase_years <- tibble()
for (x in 1:ceiling(max(phases$time) + 1)) {
  # Prepare
  within_phase   <- x <= phases$time
  not_whole_year <- x-phases$time>0 & x-phases$time<1
  #base_cashflow      <- ifelse(within_phase, phases$cashflow.a * x + phases$cashflow.b, 0)
  #remainder_cashflow <- ifelse(not_whole_year, (phases$time-floor(phases$time))*(phases$cashflow/phases$time), 0)
  base_cost          <- ifelse(within_phase, phases$cost.a * x + phases$cost.b, 0)
  remainder_cost     <- ifelse(not_whole_year, (phases$time-floor(phases$time))*(phases$cost/phases$time), 0)
  base_revenue       <- ifelse(within_phase, phases$revenue.a * x + phases$revenue.b, 0)
  remainder_revenue  <- ifelse(not_whole_year, (phases$time-floor(phases$time))*(phases$revenue/phases$time), 0)
  base_prob          <- ifelse(within_phase, phases$prob.a * x + phases$prob.b, 0)
  remainder_prob     <- ifelse(not_whole_year, phases$prob / ((phases$prob.a*x+phases$prob.b) ^ floor(phases$time)), 0)
  # NOTE: The remainder is computed by assuming that the value is evenly
  # distributed over the whole phase (i.e. constantly). If this is not true
  # then the remainder will be incorrectly computed. However, since we assume
  # that phase properties are indeed constantly distributed over the course of
  # the phase, it is not a problem for phase properties. While we do assume
  # that market sales grow linearly this is not a problem since there will be
  # no remainder given that we assume that all markets span 10 years. However,
  # please be very careful and make sure you get this right!!
  # NOTE: This computation is a bit odd since there will never be a base
  # cashflow and a remainder at the same time.
  # Make data frame
  discount.rate  <- phases$discount.rate
  time           <- phases$time
  cost           <- base_cost + remainder_cost
  revenue        <- base_revenue + remainder_revenue
  prob           <- base_prob + remainder_prob
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

