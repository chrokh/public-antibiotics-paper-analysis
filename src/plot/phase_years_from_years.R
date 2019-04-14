library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Config
INPUT  <- 'years.csv'
OUTPUT <- 'phase_years_from_years.pdf'


# I/O
phase_years <- read.csv(INPUT)
pdf(OUTPUT)

# Factors
PHASE_LEVELS <- c('PC','P1','P2','P3','P4','MP')
INTERVENTION_LEVELS <- c('NONE', 'P1ER', 'P2ER', 'P3ER', 'P4ER', 'PDMER')

# Convert factors to factors
phase_years$phase <- factor(phase_years$phase, levels=PHASE_LEVELS, ordered=TRUE)
phase_years$intervention <- factor(phase_years$intervention, levels=INTERVENTION_LEVELS)

# Transform: Phase years from different phases
phase_years_from_years <- tibble()
for (yr in 1:(ceiling(max(phase_years$t)) + 1)) {
  pyfy <- phase_years %>%
    filter(t >= yr) %>%
    group_by(intervention, subject) %>%
    arrange(subject, t) %>%
    mutate(from            = yr,
           time.to         = t - min(t),
           cashflow        = revenue - cost,
           cum.cost        = cumsum(cost),
           cum.revenue     = cumsum(revenue),
           cum.cashflow    = cum.revenue - cum.cost,
           rem.prob        = prod(prob) / cumprod(prob) * prob,
           cum.prob        = cumprod(prob),
           prob.to         = cum.prob / prob,
           # cost
           cost.rv         = cost * prob.to, # TODO:ok?
           cost.rv.cum     = cumsum(cost.rv),
           cost.pv         = cost / ((1 + discount.rate) ^ time.to),
           cost.rpv        = (cost / ((1 + discount.rate) ^ time.to)) * prob.to,
           cost.npv        = cumsum(cost.pv),
           cost.rnpv       = cumsum(cost.rpv), # TODO:ok?
           # revenue
           revenue.rv      = revenue * prob.to, # TODO:ok?
           revenue.rv.cum  = cumsum(revenue.rv),
           revenue.pv      = revenue / ((1 + discount.rate) ^ time.to),
           revenue.rpv     = (revenue / ((1 + discount.rate) ^ time.to)) * prob.to,
           revenue.npv     = cumsum(revenue.pv),
           revenue.rnpv    = cumsum(revenue.rpv), # TODO:ok?
           # cashflow
           cashflow.rv     = revenue.rv - cost.rv,
           cashflow.rv.cum = revenue.rv.cum - cost.rv.cum,
           cashflow.pv     = revenue.pv - cost.pv,
           cashflow.rpv    = revenue.rpv - cost.rpv,
           cashflow.npv    = revenue.npv - cost.npv,
           cashflow.rnpv   = revenue.rnpv - cost.rnpv,
           # year
           year            = floor(time.to)
           )
  phase_years_from_years <- bind_rows(phase_years_from_years, pyfy)
}

# Summarize: phase years from years
phase_years_from_years_summary <- phase_years_from_years %>%
  group_by(intervention, year, from) %>%
  summarize(cum.mean    = mean(cum.cashflow),
            rv.cum.mean = mean(cashflow.rv.cum),
            npv.mean    = mean(cashflow.npv),
            rnpv.mean   = mean(cashflow.rnpv))

# Transform: phase years from years summary from wide to long
phase_years_from_years_summary_long <- phase_years_from_years_summary %>%
  gather('valuation', 'value', -year, -from)

# Transform: Compute final year npv values from different years
# TODO: Not sure if final year is NPV is becoming skewed by zero cashflow years
# in the end. It seems to me that it shouldn't be a problem but: make some
# calculations to ensure that this is definitely not a problem!
final_year_from_years <- phase_years_from_years %>%
  group_by(intervention, subject, from) %>%
  filter(year == max(year)) %>%
  summarise(cashflow.npv    = tail(cashflow.npv, n=1),
            cashflow.rnpv   = tail(cashflow.rnpv, n=1),
            cum.cashflow    = tail(cum.cashflow, n=1),
            cashflow.rv.cum = tail(cashflow.rv.cum, n=1),
            )

# Summarize: Final year from year
final_year_from_years_summary <- final_year_from_years %>%
  group_by(intervention, from) %>%
  summarize(npv.mean    = mean(cashflow.npv),
            rnpv.mean   = mean(cashflow.rnpv),
            cum.mean    = mean(cum.cashflow),
            cum.rv.mean = mean(cashflow.rv.cum))

# Transform: Final year from years summary from wide to long
final_year_from_years_summary_long <- final_year_from_years_summary %>%
  gather('valuation', 'value', -from, -intervention)

# Plot: mean rnpv/npv over time (from years)
for (treatment in INTERVENTION_LEVELS) {
  sub <- final_year_from_years_summary_long %>% filter(intervention == treatment)

  # Convert valuation to factor so that we ensure legend stays consistent
  # between the plits by using scale_fill_manual.
  sub$valuation <- factor(sub$valuation)

  p1 <- ggplot(filter(sub, valuation=='npv.mean' | valuation=='rnpv.mean'),
               aes(from, value, color=valuation)) +
    geom_line() + geom_point() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_colour_discrete(drop=TRUE, limits = levels(sub$valuation)) +
    ggtitle(sprintf('Project value, starting from various years (intervention: %s)', treatment))

  # Plot: final value starting from different years
  p2 <- ggplot(sub, aes(from, value, color=valuation)) +
    geom_line() + geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_colour_discrete(drop=TRUE, limits = levels(sub$valuation)) +
    ggtitle(sprintf('Project value, starting from various years (intervention: %s)', treatment))

  grid.arrange(p1, p2, ncol=1)
}
