# Dependencies
library(dplyr)
library(forcats)
library(ggplot2)
library(ggridges)
library(tidyr)

# Visual config
options(tibble.width = Inf) # Always print all tibble cols

# Data config
set.seed(1)
N     = 50
YEARS = 25


# Factors
PHASES <- c('PC','P1','P2','P3','P4','MP')


# ========= SAMPLING ================

# Same across all phases
discount.rate <- runif(N, 0.09, 0.24)

# Sample: Pre Clinical
pc <- tibble(subject=1:N, phase=factor('PC', levels=PHASES, ordered=TRUE))
pc$time             <- runif(N, min=4.3, max=6)
pc$cost             <- runif(N, min=19, max=23.2)
pc$revenue          <- rep(0, N)
pc$prob             <- runif(N, min=0.175, max=0.69)
pc$discount.rate    <- discount.rate

# Sample: Phase 1
p1 <- tibble(subject=1:N, phase=factor('P1', levels=PHASES, ordered=TRUE))
p1$time             <- runif(N, min=0.75, max=1.8)
p1$prob             <- runif(N, min=0.25, max=0.837)
p1$cost             <- runif(N, min=7.3, max=12)
p1$revenue          <- rep(0, N)
p1$discount.rate    <- discount.rate

# Sample: Phase 2
p2 <- tibble(subject=1:N, phase=factor('P2', levels=PHASES, ordered=TRUE))
p2$time             <- runif(N, min=0.75, max=2.5)
p2$revenue          <- rep(0, N)
p2$cost             <- runif(N, min=7.12, max=18.72)
p2$prob             <- runif(N, min=0.34, max=0.74)
p2$discount.rate    <- discount.rate

# Sample: Phase 3
p3 <- tibble(subject=1:N, phase=factor('P3', levels=PHASES, ordered=TRUE))
p3$cost             <- runif(N, min=26.88, max=121.68)
p3$prob             <- runif(N, min=0.314, max=0.786)
p3$time             <- runif(N, min=0.83, max=3.9)
p3$revenue          <- rep(0, N)
p3$discount.rate    <- discount.rate

# Sample: Phase 4
p4 <- tibble(subject=1:N, phase=factor('P4', levels=PHASES, ordered=TRUE))
p4$time             <- runif(N, min=0.5, max=1.04)
p4$prob             <- runif(N, min=0.83, max=0.99)
p4$cost             <- rep(98.297168, N)
p4$revenue          <- rep(0, N)
p4$discount.rate    <- discount.rate

# Sample: Market sales
sales <- tibble(subject=1:N, phase=factor('MP', levels=PHASES, ordered=TRUE))
sales$prob          <- 1
sales$cost          <- 0
sales$revenue       <- runif(N, min=218, max=2500)
sales$time          <- 10
sales$discount.rate <- discount.rate

# Combine all phases into single dataset
phases <- bind_rows(pc, p1, p2, p3, p4, sales)

# Group by subject
phases <- phases %>% group_by(subject)

# ========= END SAMPLING ================



# Add: timestamps to every observation
phases <- phases %>% mutate(t = cumsum(time) - time)

# Compute: precompute cashflow since it's used alot
phases$cashflow <- phases$revenue - phases$cost

# Compute: Cost/revenue slopes and offsets to allow computing discrete
# cashflows for years TODO: Separate market into separate dataset and then do
# the yearly2 calc for two datasets so that we don't have to deal with
# exceptions and instead simply assume that everything is either constantly or
# linearly distributed in the development and market data set respectively.
phases$cost.a     <- ifelse(phases$phase=='MP', ((phases$cost * 2 / (phases$time + 1)) / phases$time), 0)
phases$cost.b     <- ifelse(phases$phase=='MP', 0, (phases$cost / phases$time))
phases$revenue.a  <- ifelse(phases$phase=='MP', ((phases$revenue * 2 / (phases$time + 1)) / phases$time), 0)
phases$revenue.b  <- ifelse(phases$phase=='MP', 0, (phases$revenue / phases$time))
phases$prob.a     <- 0
phases$prob.b     <- phases$prob ^ (1/phases$time)

# Plot: phase property distributions
ggplot(filter(phases, cost>0), aes(phase, cost, fill=phase)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  ggtitle('Cost by phase') +
  xlab('Phase') + ylab('USD (million)') + labs(fill='Phase')
ggplot(filter(phases, revenue>0), aes(phase, revenue, fill=phase)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  ggtitle('Revenue by phase') +
  xlab('Phase') + ylab('USD (million)') + labs(fill='Phase')
ggplot(filter(phases, prob<1), aes(phase, prob*100, fill=phase)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  ggtitle('Technical probability of success by phase') +
  xlab('Phase') + ylab('%') + labs(fill='Phase')
ggplot(phases, aes(phase, time, fill=phase)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  ggtitle('Duration by phase')

# Transform: phase properties to long from wide
phase_props <- phases %>%
  filter(phase != 'MP') %>%
  select(1:6) %>%
  gather(key='prop', value='value', 3:6) %>%
  group_by(subject, prop) %>%
  mutate(total = sum(value),
         ratio = value / total) # NOTE: will cause NaN if 0/0

# Plot: property distribution across phases
ggplot(filter(phase_props, !is.na(ratio)), aes(ratio*100, fill=phase)) +
  geom_density(alpha=0.75) +
  ggtitle('Property distribution across phases (grouped by property)') +
  facet_grid(prop ~ .) +
  xlab('Percentage of property in phase') + ylab('Density')
ggplot(phase_props, aes(ratio*100, fill=prop)) +
  geom_density(alpha=0.75) +
  ggtitle('Property distribution across phases (grouped by phase)') +
  facet_grid(phase ~ .) +
  xlab('Percentage of property in phase') + ylab('Density')
ggplot(phase_props, aes(x=prop,y=ratio*100, fill=prop)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75)) +
  ggtitle('Property distribution across phases') +
  facet_grid(~ phase) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks.x=element_blank()) +
  ylab('Percentage of property in phase') + xlab('Density')

# Summarize: property distribution across phases
phase_props_summary <- phase_props %>%
  group_by(phase, prop) %>%
  summarise(ratio.mean = mean(ratio))

# Plot: summary of property distribution across phases
ggplot(filter(phase_props_summary, is.finite(ratio.mean)), aes(x=prop, y=ratio.mean * 100)) +
  geom_bar(stat='identity', aes(fill=phase), position='dodge') +
  labs(fill='Phase') +
  xlab('Property & Phase') + ylab('Mean percentage of property in phase') +
  ggtitle('Mean property distribution across phases (grouped by property)')

# Plot: by phase
boxplot(data=phases, cumsum(cost) ~ phase, ylab='USD (million)', main='Cumulative cost by phase', las=1)
boxplot(data=phases, cumsum(revenue) ~ phase, ylab='USD (million)', main='Cumulative revenue by phase', las=1)
boxplot(data=phases, cumsum(cashflow) ~ phase, ylab='USD (million)', main='Cumulative cashflow by phase', las=1)
boxplot(data=phases, cumprod(prob)*100 ~ phase, ylab='Probability (%)', main='Cumulative technical probability of success by phase', las=1)
boxplot(data=phases, cumsum(time) ~ phase, ylab='Months', main='Cumulative duration by phase', las=1)


# Transform: Cartesian product of phases (phase from phase)
phases_from_phases <- tibble()
for (from in PHASES) {
  pfp <- phases %>%
    filter(phase >= from) %>%
    group_by(subject) %>%
    transmute(phase = phase,
           from = factor(from, levels=PHASES, ordered=TRUE),
           time.to = cumsum(time) - time,
           # Compute: PVs
           cost.pv    = cost / ((1 + discount.rate) ^ time.to),
           revenue.pv = revenue / ((1 + discount.rate) ^ time.to))
  phases_from_phases <- bind_rows(phases_from_phases, pfp)
}

# Plot: PVs of different phases from different phases
for (ph in PHASES) {
  sub <- filter(phases_from_phases, from == ph)
  boxplot(sub$cost.pv ~ sub$phase, las=1, ylab='USD (million)', main=sprintf('Present Value (PV) of cost from %s', ph))
  boxplot(sub$revenue.pv ~ sub$phase, las=1, ylab='USD (million)', main=sprintf('Present Value (PV) of revenue from %s', ph))
  boxplot((sub$revenue.pv - sub$cost.pv) ~ sub$phase, las=1, ylab='USD (million)', main=sprintf('Present Value (PV) of cashflow from %s', ph))
  boxplot(sub$time.to ~ sub$phase, las=1, ylab='Years', main=sprintf('Time to phase from %s', ph))
}

# Transform: PV means per view
phases_from_phases_summary <- phases_from_phases %>% group_by(phase, from) %>%
  summarise(revenue.pv.mean  = mean(revenue.pv),
            cost.pv.mean     = mean(cost.pv),
            cashflow.pv.mean = mean(revenue.pv - cost.pv),
            time.to.mean     = mean(time.to))

# Plot: PVs from different views in matrix
ggplot(phases_from_phases_summary, aes(phases_from_phases_summary$phase, phases_from_phases_summary$from, z=phases_from_phases_summary$cost.pv.mean)) +
  geom_tile(aes(fill = phases_from_phases_summary$cost.pv.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='lightgrey', high='red', name='PV') +
  xlab('Phase') + ylab('From') +
  ggtitle('Mean Present Value (PV) of phase cost from different phases') +
  coord_fixed()
ggplot(phases_from_phases_summary, aes(phases_from_phases_summary$phase, phases_from_phases_summary$from, z=phases_from_phases_summary$revenue.pv.mean)) +
  geom_tile(aes(fill = phases_from_phases_summary$revenue.pv.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='lightgrey', high='darkgreen', name='PV') +
  xlab('Phase') + ylab('From') +
  ggtitle('Mean Present Value (PV) of phase revenue from different phases') +
  coord_fixed()
ggplot(phases_from_phases_summary, aes(phases_from_phases_summary$phase, phases_from_phases_summary$from, z=phases_from_phases_summary$cashflow.pv.mean)) +
  geom_tile(aes(fill = phases_from_phases_summary$cashflow.pv.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='lightgrey', high='darkgreen', name='PV') +
  xlab('Phase') + ylab('From') +
  ggtitle('Mean Present Value (PV) of phase cashflow from different phases') +
  coord_fixed()
ggplot(phases_from_phases_summary, aes(phases_from_phases_summary$phase, phases_from_phases_summary$from, z=phases_from_phases_summary$time.to.mean)) +
  geom_tile(aes(fill = phases_from_phases_summary$time.to.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='lightgrey', high='darkgreen', name='Yrs') +
  xlab('To') + ylab('From') +
  ggtitle('Mean time to phase from different phases') +
  coord_fixed()



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
  subject        <- phases$subject
  phase.year     <- x
  phase          <- phases$phase
  t              <- phases$t + phase.year - 1
  df             <- tibble(subject, phase.year, t, phase, cost, revenue, prob, time, discount.rate)
  df             <- df[cost != 0 | revenue != 0 | prob > 0, ] # No need to keep years without cashflow
  phase_years    <- bind_rows(phase_years, df)
}

# Plot: Property per phase year of different phases
for (ph in PHASES) {
  sub <- subset(phase_years, phase_years$phase == ph)
  boxplot(sub$cost ~ sub$phase.year, main=paste('Cost per year in', ph))
  mtext('Note: phase duration vary across projects.')
  boxplot(sub$revenue ~ sub$phase.year, main=paste('Revenue per year in', ph))
  mtext('Note: phase duration vary across projects.')
  boxplot(sub$prob * 100 ~ sub$phase.year, main=paste('Probability of success per year in', ph))
  mtext('Note: phase duration vary across projects.')
}

# Summarize: phase_years
phase_years_summary <- phase_years %>% group_by(subject, phase) %>%
  arrange(phase.year) %>%
  mutate(cum.phase.cost     = cumsum(cost),
         cum.phase.revenue  = cumsum(revenue),
         cum.phase.cashflow = cumsum(revenue - cost),
         ) %>%
  group_by(phase, phase.year) %>%
  summarise(revenue.mean  = mean(revenue),
            cost.mean     = mean(cost),
            cashflow.mean = mean(revenue - cost),
            cum.phase.revenue.mean  = mean(cum.phase.revenue),
            cum.phase.cost.mean     = mean(cum.phase.cost),
            cum.phase.cashflow.mean = mean(cum.phase.revenue - cum.phase.cost),
            )

# Plot: PVs from different views in matrix
sub <- phase_years_summary
ggplot(sub, aes(x=sub$phase, y=sub$phase.year, z=sub$cost.mean)) +
  geom_tile(aes(fill =sub$cost.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='lightgrey', high='red', name='USD\n(million)') +
  xlab('Phase') + ylab('Year') +
  ggtitle('Mean cost per phase year')
ggplot(sub, aes(x=sub$phase, y=sub$phase.year, z=sub$revenue.mean)) +
  geom_tile(aes(fill =sub$revenue.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='lightgrey', high='darkgreen', name='USD\n(million)') +
  xlab('Phase') + ylab('Year') +
  ggtitle('Mean revenue per phase year')
ggplot(sub, aes(x=sub$phase, y=sub$phase.year, z=sub$cum.phase.cost.mean)) +
  geom_tile(aes(fill =sub$cum.phase.cost.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='lightgrey', high='red', name='USD\n(million)') +
  xlab('Phase') + ylab('Year') +
  ggtitle('Mean cumulative cost per phase year')


# Transform: Phase years from different phases
phase_years_from_phases <- tibble()
for (from in PHASES) {
  pyfp <- phase_years %>% filter(phase >= from) %>%
    group_by(subject) %>%
    arrange(subject, t) %>%
    mutate(from        = factor(from, levels=PHASES, ordered=TRUE),
           time.to     = t - min(t),
           cum.cost    = cumsum(cost),
           cum.revenue = cumsum(revenue),
           cost.pv     = cost / ((1 + discount.rate) ^ time.to),
           revenue.pv  = revenue / ((1 + discount.rate) ^ time.to),
           cost.npv    = cumsum(cost.pv),
           revenue.npv = cumsum(revenue.pv),
           year        = floor(time.to)
           )
  phase_years_from_phases <- bind_rows(phase_years_from_phases, pyfp)
}


# Plot: Yearly data from different phases)
for (ph in PHASES) {
  sub <- filter(phase_years_from_phases, from == ph)
  # Cost
  boxplot(data=sub, cost ~ year, xlab='Year', ylab='USD (million)', las=1, main=sprintf('Cost per year from %s', ph))
  boxplot(data=sub, -cost.pv ~ year, xlab='Year', ylab='USD (million)', las=1, main=sprintf('Cost PV of future years from %s', ph))
  boxplot(data=sub, -cost.npv ~ year, xlab='Exit Year', ylab='USD (million)', las=1, main=sprintf('Cost NPV (cumulative PV) per year from %s', ph))
  # Revenue
  boxplot(data=sub, revenue ~ year, xlab='Year', ylab='USD (million)', las=1, main=sprintf('Revenue per year from %s', ph))
  boxplot(data=sub, revenue.pv ~ year, xlab='Year', ylab='USD (million)', las=1, main=sprintf('Revenue PV of future years from %s', ph))
  boxplot(data=sub, revenue.npv ~ year, xlab='Exit Year', ylab='USD (million)', las=1, main=sprintf('Revenue NPV (cumulative PV) per year from %s', ph))
  # Cashflow
  boxplot(data=sub, (revenue - cost) ~ year, xlab='Year', ylab='USD (million)', las=1, main=sprintf('Cashflow per year from %s', ph))
  boxplot(data=sub, (revenue.pv - cost.pv) ~ year, xlab='Year', ylab='USD (million)', las=1, main=sprintf('Cashflow PV of future years from %s', ph))
  boxplot(data=sub, (revenue.npv - cost.npv) ~ year, xlab='Exit Year', ylab='USD (million)', las=1, main=sprintf('Cashflow NPV (cumulative PV) per year from %s', ph))
  # Prob
  boxplot(data=sub, prob*100 ~ year, xlab='Year', ylab='Probability (%)', las=1, main=sprintf('Technical probability of success, per year from %s', ph))
}


for (ph in PHASES) {
  sub <- phase_years_from_phases %>%
    group_by(subject) %>%
    filter(from == ph) %>%
    filter(year == max(year)) %>%
    summarise(cost.npv = tail(cost.npv, n=1),
              revenue.npv = tail(revenue.npv, n=1),
              cashflow.npv = tail(revenue.npv, n=1) - tail(cost.npv, n=1)
              )

  print(ggplot(sub, aes(from, cashflow.npv)) +
    geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.5) +
    ggtitle('Final year NPV from PC'))

  print(ggplot(sub, aes(cashflow.npv)) + geom_histogram(aes(y=..density..)) + geom_density(col=2))
}

sub <- phase_years_from_phases %>%
  group_by(subject, from) %>%
  filter(year == max(year)) %>%
  summarise(cashflow.npv = tail(revenue.npv, n=1) - tail(cost.npv, n=1),
            cum.cashflow = tail(cum.revenue, n=1) - tail(cum.cost, n=1),
            )

ggplot(sub, aes(cum.cashflow, from)) +
  geom_density_ridges(quantile_lines = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle('Cumulative cashflow density from different starting phases') +
  xlab('Cumulative cashflow (million USD)') + ylab('Starting Phase')

ggplot(sub, aes(cashflow.npv, from)) +
  geom_density_ridges(quantile_lines = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle('NPV density from different starting phases') +
  xlab('NPV') + ylab('Starting Phase')

sub <- phase_years_from_phases %>%
  filter(from == 'PC') %>%
  group_by(subject, year) %>%
  summarise(cashflow.npv = tail(revenue.npv, n=1) - tail(cost.npv, n=1))

  ggplot(sub, aes(cashflow.npv, as.factor(year))) +
  geom_density_ridges(quantile_lines = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle('NPV density at different exit years, starting from PC') +
  xlab('NPV') + ylab('Exit year')

sub <- phase_years_from_phases %>%
  filter(from == 'PC') %>%
  group_by(subject, phase) %>%
  summarise(cashflow.npv = tail(revenue.npv, n=1) - tail(cost.npv, n=1))

  ggplot(sub, aes(cashflow.npv, phase)) +
  geom_density_ridges(quantile_lines = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle('NPV density when exiting at the end(?) of different phases, starting from PC') +
  xlab('NPV') + ylab('Exit year')


# Summarize: phase_years_from_phases
phase_years_from_phases_summary <- phase_years_from_phases %>%
  group_by(from, year) %>%
  summarise(cost.mean     = mean(cost),
            revenue.mean  = mean(revenue),
            cashflow.mean = mean(revenue - cost),
            cum.cost.mean     = mean(cum.cost),
            cum.revenue.mean  = mean(cum.revenue),
            cum.cashflow.mean = mean(cum.revenue - cum.cost),
            cost.pv.mean     = mean(cost.pv),
            revenue.pv.mean  = mean(revenue.pv),
            cashflow.pv.mean = mean(revenue.pv - cost.pv),
            cost.npv.mean     = mean(cost.npv),
            revenue.npv.mean  = mean(revenue.npv),
            cashflow.npv.mean = mean(revenue.npv - cost.npv),
            )

# Plot: Properties by year from phase
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=-cost.mean)) +
  geom_tile(aes(fill=-cost.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='red', high='grey90', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean yearly costs from different phases')
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=revenue.mean)) +
  geom_tile(aes(fill=revenue.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='grey90', high='darkgreen', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean yearly revenues from different phases')
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=cashflow.mean)) +
  geom_tile(aes(fill=cashflow.mean)) +
  theme_minimal() +
  scale_fill_gradient2(low='red', mid='grey90', high='darkgreen', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean yearly cashflows from different phases')

# Plot: PV by year from phase
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=-cost.pv.mean)) +
  geom_tile(aes(fill=-cost.pv.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='red', high='grey90', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean PV of future costs from different phases')
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=revenue.pv.mean)) +
  geom_tile(aes(fill=revenue.pv.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='grey90', high='darkgreen', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean PV of future revenues from different phases')
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=cashflow.pv.mean)) +
  geom_tile(aes(fill=cashflow.pv.mean)) +
  theme_minimal() +
  scale_fill_gradient2(low='red', mid='grey90', high='darkgreen', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean PV of future cashflows from different phases')

# Plot: Cumulative properties by year from phase
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=-cum.cost.mean)) +
  geom_tile(aes(fill=-cum.cost.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='red', high='grey90', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean cumulative costs by year from different phases')
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=cum.revenue.mean)) +
  geom_tile(aes(fill=cum.revenue.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='grey90', high='darkgreen', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean cumulative revenues by year from different phases')
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=cum.cashflow.mean)) +
  geom_tile(aes(fill=cum.cashflow.mean)) +
  theme_minimal() +
  scale_fill_gradient2(low='red', mid='grey90', high='darkgreen', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean cumulative cashflows by year from different phases')

# Plot: NPV by year from phase
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=-cost.npv.mean)) +
  geom_tile(aes(fill=-cost.npv.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='red', high='grey90', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean NPV (costs only) of future exit points starting from different phases')
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=revenue.npv.mean)) +
  geom_tile(aes(fill=revenue.npv.mean)) +
  theme_minimal() +
  scale_fill_gradient(low='grey90', high='darkgreen', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean NPV (revenues only) of future exit points starting from different phases')
ggplot(phase_years_from_phases_summary, aes(x=from, y=year, z=cashflow.npv.mean)) +
  geom_tile(aes(fill=cashflow.npv.mean)) +
  theme_minimal() +
  scale_fill_gradient2(low='red', mid='grey90', high='darkgreen', name='USD\n(million)') +
  xlab('Phase used as starting point') + ylab('Year') +
  ggtitle('Mean NPV of future exit points starting from different phases')

