library(dplyr)
library(ggplot2)

# Config
INPUT  <- 'output/data/treated_phases.csv'
OUTPUT <- 'output/plots/phases_from_phases.pdf'


# I/O
phases <- read.csv(INPUT)
pdf(OUTPUT)

# Factors
PHASE_LEVELS <- c('PC','P1','P2','P3','P4','MP')


# Convert phases to ordered factor
phases$phase <- factor(phases$phase, levels=PHASE_LEVELS, ordered=TRUE)

# Only analyze control group
phases <- filter(phases, intervention == 'NONE')

# Transform: Cartesian product of phases (phase from phase)
phases_from_phases <- tibble()
for (from in PHASE_LEVELS) {
  pfp <- phases %>%
    filter(phase >= from) %>%
    group_by(subject) %>%
    transmute(phase = phase,
           from = factor(from, levels=PHASE_LEVELS, ordered=TRUE),
           time.to = cumsum(time) - time,
           # Compute: PVs
           cost.pv    = cost / ((1 + discount.rate) ^ time.to),
           revenue.pv = (sales + grants) / ((1 + discount.rate) ^ time.to))
  phases_from_phases <- bind_rows(phases_from_phases, pfp)
}


# Plot: PV of different properties from different phases (violin matrices)
ggplot(filter(phases_from_phases, cost.pv>0), aes(phase, cost.pv, fill=from)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  ggtitle('Cost PV (present value) from the perspective of different phases') +
  facet_grid(rows=vars(from), cols=vars(phase), scales='free')
ggplot(filter(phases_from_phases, revenue.pv>0), aes(phase, revenue.pv, fill=from)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  ggtitle('Revenue PV (present value) from the perspective of different phases')
  #facet_grid(rows=vars(from), cols=vars(phase), scales='free')
ggplot(filter(phases_from_phases, (revenue.pv-cost.pv)!=0), aes(phase, (revenue.pv-cost.pv), fill=from)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  ggtitle('PV (present value) from the perspective of different phases') +
  facet_grid(rows=vars(from), cols=vars(phase), scales='free')

# Plot: PV of different properties from different phases (density plots)
# TODO: Why is P4 from P4 not a single line since it's a constant value?
# SOLUTION: Increase bandwidth (bw). Not sure if it's an appropriate solution
# though. Or switch to histograms.
phases_from_phases %>% filter(cost.pv > 0) %>%
  ggplot(aes(cost.pv, fill=from)) +
  geom_density(alpha=0.3) +
  ggtitle('Cost PV (present value) from the perspective of different phases') +
  facet_grid(phase ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Density')
phases_from_phases %>% filter(revenue.pv > 0) %>%
  ggplot(aes(revenue.pv, fill=from)) +
  geom_density(alpha=0.3) +
  ggtitle('Revenue PV (present value) from the perspective of different phases') +
  facet_grid(phase ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Density')


# Transform: PV means per view
phases_from_phases_summary <- phases_from_phases %>% group_by(phase, from) %>%
  summarise(revenue.pv.mean  = mean(revenue.pv),
            cost.pv.mean     = mean(cost.pv),
            time.to.mean     = mean(time.to))

# Plot: mean PVs of props for phases from phases
phases_from_phases_summary %>%
  filter(cost.pv.mean > 0) %>%
  ggplot(aes(phase, cost.pv.mean)) +
  geom_bar(stat='identity', aes(fill=from), position='dodge') + ylab('PV (cost)') +
  ggtitle('Mean cost PV of phase from the perspective of different phases')

phases_from_phases_summary %>%
  ggplot(aes(phase, (revenue.pv.mean - cost.pv.mean))) +
  geom_bar(stat='identity', aes(fill=from), position='dodge') + ylab('PV (cashflow)') +
  ggtitle('Mean cashflow PV of phase from the perspective of different phases')

# TODO: This plot feels out of place, and if this is plotted I guess remaining
# probability should be plotted as well.
phases_from_phases_summary %>%
  filter(time.to.mean > 0) %>%
  ggplot(aes(phase, time.to.mean)) +
  geom_bar(stat='identity', aes(fill=from), position='dodge') + ylab('Time to') +
  ggtitle('Mean time remaining to different phases from different phases')

