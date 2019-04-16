library(dplyr)
library(ggplot2)

# Config
INPUT  <- 'treated_phases.csv'
OUTPUT <- 'phases_from_phases.pdf'

# I/O
phases <- read.csv(INPUT)
pdf(OUTPUT)

# Convert phases to ordered factor
PHASE_LEVELS <- c('PC','P1','P2','P3','P4','MP')
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
           revenue.pv = (sales + prizes) / ((1 + discount.rate) ^ time.to))
  phases_from_phases <- bind_rows(phases_from_phases, pfp)
}


# ==================================
# Cost PV from different phases
# ==================================

phases_from_phases %>% filter(cost.pv > 0) %>%
  ggplot(aes(phase, cost.pv, fill=from)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  ggtitle('Cost PV (present value) from the perspective of different phases') +
  facet_grid(rows=vars(from), cols=vars(phase), scales='free')

phases_from_phases %>% filter(cost.pv > 0) %>%
  ggplot(aes(phase, cost.pv, fill=from)) +
  geom_boxplot(alpha=0.7) +
  ggtitle('Cost PV (present value) from the perspective of different phases')

phases_from_phases %>% filter(cost.pv > 0) %>%
  ggplot(aes(from, cost.pv, fill=phase)) +
  geom_boxplot(alpha=0.7) +
  ggtitle('Cost PV (present value) from the perspective of different phases')

# TODO: NOTE: Assumes distribution (normal?). Consider e.g. p4 from p4. Should be line/point.
phases_from_phases %>% filter(cost.pv > 0) %>%
  ggplot(aes(cost.pv, fill=from)) +
  geom_density(alpha=0.5, bw=2) +
  ggtitle('Cost PV (present value) from the perspective of different phases') +
  facet_grid(phase ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Density')

phases_from_phases %>% filter(cost.pv > 0) %>%
  ggplot(aes(cost.pv, fill=phase)) +
  geom_density(alpha=0.5, bw=2) +
  ggtitle('Cost PV (present value) from the perspective of different phases') +
  facet_grid(from ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Density')

phases_from_phases %>% filter(cost.pv > 0) %>%
  ggplot(aes(cost.pv, fill=from, color=from)) +
  geom_histogram(binwidth=2, alpha=0.5, position='identity') +
  ggtitle('Cost PV (present value) from the perspective of different phases') +
  facet_grid(phase ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Frequency')

phases_from_phases %>% filter(cost.pv > 0) %>%
  ggplot(aes(cost.pv, fill=phase, color=phase)) +
  geom_histogram(binwidth=2, alpha=0.5, position='identity') +
  ggtitle('Cost PV (present value) from the perspective of different phases') +
  facet_grid(from ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Frequency')


# ======================================
# Revenue PV from different phases
# ======================================

phases_from_phases %>% filter(revenue.pv > 0) %>%
  ggplot(aes(phase, revenue.pv, fill=from)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  ggtitle('Revenue PV (present value) from the perspective of different phases')

phases_from_phases %>% filter(revenue.pv > 0) %>%
  ggplot(aes(phase, revenue.pv, fill=from)) +
  geom_boxplot(alpha=0.7) +
  ggtitle('Revenue PV (present value) from the perspective of different phases')

phases_from_phases %>% filter(revenue.pv > 0) %>%
  ggplot(aes(revenue.pv, fill=from)) +
  geom_density(alpha=0.3) +
  ggtitle('Revenue PV (present value) from the perspective of different phases') +
  facet_grid(phase ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Density')

phases_from_phases %>% filter(revenue.pv > 0) %>%
  ggplot(aes(revenue.pv, fill=from, color=from)) +
  geom_histogram(binwidth=50, alpha=0.5, position='identity') +
  ggtitle('Revenue PV (present value) from the perspective of different phases') +
  facet_grid(phase ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Density')


# ==============================
# PV from different phases
# ==============================

with_pv <- phases_from_phases %>%
  filter(from != 'MP') %>%
  filter(phase != 'MP') %>%
  filter(revenue.pv - cost.pv != 0)
with_pv$pv <- with_pv$revenue.pv - with_pv$cost.pv

with_pv %>%
  ggplot(aes(phase, pv, fill=from)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  ggtitle('PV (present value) from the perspective of different phases') +
  facet_grid(rows=vars(from), cols=vars(phase), scales='free')

with_pv %>%
  ggplot(aes(phase, pv, fill=from)) +
  geom_boxplot(alpha=0.7) +
  ggtitle('PV (present value) from the perspective of different phases')

with_pv %>%
  ggplot(aes(from, pv, fill=phase)) +
  geom_boxplot(alpha=0.7) +
  ggtitle('PV (present value) from the perspective of different phases')

with_pv %>%
  ggplot(aes(pv, fill=from)) +
  # TODO: NOTE: Assumes distribution (normal?). Consider e.g. p4 from p4. Should be line/point.
  geom_density(alpha=0.5, bw=2) +
  ggtitle('PV (present value) from the perspective of different phases') +
  facet_grid(phase ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Density')

with_pv %>%
  ggplot(aes(pv, fill=phase)) +
  # TODO: NOTE: Assumes distribution (normal?). Consider e.g. p4 from p4. Should be line/point.
  geom_density(alpha=0.5, bw=2) +
  ggtitle('PV (present value) from the perspective of different phases') +
  facet_grid(from ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Density')

with_pv %>%
  ggplot(aes(pv, fill=from, color=from)) +
  geom_histogram(binwidth=2, alpha=0.5, position='identity') +
  ggtitle('PV (present value) from the perspective of different phases') +
  facet_grid(phase ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Frequency')

with_pv %>%
  ggplot(aes(pv, fill=phase, color=phase)) +
  geom_histogram(binwidth=2, alpha=0.5, position='identity') +
  ggtitle('PV (present value) from the perspective of different phases') +
  facet_grid(from ~ ., scales='free_y') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab('PV (million USD)') + ylab('Frequency')


# ==================================
# Means
# ==================================

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

