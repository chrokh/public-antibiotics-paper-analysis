library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(gridExtra)

# Config
INPUT  <- 'output/data/years.csv'
OUTPUT <- 'output/plots/phase_years_from_phases.pdf'


# I/O
phase_years <- read.csv(INPUT)
pdf(OUTPUT)

# Factors
PHASE_LEVELS <- c('PC','P1','P2','P3','P4','MP')

# Only analyze control group
phase_years <- filter(phase_years, intervention == 'NONE')

# Convert phases to ordered factor
phase_years$phase <- factor(phase_years$phase, levels=PHASE_LEVELS, ordered=TRUE)

# Transform: Phase years from different phases
phase_years_from_phases <- tibble()
for (from in PHASE_LEVELS) {
  pyfp <- phase_years %>%
    filter(phase >= from) %>%
    group_by(subject) %>%
    arrange(subject, t) %>%
    mutate(from          = factor(from, levels=PHASE_LEVELS, ordered=TRUE),
           time.to       = t - min(t),
           cashflow      = revenue - cost,
           cum.cost      = cumsum(cost),
           cum.revenue   = cumsum(revenue),
           cum.cashflow  = cum.revenue - cum.cost,
           rem.prob      = prod(prob) / cumprod(prob) * prob,
           cum.prob      = cumprod(prob),
           prob.to       = cum.prob / prob,
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
  phase_years_from_phases <- bind_rows(phase_years_from_phases, pyfp)
}

# Plot: Yearly data from different phases
for (ph in c('PC')) {
  sub <- filter(phase_years_from_phases, from == ph)
  p1 <- ggplot(sub, aes(as.factor(year), cashflow)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggtitle('Not capitalized') +
    xlab(element_blank()) + ylab(element_blank()) +
    geom_boxplot(fill='#f8766d')
  p2 <- ggplot(sub, aes(as.factor(year), cashflow.rv)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggtitle('RV') +
    xlab(element_blank()) + ylab(element_blank()) +
    geom_boxplot(fill='#f8766d')
  p3 <- ggplot(sub, aes(as.factor(year), cashflow.pv)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggtitle('PV') +
    xlab(element_blank()) + ylab(element_blank()) +
    geom_boxplot(fill='#f8766d')
  p4 <- ggplot(sub, aes(as.factor(year), cashflow.rpv)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggtitle('rPV') +
    xlab(element_blank()) + ylab(element_blank()) +
    geom_boxplot(fill='#f8766d')
  p5 <- ggplot(sub, aes(as.factor(year), cum.cashflow)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggtitle('Not capitalized') +
    xlab(element_blank()) + ylab(element_blank()) +
    geom_boxplot(fill='#00bfc4')
  p6 <- ggplot(sub, aes(as.factor(year), cashflow.rv.cum)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggtitle('Cumulative RV') +
    xlab(element_blank()) + ylab(element_blank()) +
    geom_boxplot(fill='#00bfc4')
  p7 <- ggplot(sub, aes(as.factor(year), cashflow.npv)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggtitle('NPV (cumulative PV)') +
    xlab(element_blank()) + ylab(element_blank()) +
    geom_boxplot(fill='#00bfc4')
  p8 <- ggplot(sub, aes(as.factor(year), cashflow.rnpv)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggtitle('rNPV (cumulative rPV)') +
    xlab(element_blank()) + ylab(element_blank()) +
    geom_boxplot(fill='#00bfc4')
  grid.arrange(p1, p2, p3, p4, nrow=2, top=sprintf('Cashflow per year from %s', ph))
  grid.arrange(p5, p6, p7, p8, nrow=2, top=sprintf('Cumulative cashflow per year (i.e. value at different exit-points/targets) from %s', ph))
}


# Summarize: phase years from phases
phase_years_from_phases_summary <- phase_years_from_phases %>%
  group_by(year, from) %>%
  summarize(cum.mean    = mean(cum.cashflow),
            rv.cum.mean = mean(cashflow.rv.cum),
            npv.mean    = mean(cashflow.npv),
            rnpv.mean   = mean(cashflow.rnpv))

# Transform: phase years from phases summary from wide to long
phase_years_from_phases_summary_long <- phase_years_from_phases_summary %>%
  gather('valuation', 'value', -year, -from)


# Plot: mean rnpv/npv over time (from pc)
# TODO: P4 has a very dramatic drop in the last year, why? That makes no sense?
ggplot(filter(phase_years_from_phases_summary_long,
              valuation!='cum.mean' & valuation!='rv.cum.mean'
              ),
       aes(year, value, color=valuation)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylab('Valuation statistic') +
  facet_wrap(from~., scale='free', ncol=2) +
  ggtitle('Valuation of different exit years starting from different phases')


# Transform: Compute final year npv values (from phases)
# TODO: Not sure if final year is NPV is becoming skewed by zero cashflow years
# in the end. It seems to me that it shouldn't be a problem but: make some
# calculations to ensure that this is definitely not a problem!
final_year_from_phases <- phase_years_from_phases %>%
  group_by(subject, from) %>%
  filter(year == max(year)) %>%
  summarise(cashflow.npv    = tail(cashflow.npv, n=1),
            cashflow.rnpv   = tail(cashflow.rnpv, n=1),
            cum.cashflow    = tail(cum.cashflow, n=1),
            cashflow.rv.cum = tail(cashflow.rv.cum, n=1),
            )

# Summarize: Final year from phases
final_year_from_phases_summary <- final_year_from_phases %>%
  group_by(from) %>%
  summarize(npv.mean    = mean(cashflow.npv),
            rnpv.mean   = mean(cashflow.rnpv),
            cum.mean    = mean(cum.cashflow),
            cum.rv.mean = mean(cashflow.rv.cum))

# Transform: Final year from phases summary from wide to long
final_year_from_phases_summary_long <- final_year_from_phases_summary %>%
  gather('valuation', 'value', -from)

# Plot: mean rnpv/npv over time (from phases)
# TODO: Should plot at start of phase rather than at end of phase!
p1 <- ggplot(filter(final_year_from_phases_summary_long,
                    valuation=='npv.mean' | valuation=='rnpv.mean'),
             aes(from, value, color=valuation, group=valuation)) +
  geom_line() + geom_point() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle('Project value, starting from various phases')

# Plot: final value starting from different phases
p2 <- ggplot(final_year_from_phases_summary_long,
             aes(from, value, color=valuation, group=valuation)) +
  geom_line() + geom_point() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle('Project value, starting from various phases')

grid.arrange(p1, p2, ncol=1)


# Plot: Final year NPV starting from different phases (histogram)
summary <- final_year_from_phases %>%
  group_by(from) %>%
  summarize(rnpv.median = median(cashflow.rnpv),
            npv.median = median(cashflow.npv))
p1 <- ggplot(final_year_from_phases, aes(cashflow.rnpv, fill=from)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(col='black', fill='transparent') +
  geom_vline(data=summary, aes(xintercept=rnpv.median), colour='red') +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        legend.position='none') +
  facet_grid(from~., scale='free_y') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ggtitle('Project rNPV starting from different phases (red line=median)')
p2 <- ggplot(final_year_from_phases, aes(cashflow.npv, fill=from)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(col='black', fill='transparent') +
  geom_vline(data=summary, aes(xintercept=npv.median), colour='red') +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        legend.position='none') +
  facet_grid(from~., scale='free_y') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ggtitle('Project NPV starting from different phases (red line=median)')
grid.arrange(p1, p2)

# Summarize: rNPV and NPV at different exit phases
sub <- phase_years_from_phases %>%
  filter(from == 'PC') %>%
  group_by(subject, phase) %>%
  summarise(cashflow.npv = tail(cashflow.npv, n=1),
            cashflow.rnpv = tail(cashflow.rnpv, n=1))

# Plot: rNPV and NPV at different exit phases
p1 <- ggplot(sub, aes(cashflow.rnpv, phase)) +
  geom_density_ridges(quantile_lines = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle('rNPV density when exiting at the end(?) of different phases, starting from PC') +
  xlab('rNPV') + ylab('Exit phase')
p2 <- ggplot(sub, aes(cashflow.npv, phase)) +
  geom_density_ridges(quantile_lines = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle('NPV density when exiting at the end(?) of different phases, starting from PC') +
  xlab('NPV') + ylab('Exit phase')
grid.arrange(p1, p2, ncol=1)

