library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(scales)

# Config
INPUT  <- 'output/data/years.csv'
OUTPUT <- 'output/plots/interventions.pdf'
set.seed(1)


# I/O
pdf(OUTPUT)
phase_years <- read.csv(INPUT)
source('src/shared.R')

# Convert factors to factors
phase_years$phase <- factor(phase_years$phase, levels=PHASE_LEVELS, ordered=TRUE)
phase_years$intervention <- factor(phase_years$intervention, levels=INTERVENTION_LEVELS)

# Rename: private sector discount rate
names(phase_years)[names(phase_years) == 'discount.rate'] <- 'private_discount_rate'

# Sample: public sector discount rate
phase_years <- phase_years %>%
  group_by(subject) %>%
  mutate(public_discount_rate = runif(1, min=0.035, max=0.045))

# Compute: cashflow
phase_years$treated_cashflow <- phase_years$revenue - phase_years$cost

# Drop: unused variables
phase_years <- phase_years[!names(phase_years) %in% c('phase.year', 'cost', 'revenue')]

# Compute: Cost of interventions
untreated <- phase_years %>% filter(intervention == 'NONE')
untreated <- data.frame(subject=untreated$subject, t=untreated$t, untreated_cashflow=untreated$treated_cashflow)
phase_years <- merge(phase_years, untreated, by=c('subject', 't'))
phase_years$intervention_cost <- phase_years$treated_cashflow - phase_years$untreated_cashflow

# Compute: rNPV and other aggregates
phase_years <- phase_years %>%
  group_by(intervention, subject) %>%
  arrange(subject, t) %>%
  transmute(time.to = t - min(t),
            prob_to = cumprod(prob) / prob,
            # private value
            cum     = cumsum(treated_cashflow),
            rv      = treated_cashflow * prob_to,
            rv.cum  = cumsum(rv),
            pv      = treated_cashflow / ((1 + private_discount_rate) ^ time.to),
            rpv     = (treated_cashflow / ((1 + private_discount_rate) ^ time.to)) * prob_to,
            npv     = cumsum(pv),
            rnpv    = cumsum(rpv),
            # public cost
            intervention_cost_cum = cumsum(intervention_cost),
            public_rpv = -(intervention_cost / ((1 + public_discount_rate) ^ time.to)) * prob_to,
            public_rnpv = cumsum(public_rpv),
            )

# Save final rnpv
finals <- phase_years %>%
  group_by(subject, intervention) %>%
  filter(time.to == max(time.to)) %>%
  summarise(cum    = tail(cum, n=1),
            rv.cum = tail(rv.cum, n=1),
            npv    = tail(npv, n=1),
            rnpv   = tail(rnpv, n=1),
            intervention_cost = tail(intervention_cost_cum, n=1),
            public_rnpv = tail(public_rnpv, n=1)
            )

# Compute: gos/nogos
finals$go <- factor(finals$rnpv >= 0, levels=c(TRUE, FALSE))


# =========================================================


# Predictive model: Logistic regression
model <- glm(data=finals,
             go ~ intervention_cost * intervention,
             family=binomial(link='logit'))

# Predict values
finals$pred_go_from_intervention_cost <- predict(model, type='response')


# =========================================================


# Predictive model: Logistic regression
model <- glm(data=finals,
             go ~ public_rnpv * intervention,
             family=binomial(link='logit'))

# Predict values
finals$pred_go_from_public_rnpv <- predict(model, type='response')


# =========================================================


# Plot: RNPV ~ intervention cost
ggplot(finals, aes(intervention_cost, rnpv, color=intervention)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  coord_cartesian(xlim = c(0, 3000), ylim = c(-25, 800)) +
  xlab('Intervention cost') +
  ylab('Private rNPV') +
  ggtitle('Correlation between intervention cost and private rNPV')

# Plot: RNPV ~ intervention cost (not log, facets)
ggplot(finals, aes(intervention_cost, rnpv, color=intervention)) +
  geom_point() +
  geom_smooth(method='lm', color='black', se=FALSE) +
  facet_wrap(. ~ intervention, ncol=2, scale='free') +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none') +
  ggtitle('Correlation between intervention cost and private rNPV')


# =========================================================


# Plot: Probability of go by intervention cost (log)
ggplot(finals, aes(intervention_cost, (1-pred_go_from_intervention_cost)*100, color=intervention)) +
  geom_line() +
  scale_x_continuous(trans = 'log',
                     breaks = c(1 %o% 10^(0:4)),
                     minor_breaks = c(1:9 %o% 10^(0:4))) +
  xlab('Intervention cost (million USD) (log 10)') + ylab('Probability') +
  ggtitle('Probability of PC go-decision by intervention cost (logistic regression)')

# Transform: Find intervention costs
min_costs <- finals %>%
  filter(intervention != 'NONE') %>% # TODO: Could it be included somehow?
  group_by(intervention) %>%
  summarise(
            '95%' = min(intervention_cost[pred_go_from_intervention_cost <= 0.05]),
            '50%' = min(intervention_cost[pred_go_from_intervention_cost <= 0.5]),
            '25%' = min(intervention_cost[pred_go_from_intervention_cost <= 0.75]),
            )
min_costs_long <- gather(min_costs, 'gorate', 'value', -intervention)

# Plot: Intervention costs for selected goratio thresholds
ggplot(min_costs_long,
       aes(x=intervention, y=value, fill=gorate,
           group=interaction(intervention, gorate))) +
  geom_bar(stat='identity', position='identity') +
  scale_y_continuous(trans = 'log',
                     breaks = c(1:9 %o% 10^(0:3)),
                     minor_breaks = c(1:9 %o% 10^(0:3))) +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        ) +
  ggtitle('Minimum intervention cost yielding go-rate')


# ==============================================


# Plot: Intervention cost and public rNPV
ggplot(finals, aes(intervention_cost, -public_rnpv, color=intervention)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  scale_x_continuous(trans = 'log',
                     breaks = c(1 %o% 10^(0:4)),
                     minor_breaks = c(1:9 %o% 10^(0:4))) +
  scale_y_continuous(trans = 'log',
                     breaks = c(1 %o% 10^(0:3)),
                     minor_breaks = c(1:9 %o% 10^(0:3))) +
  ggtitle('Correlation between intervention cost and public rNPV')


# =========================================================


# Plot: Private rNPV vs public rNPV
ggplot(finals, aes(y=rnpv, x=-public_rnpv, color=intervention)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  coord_cartesian(xlim = c(0, 500), ylim = c(-25, 200)) +
  ylab('Private rNPV') + xlab('Inverse public rNPV') +
  ggtitle('Correlation between negative public rNPV and positive private rNPV')

# Plot: Private rNPV vs public rNPV (not log, facets)
ggplot(finals, aes(y=rnpv, x=-public_rnpv, color=intervention)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  ylab('Private rNPV') + xlab('Inverse public rNPV') +
  ggtitle('Correlation between negative public rNPV and positive private rNPV') +
  facet_wrap(. ~ intervention, ncol=2, scale='free') +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')


# =========================================================


# Plot: Probability of go by public rNPV (log)
ggplot(finals, aes(-public_rnpv, (1-pred_go_from_public_rnpv)*100, color=intervention)) +
  geom_line() +
  scale_x_continuous(trans = 'log',
                     breaks = c(1 %o% 10^(0:4)),
                     minor_breaks = c(1:9 %o% 10^(0:4))) +
  xlab('Public rNPV') + ylab('Probability') +
  ggtitle('Probability of PC go-decision by by public rNPV (logistic regression)')


# Transform: Find public rnpvs
min_costs <- finals %>%
  filter(intervention != 'NONE') %>% # TODO: Could it be included somehow?
  group_by(intervention) %>%
  summarise(
            '95%' = min(-public_rnpv[pred_go_from_public_rnpv <= 0.05]),
            '50%' = min(-public_rnpv[pred_go_from_public_rnpv <= 0.5]),
            '25%' = min(-public_rnpv[pred_go_from_public_rnpv <= 0.75]),
            )
min_costs_long <- gather(min_costs, 'gorate', 'value', -intervention)

# Plot: Intervention costs for selected goratio thresholds
ggplot(min_costs_long,
       aes(x=intervention, y=value, fill=gorate,
           group=interaction(intervention, gorate))) +
  geom_bar(stat='identity', position='identity') +
  scale_y_continuous(trans = 'log',
                     breaks = c(1:9 %o% 10^(0:3)),
                     minor_breaks = c(1:9 %o% 10^(0:3))) +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        ) +
  ggtitle('Best (inverse) public rNPV yielding go-rate')

