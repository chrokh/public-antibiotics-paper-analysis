library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(scales)

# Config
INPUT  <- 'output/data/years.csv'
OUTPUT <- 'output/plots/interventions.pdf'


# I/O
phase_years <- read.csv(INPUT)
pdf(OUTPUT)

# Convert phase to ordered factor
phase_levels <- c('PC','P1','P2','P3','P4','MP')
phase_years$phase <- factor(phase_years$phase, levels=phase_levels, ordered=TRUE)

# Convert intervention to factor
intervention_levels <- c('NONE', 'P1ER', 'P2ER', 'P3ER', 'P4ER', 'PDMER')
phase_years$intervention <- factor(phase_years$intervention, levels=intervention_levels)

phase_years <- phase_years %>%
  group_by(intervention, subject) %>%
  arrange(subject, t) %>%
  transmute(time.to         = t - min(t),
            year            = floor(time.to),
            # prob
            cum.prob        = cumprod(prob),
            prob.to         = cum.prob / prob,
            # cost
            cost.cum        = cumsum(cost),
            cost.rv         = cost * prob.to, # TODO:ok?
            cost.rv.cum     = cumsum(cost.rv),
            cost.pv         = cost / ((1 + discount.rate) ^ time.to),
            cost.rpv        = (cost / ((1 + discount.rate) ^ time.to)) * prob.to,
            cost.npv        = cumsum(cost.pv),
            cost.rnpv       = cumsum(cost.rpv), # TODO:ok?
            # revenue
            revenue.cum     = cumsum(revenue),
            revenue.rv      = revenue * prob.to, # TODO:ok?
            revenue.rv.cum  = cumsum(revenue.rv),
            revenue.pv      = revenue / ((1 + discount.rate) ^ time.to),
            revenue.rpv     = (revenue / ((1 + discount.rate) ^ time.to)) * prob.to,
            revenue.npv     = cumsum(revenue.pv),
            revenue.rnpv    = cumsum(revenue.rpv), # TODO:ok?
            # cashflow
            cashflow.cum    = revenue.cum - cost.cum,
            cashflow.rv     = revenue.rv - cost.rv,
            cashflow.rv.cum = revenue.rv.cum - cost.rv.cum,
            cashflow.pv     = revenue.pv - cost.pv,
            cashflow.rpv    = revenue.rpv - cost.rpv,
            cashflow.npv    = revenue.npv - cost.npv,
            cashflow.rnpv   = revenue.rnpv - cost.rnpv,
            )

# Save final rnpv
finals <- phase_years %>%
  group_by(subject, intervention) %>%
  filter(year == max(year)) %>%
  summarise(cashflow.cum    = tail(cashflow.cum, n=1),
            cashflow.rv.cum = tail(cashflow.rv.cum, n=1),
            cashflow.npv    = tail(cashflow.npv, n=1),
            cashflow.rnpv   = tail(cashflow.rnpv, n=1),
            )

# Compute: intervention size/cost
control <- finals %>% filter(intervention == 'NONE')
finals$control.cashflow.cum <- rep(control$cashflow.cum, each=length(intervention_levels))
finals$intervention_cost    <- finals$cashflow.cum - finals$control.cashflow.cum

# Plot: RNPV ~ intervention cost (log)
ggplot(finals, aes(intervention_cost, cashflow.rnpv, color=intervention)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  scale_y_continuous(trans = 'log',
                     breaks = c(1 %o% 10^(0:3)),
                     minor_breaks = c(1:9 %o% 10^(0:3))) +
  scale_x_continuous(trans = 'log',
                     breaks = c(1 %o% 10^(0:4)),
                     minor_breaks = c(1:9 %o% 10^(0:4)))

# Plot: RNPV ~ intervention cost (not log)
ggplot(finals, aes(intervention_cost, cashflow.rnpv, color=intervention)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10))

# Plot: RNPV ~ intervention cost (not log, facets)
ggplot(finals, aes(intervention_cost, cashflow.rnpv, color=intervention)) +
  geom_point() +
  geom_smooth(method='lm', color='black', se=FALSE) +
  facet_wrap(. ~ intervention, scale='free') +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')

# Compute: gos/nogos
finals$go <- factor(finals$cashflow.rnpv >= 0, levels=c(TRUE, FALSE))

# Predictive model: Logistic regression
finals <- finals
model <- glm(data=finals,
             go ~ intervention_cost * intervention,
             family=binomial(link='logit'))

# Predict values
finals$predicted <- predict(model, type='response')

# Plot: Probability of go by intervention cost (log)
ggplot(finals, aes(intervention_cost, (1-predicted)*100, color=intervention)) +
  geom_line() +
  scale_x_continuous(trans = 'log',
                     breaks = c(1 %o% 10^(0:4)),
                     minor_breaks = c(1:9 %o% 10^(0:4))) +
  xlab('Intervention cost (million USD) (log 10)') + ylab('Probability') +
  ggtitle('Probability of PC go-decision by intervention cost (logistic regression)')

# Plot: Probability of go by intervention cost (not log)
ggplot(finals, aes(intervention_cost, (1-predicted)*100, color=intervention)) +
  geom_line() +
  scale_x_continuous(breaks=pretty_breaks(n=10))

# Transform: Find intervention costs
min_costs <- finals %>%
  filter(intervention != 'NONE') %>% # TODO: Could it be included somehow?
  group_by(intervention) %>%
  summarise(
            '95%' = min(intervention_cost[predicted <= 0.05]),
            '50%' = min(intervention_cost[predicted <= 0.5]),
            '25%' = min(intervention_cost[predicted <= 0.75]),
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
        )

