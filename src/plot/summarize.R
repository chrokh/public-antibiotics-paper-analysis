library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# config
INPUT  <- 'output/data/treated_phases.csv'
OUTPUT <- 'output/plots/summary.pdf'


# I/O
source('src/shared.R')
phases <- read.csv(INPUT)
pdf(OUTPUT)


# Convert phases to ordered factor
phases$phase <- factor(phases$phase, levels=PHASE_LEVELS, ordered=TRUE)


# Only do summary for control group and not treatment group
sub <- filter(phases, intervention == 'NONE')

# Plot: phase property distributions
p1 <- ggplot(sub, aes(cost, fill=phase)) +
  geom_histogram() +
  facet_grid(phase ~ ., scale='free_y') +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        legend.position='none') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  xlab('Cost (million USD)')
p2 <- ggplot(sub, aes(sales, fill=phase)) +
  geom_histogram() +
  facet_grid(phase ~ ., scale='free_y') +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        legend.position='none') +
  xlab('Sales (million USD)')
p5 <- ggplot(sub, aes(grants, fill=phase)) +
  geom_histogram() +
  facet_grid(phase ~ ., scale='free_y') +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        legend.position='none') +
  xlab('Grants (million USD)')
p3 <- ggplot(sub, aes(prob*100, fill=phase)) +
  geom_histogram() +
  facet_grid(phase ~ ., scale='free_y') +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        legend.position='none') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  xlab('Probability (%)')
p4 <- ggplot(sub, aes(time, fill=phase)) +
  geom_histogram() +
  facet_grid(phase ~ ., scale='free_y') +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        legend.position='none') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  xlab('Duration (months)')

grid.arrange(p1, p2, p3, p4, p5, ncol=3, top='Phase input distributions')


# Transform: phase properties to long from wide
phase_props <- sub %>%
  filter(phase != 'MP') %>%
  select(-discount.rate, -intervention) %>%
  gather(key='prop', value='value', -subject, -phase) %>%
  group_by(subject, prop) %>%
  mutate(total = sum(value),
         ratio = value / total) # NOTE: will cause NaN if 0/0


# Plot: property distribution across phases
print(ggplot(filter(phase_props, !is.na(ratio)), aes(ratio, fill=phase)) +
  geom_density(alpha=0.75) +
  ggtitle('Property distribution across phases (grouped by property)') +
  facet_grid(prop ~ .) +
  xlab('Percentage of property in phase') + ylab('Density'))
print(ggplot(phase_props, aes(ratio*100, fill=prop)) +
  geom_density(alpha=0.75) +
  ggtitle('Property distribution across phases (grouped by phase)') +
  facet_grid(phase ~ .) +
  xlab('Percentage of property in phase') + ylab('Density'))
print(ggplot(phase_props, aes(x=prop,y=ratio*100, fill=prop)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75)) +
  ggtitle('Property distribution across phases') +
  facet_grid(~ phase) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks.x=element_blank()) +
  ylab('Percentage of property in phase') + xlab('Density'))


# Summarize: property distribution across phases
phase_props_summary <- phase_props %>%
  group_by(phase, prop) %>%
  summarise(ratio.mean = mean(ratio))

# Plot: summary of property distribution across phases
print(ggplot(filter(phase_props_summary, is.finite(ratio.mean)), aes(x=prop, y=ratio.mean * 100)) +
  geom_bar(stat='identity', aes(fill=phase), position='dodge') +
  labs(fill='Phase') +
  xlab('Property & Phase') + ylab('Mean percentage of property in phase') +
  ggtitle('Mean property distribution across phases (grouped by property)'))

# Plot: summary of property distribution across phases
print(ggplot(filter(phase_props_summary, is.finite(ratio.mean)), aes(x=prop, y=ratio.mean * 100)) +
  geom_bar(stat='identity', aes(fill=phase), position='stack') +
  labs(fill='Phase') +
  xlab('Property & Phase') + ylab('Mean percentage of property in phase') +
  ggtitle('Mean property distribution across phases (grouped by property)'))


# Compute: cumulative phase properties
sub <- sub %>% group_by(subject) %>%
  mutate(cum_cost   = cumsum(cost),
         cum_sales  = cumsum(sales),
         cum_grants = cumsum(grants),
         cum_time   = cumsum(time),
         cum_prob   = cumprod(prob))

# Transform: cumulative phase properties to long from wide
cum_phase_props <- sub %>%
  filter(phase != 'MP') %>%
  select(c(1, 2, cum_cost:cum_prob)) %>%
  gather(key='prop', value='value', cum_cost:cum_prob)

# Filter: Props with no values
cum_phase_props_no_zeroes <- cum_phase_props %>%
  group_by(prop) %>%
  filter(max(value) > 0)

# Plot: cumulative properties per phase (density plot)
ggplot(cum_phase_props_no_zeroes, aes(value, fill=phase)) +
  facet_wrap(prop ~ ., scale='free', ncol=1) +
  geom_density(alpha=0.75) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle('Cumulative properties by end of phase') +
  ylab('Frequency') + xlab(element_blank()) + labs(fill='Phase')

# Plot: cumulative properties per phase (violin plot)
print(ggplot(cum_phase_props_no_zeroes, aes(phase, value, fill=phase)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), alpha=0.75) +
  facet_wrap(prop ~ ., scale='free', ncol=1) +
  ggtitle('Cumulative properties by end of phase') +
  xlab('Phase') + ylab(element_blank()) + labs(fill='Phase'))


