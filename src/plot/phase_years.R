library(dplyr)
library(ggplot2)
library(gridExtra)

# Config
INPUT  <- 'output/data/years.csv'
OUTPUT <- 'output/plots/phase_years.pdf'


# I/O
phase_years <- read.csv(INPUT)
pdf(OUTPUT)


# Convert phases to ordered factor
phase_levels <- c('PC','P1','P2','P3','P4','MP')
phase_years$phase <- factor(phase_years$phase, levels=phase_levels, ordered=TRUE)



# Plot: Properties per phase year of different phases
ggplot(phase_years, aes(as.factor(phase.year), cost, fill=phase)) +
  geom_boxplot(alpha=0.6) +
  facet_wrap(.~phase, scale='free', nrow=2) +
  xlab('Phase year (note: phase duration vary across projects and phases)') +
  theme(legend.position='none') +
  ggtitle('Cost of year in phase')
ggplot(phase_years, aes(as.factor(phase.year), revenue, fill=phase)) +
  geom_boxplot(alpha=0.6) +
  facet_wrap(.~phase, scale='free', nrow=2) +
  xlab('Phase year (note: phase duration vary across projects and phases)') +
  theme(legend.position='none') +
  ggtitle('Revenue of year in phase')
ggplot(phase_years, aes(as.factor(phase.year), prob, fill=phase)) +
  geom_boxplot(alpha=0.6) +
  facet_wrap(.~phase, scale='free', nrow=2) +
  xlab('Phase year (note: phase duration vary across projects and phases)') +
  theme(legend.position='none') +
  ggtitle('Probability of success of year in phase')

# Summarize: phase years
phase_years_summary <- phase_years %>%
  group_by(phase, phase.year) %>%
  summarize(cost.mean    = mean(cost),
            prob.mean    = mean(prob),
            revenue.mean = mean(revenue))

# Plot: Mean properties per phase year of different phases
p1 <- ggplot(phase_years_summary, aes(phase.year, cost.mean, color=phase)) +
  geom_line() + geom_point() + ggtitle('Mean cost of year in phase')
p2 <- ggplot(phase_years_summary, aes(phase.year, revenue.mean, color=phase)) +
  geom_line() + geom_point() + ggtitle('Mean revenue of year in phase')
p3 <- ggplot(phase_years_summary, aes(phase.year, prob.mean, color=phase)) +
  geom_line() + geom_point() + ggtitle('Mean probability of success of year in phase')
grid.arrange(p1, p2, p3, ncol=1)
