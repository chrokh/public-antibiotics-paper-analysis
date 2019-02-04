library(dplyr)
library(ggplot2)

# config
INPUT  <- 'output/data/treated_phases.csv'
OUTPUT <- 'output/plots/intervention_input_summary.pdf'


# I/O
phases <- read.csv(INPUT)
pdf(OUTPUT)


# Convert phases to ordered factor
phase_levels <- c('PC','P1','P2','P3','P4','MP')
phases$phase <- factor(phases$phase, levels=phase_levels, ordered=TRUE)

# Convert intervention to factor
intervention_levels <- c('NONE', 'PCER', 'P1ER', 'P2ER', 'P3ER', 'P4ER', 'PDMER', 'FDMER')
phases$intervention <- factor(phases$intervention, levels=intervention_levels)

# Separate control and treatment group
control <- filter(phases, intervention == 'NONE') %>% arrange(subject, phase)
treated <- filter(phases, intervention != 'NONE') %>% arrange(subject, phase)

# Compute diff(s) introduced by treatment
diffs <- treated %>% group_by(intervention) %>%
  arrange(subject, phase) %>%
  transmute(phase   = phase,
            time    = time - control$time,
            cost    = cost - control$cost,
            revenue = revenue - control$revenue,
            prob    = prob - control$prob,
            discount.rate = discount.rate - control$discount.rate)


# Only plot where there is a difference
ggplot(filter(diffs, revenue>0), aes(revenue,
                                     fill=phase)) +
                                     #fill=interaction(phase, intervention))) +
geom_histogram() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank()) +
  facet_wrap(interaction(intervention, phase)~., scale='free_y') +
  ggtitle('Difference in input phase parameters caused by intervention')
