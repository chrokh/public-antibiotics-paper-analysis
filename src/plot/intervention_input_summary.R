library(dplyr)
library(ggplot2)

# config
INPUT  <- 'treated_phases.csv'
OUTPUT <- 'intervention_input_summary.pdf'


# I/O
phases <- read.csv(INPUT)
pdf(OUTPUT)

# Factors
PHASE_LEVELS <- c('PC','P1','P2','P3','P4','MP')
INTERVENTION_LEVELS <- c('NONE', 'P1ER', 'P2ER', 'P3ER', 'P4ER', 'PDMER')

# Convert factors to factors
phases$phase <- factor(phases$phase, levels=PHASE_LEVELS, ordered=TRUE)
phases$intervention <- factor(phases$intervention, levels=INTERVENTION_LEVELS)

# Separate control and treatment group
control <- filter(phases, intervention == 'NONE') %>% arrange(subject, phase)
treated <- filter(phases, intervention != 'NONE') %>% arrange(subject, phase)

# Compute diff(s) introduced by treatment
diffs <- treated %>% group_by(intervention) %>%
  arrange(subject, phase) %>%
  transmute(phase   = phase,
            time    = time - control$time,
            cost    = cost - control$cost,
            sales   = sales - control$sales,
            prizes  = prizes - control$prizes,
            prob    = prob - control$prob,
            discount.rate = discount.rate - control$discount.rate)


# Only plot where there is a difference
ggplot(filter(diffs, prizes>0), aes(prizes, fill=phase)) +
geom_histogram() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank()) +
  facet_wrap(interaction(intervention, phase)~., scale='free_y') +
  ggtitle('Difference in input phase parameters caused by intervention')
