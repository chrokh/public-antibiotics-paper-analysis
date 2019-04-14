set.seed(1)

source('src/simulate/sample.R', chdir=TRUE)
phases <- sample(1000)
write.csv(phases, row.names=FALSE, file='output/data/phases.csv')

source('src/simulate/intervene.R', chdir=TRUE)
intervened <- intervene(phases)
write.csv(intervened, row.names=FALSE, file='output/data/treated_phases.csv')

source('src/simulate/to_years.R', chdir=TRUE)
years <- to_years(intervened)
write.csv(years, row.names=FALSE, file='output/data/years.csv')
