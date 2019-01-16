set.seed(2)
N     = 3
YEARS = 8

# Create dataset for sampling
obs <- data.frame(subject=1:N)

# Sample: Discount rate
discount.rate <- runif(N, 0.09, 0.24)

# Sample: Pre-clinical
obs$pc.time          <- runif(N, min=4.3, max=6)
obs$pc.prob          <- runif(N, min=0.175, max=0.69)
obs$pc.cost          <- runif(N, min=19, max=23.2)
obs$pc.revenue       <- rep(0, N)
obs$pc.timeto        <- 0
obs$pc.discount.rate <- discount.rate

# Sample: Phase 1
obs$p1.time          <- runif(N, min=0.75, max=1.8)
obs$p1.prob          <- runif(N, min=0.25, max=0.837)
obs$p1.cost          <- runif(N, min=7.3, max=12)
obs$p1.revenue       <- rep(0, N)
obs$p1.timeto        <- obs$pc.timeto + obs$pc.time
obs$p1.discount.rate <- discount.rate

# Sample: Phase 2
obs$p2.time          <- runif(N, min=0.75, max=2.5)
obs$p2.prob          <- runif(N, min=0.34, max=0.74)
obs$p2.cost          <- runif(N, min=7.12, max=18.72)
obs$p2.revenue       <- rep(0, N)
obs$p2.timeto        <- obs$p1.timeto + obs$p1.time
obs$p2.discount.rate <- discount.rate

# Sample: Phase 3
obs$p3.time          <- runif(N, min=0.83, max=3.9)
obs$p3.prob          <- runif(N, min=0.314, max=0.786)
obs$p3.cost          <- runif(N, min=26.88, max=121.68)
obs$p3.revenue       <- rep(0, N)
obs$p3.timeto        <- obs$p2.timeto + obs$p2.time
obs$p3.discount.rate <- discount.rate

# Sample: Phase 4
obs$p4.time          <- runif(N, min=0.5, max=1.04)
obs$p4.prob          <- runif(N, min=0.83, max=0.99)
obs$p4.cost          <- rep(98.297168, N)
obs$p4.revenue       <- rep(0, N)
obs$p4.timeto        <- obs$p3.timeto + obs$p3.time
obs$p4.discount.rate <- discount.rate

# Sample: Market
obs$m.time           <- 10
obs$m.prob           <- 1
obs$m.cost           <- 0
obs$m.revenue        <- runif(N, min=218, max=2500)
obs$m.discount.rate  <- discount.rate


# Make prop specific data frames
cost    <- data.frame(pc=obs$pc.cost, p1=obs$p1.cost, p2=obs$p2.cost, p3=obs$p3.cost, p4=obs$p4.cost)
prob    <- data.frame(pc=obs$pc.prob, p1=obs$p1.prob, p2=obs$p2.prob, p3=obs$p3.prob, p4=obs$p4.prob)
time    <- data.frame(pc=obs$pc.time, p1=obs$p1.time, p2=obs$p2.time, p3=obs$p3.time, p4=obs$p4.time)
revenue <- data.frame(pc=obs$pc.revenue, p1=obs$p1.revenue, p2=obs$p2.revenue, p3=obs$p3.revenue, p4=obs$p4.revenue)
timeto  <- data.frame(pc=obs$pc.timeto, p1=obs$p1.timeto, p2=obs$p2.timeto, p3=obs$p3.timeto, p4=obs$p4.timeto)


# Summarize
boxplot(cost, ylab='USD (million)', main='Phase cost', las=1)
boxplot(prob, ylab='Probability', main='Technical probability of phase completion', las=1)
boxplot(time, ylab='Months', main='Phase duration', las=1)


# Helper funtions
propAtYear <- function(n, prop, time, timeto) {
# TODO: Does not work for prob cuz of product rather than sum!
  unit      <- prop / time
  ongoing   <- n <= floor(time + timeto) & n >= timeto
  last      <- n == ceiling(timeto + time)
  remainder <- prop - unit * floor(time)
  (ongoing * unit) + (last * remainder)
}


# Compute: revenue per market year
rpmy <- data.frame()
obs$m.revenue.y1 <- 0
obs$m.revenue.y2 <- obs$m.revenue * 2 / (obs$m.time + 1)  # compute pys
obs$m.revenue.m  <- obs$m.revenue.y2 / obs$m.time         # compute slope
for (yr in 1:YEARS) {
  subject <- 1:nrow(obs)
  rev     <- ifelse(yr <= obs$m.time, obs$m.revenue.m * yr, 0)
  rpmy    <- rbind(rpmy, data.frame(subject=subject, year=yr, revenue=rev))
}

# Compute: cost per year
cpy <- data.frame()
for (year in 1:YEARS) {
  value <- apply(propAtYear(year, cost, time, timeto), 1, sum)
  cpy   <- rbind(cpy, data.frame(subject=1:nrow(cost), year=year, cost=value))
}

# Compute: revenue per year
rpy <- data.frame()
for (year in 1:YEARS) {
  value <- apply(propAtYear(year, revenue, time, timeto), 1, sum)
  rpy   <- rbind(rpy, data.frame(subject=1:nrow(revenue), year=year, revenue=value))
}


# Combine yearly data into single data frame
yearly <- cpy
yearly <- merge(yearly, rpy, by=c('year', 'subject'))
yearly$cashflow <- yearly$revenue - yearly$cost


# Plot: cost per year
boxplot(yearly$cost ~ yearly$year, xlab='Year', ylab='USD (million)', las=1, main='Cost per year')

# Plot: revenue per year
boxplot(yearly$revenue ~ yearly$year, xlab='Year', ylab='USD (million)', las=1, main='Revenue per year (excluding sales)')

# Plot: revenue per market year
boxplot(rpmy$revenue ~ rpmy$year, xlab='Market year', ylab='USD (million)', las=1, main='Revenue per market year')

# Plot: cashflow per year
boxplot(yearly$cashflow ~ yearly$year, xlab='Year', ylab='USD (million)', las=1, main='Cashflow per year')
