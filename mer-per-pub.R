set.seed(2)
N     = 3
YEARS = 25

# Create dataset for sampling
obs <- data.frame(subject=1:N)

# Sample: Pre-clinical
obs$pc.time          <- runif(N, min=4.3, max=6)
obs$p1.time          <- runif(N, min=0.75, max=1.8)
obs$p2.time          <- runif(N, min=0.75, max=2.5)
obs$p3.time          <- runif(N, min=0.83, max=3.9)
obs$p4.time          <- runif(N, min=0.5, max=1.04)
obs$m.time           <- 10

obs$pc.prob          <- runif(N, min=0.175, max=0.69)
obs$p1.prob          <- runif(N, min=0.25, max=0.837)
obs$p2.prob          <- runif(N, min=0.34, max=0.74)
obs$p3.prob          <- runif(N, min=0.314, max=0.786)
obs$p4.prob          <- runif(N, min=0.83, max=0.99)
obs$m.prob           <- 1

obs$pc.cost          <- runif(N, min=19, max=23.2)
obs$p1.cost          <- runif(N, min=7.3, max=12)
obs$p2.cost          <- runif(N, min=7.12, max=18.72)
obs$p3.cost          <- runif(N, min=26.88, max=121.68)
obs$p4.cost          <- rep(98.297168, N)
obs$m.cost           <- 0

obs$pc.revenue       <- rep(0, N)
obs$p1.revenue       <- rep(0, N)
obs$p2.revenue       <- rep(0, N)
obs$p3.revenue       <- rep(0, N)
obs$p4.revenue       <- rep(0, N)
obs$m.revenue        <- runif(N, min=218, max=2500)

discount.rate <- runif(N, 0.09, 0.24)
obs$pc.discount.rate <- discount.rate
obs$p1.discount.rate <- discount.rate
obs$p2.discount.rate <- discount.rate
obs$p3.discount.rate <- discount.rate
obs$p4.discount.rate <- discount.rate
obs$m.discount.rate  <- discount.rate

# Compute: Time to phase
obs$pc.timeto        <- 0
obs$p1.timeto        <- obs$pc.timeto + obs$pc.time
obs$p2.timeto        <- obs$p1.timeto + obs$p1.time
obs$p3.timeto        <- obs$p2.timeto + obs$p2.time
obs$p4.timeto        <- obs$p3.timeto + obs$p3.time
obs$m.timeto         <- obs$p4.timeto + obs$p4.time

# Compute: Probability
obs$m.prob.remaining  <- 1
obs$p4.prob.remaining <- obs$m.prob.remaining  * obs$p4.prob
obs$p3.prob.remaining <- obs$p4.prob.remaining * obs$p3.prob
obs$p2.prob.remaining <- obs$p3.prob.remaining * obs$p2.prob
obs$p1.prob.remaining <- obs$p2.prob.remaining * obs$p1.prob
obs$pc.prob.remaining <- obs$p1.prob.remaining * obs$pc.prob


# Make prop specific data frames
cost    <- data.frame(pc=obs$pc.cost, p1=obs$p1.cost, p2=obs$p2.cost, p3=obs$p3.cost, p4=obs$p4.cost)
prob    <- data.frame(pc=obs$pc.prob, p1=obs$p1.prob, p2=obs$p2.prob, p3=obs$p3.prob, p4=obs$p4.prob)
time    <- data.frame(pc=obs$pc.time, p1=obs$p1.time, p2=obs$p2.time, p3=obs$p3.time, p4=obs$p4.time)
revenue <- data.frame(pc=obs$pc.revenue, p1=obs$p1.revenue, p2=obs$p2.revenue, p3=obs$p3.revenue, p4=obs$p4.revenue)
timeto  <- data.frame(pc=obs$pc.timeto, p1=obs$p1.timeto, p2=obs$p2.timeto, p3=obs$p3.timeto, p4=obs$p4.timeto)

# Compute: Market slope
obs$m.revenue.y1 <- 0
obs$m.revenue.y2 <- obs$m.revenue * 2 / (obs$m.time + 1)  # compute pys
obs$m.revenue.m  <- obs$m.revenue.y2 / obs$m.time         # compute slope


# Summarize
boxplot(cost, ylab='USD (million)', main='Phase cost', las=1)
boxplot(prob, ylab='Probability', main='Technical probability of phase completion', las=1)
boxplot(time, ylab='Months', main='Phase duration', las=1)


# Helper funtion: Splitting prop over years uniformly
prop.at.year <- function(n, prop, time, timeto) {
# TODO: Does not work for prob cuz of product rather than sum!
  unit      <- prop / time
  ongoing   <- n <= floor(time + timeto) & n >= timeto
  last      <- n == ceiling(timeto + time)
  remainder <- prop - unit * floor(time)
  (ongoing * unit) + (last * remainder)
}

# Helper funtion: Splitting sales over years linearly
sales.at.year <- function(yr) {
  myr <- yr - obs$m.timeto
  ifelse(myr <= obs$m.time & myr >= 0, obs$m.revenue.m * myr, 0)
}


# Transform: Phasely to yearly
yearly <- data.frame()
for (year in 1:YEARS) {
  yr  <- rep(year, N)
  cst <- apply(prop.at.year(year, cost, time, timeto), 1, sum)
  rvn <- apply(prop.at.year(year, revenue, time, timeto), 1, sum)
  sls <- sales.at.year(year)
  yearly <- rbind(yearly, data.frame(subject=1:N, year=yr, cost=cst, revenue=rvn, sales=sls))
}

# Compute: yearly cashflow
yearly$cashflow <- yearly$revenue + yearly$sales - yearly$cost


# Plot: Yearly data
boxplot(yearly$cost ~ yearly$year, xlab='Year', ylab='USD (million)', las=2, main='Cost per year')
boxplot(yearly$revenue ~ yearly$year, xlab='Year', ylab='USD (million)', las=2, main='Grants per year')
boxplot(yearly$sales ~ yearly$year, xlab='Year', ylab='USD (million)', las=2, main='Sales per year')
boxplot(yearly$cashflow ~ yearly$year, xlab='Year', ylab='USD (million)', las=2, main='Cashflow per year')

