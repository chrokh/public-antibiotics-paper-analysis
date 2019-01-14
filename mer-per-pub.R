set.seed(2)
N = 3

# Sample: Discount rate
discount.rate <- runif(N, 0.09, 0.24)

# Sample: Pre-clinical
time    <- runif(N, min=4.3, max=6)
prob    <- runif(N, min=0.175, max=0.69)
cost    <- runif(N, min=19, max=23.2)
revenue <- rep(0, N)
pc      <- data.frame(time, cost, prob, revenue, discount.rate)
pc$timeto <- 0

# Sample: Phase 1
time    <- runif(N, min=0.75, max=1.8)
prob    <- runif(N, min=0.25, max=0.837)
cost    <- runif(N, min=7.3, max=12)
revenue <- rep(0, N)
p1      <- data.frame(time, cost, prob, revenue, discount.rate)
p1$timeto <- pc$timeto + pc$time

# Sample: Phase 2
time    <- runif(N, min=0.75, max=2.5)
prob    <- runif(N, min=0.34, max=0.74)
cost    <- runif(N, min=7.12, max=18.72)
revenue <- rep(0, N)
p2      <- data.frame(time, cost, prob, revenue, discount.rate)
p2$timeto <- p1$timeto + p1$time

# Sample: Phase 3
time    <- runif(N, min=0.83, max=3.9)
prob    <- runif(N, min=0.314, max=0.786)
cost    <- runif(N, min=26.88, max=121.68)
revenue <- rep(0, N)
p3      <- data.frame(time, cost, prob, revenue, discount.rate)
p3$timeto <- p2$timeto + p2$time

# Sample: Phase 4
time    <- runif(N, min=0.5, max=1.04)
prob    <- runif(N, min=0.83, max=0.99)
cost    <- rep(98.297168, N)
revenue <- rep(0, N)
p4      <- data.frame(time, cost, prob, revenue, discount.rate)
p4$timeto <- p3$timeto + p3$time


# Summarize
boxplot(pc$cost, p1$cost, p2$cost, p3$cost, p4$cost, ylab='cost', las=2, main='Cost')
axis(1, at=seq(1,7), labels=seq(0,6))
boxplot(pc$prob, p1$prob, p2$prob, p3$prob, p4$prob, ylab='prob', las=2, main='Prob')
axis(1, at=seq(1,7), labels=seq(0,6))
boxplot(pc$time, p1$time, p2$time, p3$time, p4$time, ylab='time', las=2, main='Time')
axis(1, at=seq(1,7), labels=seq(0,6))


# Make prop specific data frames
cost    <- data.frame(pc=pc$cost, p1=p1$cost, p2=p2$cost, p3=p3$cost, p4=p4$cost)
prob    <- data.frame(pc=pc$prob, p1=p1$prob, p2=p2$prob, p3=p3$prob, p4=p4$prob)
time    <- data.frame(pc=pc$time, p1=p1$time, p2=p2$time, p3=p3$time, p4=p4$time)
revenue <- data.frame(pc=pc$revenue, p1=p1$revenue, p2=p2$revenue, p3=p3$revenue, p4=p4$revenue)
timeto  <- data.frame(pc$timeto, p1$timeto, p2$timeto, p3$timeto, p4$timeto)


# TODO: Does not work for prob cuz of product rather than sum!
propAtYear <- function(n, prop, time, timeto) {
  unit      <- prop / time
  ongoing   <- n <= floor(time + timeto) & n >= timeto
  last      <- n == ceiling(timeto + time)
  remainder <- prop - unit * floor(time)
  (ongoing * unit) + (last * remainder)
}
sumPropAtYear <- function(n, prop, time, timeto) {
  apply(propAtYear(n, prop, time, timeto), 1, sum)
}
propPerYear <- function(start, years, prop, time, timeto) {
  df <- data.frame()
  for (year in start:(start+years-1)) {
    subject <- 1:nrow(prop)
    value   <- sumPropAtYear(year, prop, time, timeto)
    df <- rbind(df, data.frame(subject=subject, year=year, value=value))
  }
  df
}

# Plot: cost per year
cpy <- propPerYear(1, 14, cost, time, timeto)
boxplot(cpy$value ~ cpy$year, xlab='year', ylab='cost', las=1, main='Cost per year')

# Plot: revenue per year
rpy <- propPerYear(10, 21, revenue, time, timeto)
boxplot(rpy$value ~ rpy$year, xlab='year', ylab='revenue', las=1, main='Revenue per year')
