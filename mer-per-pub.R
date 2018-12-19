set.seed(1)
N = 500000

# Sample discount rate
discount.rates <- runif(N, 0.09, 0.24)

# Sample durations
p0.time <- runif(N, min=4.3, max=6)
p1.time <- runif(N, min=0.75, max=1.8)
p2.time <- runif(N, min=0.75, max=2.5)
p3.time <- runif(N, min=0.83, max=3.9)
p4.time <- runif(N, min=0.5, max=1.04)
p5.time <- rep(0, N)
p6.time <- rep(10, N)

# Sample probabilities
p0.prob <- runif(N, min=0.175, max=0.69)
p1.prob <- runif(N, min=0.25, max=0.837)
p2.prob <- runif(N, min=0.34, max=0.74)
p3.prob <- runif(N, min=0.314, max=0.786)
p4.prob <- runif(N, min=0.83, max=0.99)
p5.prob <- rep(1, N)
p6.prob <- rep(1, N)
p17.prob <- 0.5 * # should actually be triangular

# Sample costs
p0.cost <- runif(N, min=19, max=23.2)
p1.cost <- runif(N, min=7.3, max=12)
p2.cost <- runif(N, min=7.12, max=18.72)
p3.cost <- runif(N, min=26.88, max=121.68)
p4.cost <- rep(98.297168, N)
p5.cost <- rep(10, N)
p6.cost <- rep(0, N)

# Sample revenues
p0.revenue <- rep(0, N)
p1.revenue <- rep(0, N)
p2.revenue <- rep(0, N)
p3.revenue <- rep(0, N)
p4.revenue <- rep(0, N)
p5.revenue <- rep(0, N)
p6.revenue <- runif(N, min=218, max=2500)

# Create dataframe
df <- data.frame(
  p0.time, p1.time, p2.time, p3.time, p4.time, p5.time, p6.time,
  p0.prob, p1.prob, p2.prob, p3.prob, p4.prob, p5.prob, p6.prob,
  p0.cost, p1.cost, p2.cost, p3.cost, p4.cost, p5.cost, p6.cost,
  p0.revenue, p1.revenue, p2.revenue, p3.revenue, p4.revenue, p5.revenue, p6.revenue
  )

# Summarize
boxplot(p0.cost, p1.cost, p2.cost, p3.cost, p4.cost, p5.cost, p6.cost, ylab='cost', las=2)
axis(1, at=seq(1,7), labels=seq(0,6))
boxplot(p0.prob, p1.prob, p2.prob, p3.prob, p4.prob, p5.prob, p6.prob, ylab='prob', las=2)
axis(1, at=seq(1,7), labels=seq(0,6))
boxplot(p0.time, p1.time, p2.time, p3.time, p4.time, p5.time, p6.time, ylab='time', las=2)
axis(1, at=seq(1,7), labels=seq(0,6))

