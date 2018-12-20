set.seed(1)
N = 3

# Sample discount rate
discount.rates <- runif(N, 0.09, 0.24)

# Sample durations
pc.time  <- runif(N, min=4.3, max=6)
p1.time  <- runif(N, min=0.75, max=1.8)
p2.time  <- runif(N, min=0.75, max=2.5)
p3.time  <- runif(N, min=0.83, max=3.9)
p4.time  <- runif(N, min=0.5, max=1.04)

# Sample probabilities
pc.prob <- runif(N, min=0.175, max=0.69)
p1.prob <- runif(N, min=0.25, max=0.837)
p2.prob <- runif(N, min=0.34, max=0.74)
p3.prob <- runif(N, min=0.314, max=0.786)
p4.prob <- runif(N, min=0.83, max=0.99)

# Sample costs
pc.cost <- runif(N, min=19, max=23.2)
p1.cost <- runif(N, min=7.3, max=12)
p2.cost <- runif(N, min=7.12, max=18.72)
p3.cost <- runif(N, min=26.88, max=121.68)
p4.cost <- rep(98.297168, N)
m1.cost <- rep(10, N)

# Sample revenues
pc.revenue <- rep(0, N)
p1.revenue <- rep(0, N)
p2.revenue <- rep(0, N)
p3.revenue <- rep(0, N)
p4.revenue <- rep(0, N)
p5.revenue <- rep(0, N)
m1.revenue  <- runif(N, min=0, max=100) # TODO: data
m2.revenue  <- runif(N, min=0, max=100) # TODO: data
m3.revenue  <- runif(N, min=0, max=100) # TODO: data
m4.revenue  <- runif(N, min=0, max=100) # TODO: data
m5.revenue  <- runif(N, min=0, max=100) # TODO: data
m6.revenue  <- runif(N, min=0, max=100) # TODO: data
m7.revenue  <- runif(N, min=0, max=100) # TODO: data
m8.revenue  <- runif(N, min=0, max=100) # TODO: data
m9.revenue  <- runif(N, min=0, max=100) # TODO: data
m10.revenue <- runif(N, min=0, max=100) # TODO: data
g1.revenue  <- m10.revenue * 0.5
g2.revenue  <- m10.revenue * 0.5
g3.revenue  <- m10.revenue * 0.5
g4.revenue  <- m10.revenue * 0.5
g5.revenue  <- m10.revenue * 0.5
g6.revenue  <- m10.revenue * 0.5
g7.revenue  <- m10.revenue * 0.5
g8.revenue  <- m10.revenue * 0.5
g9.revenue  <- m10.revenue * 0.5
g10.revenue <- m10.revenue * 0.5

# Compute timeto
pc.timeto <- 0
p1.timeto <- pc.timeto + pc.time
p2.timeto <- p1.timeto + p1.time
p3.timeto <- p2.timeto + p2.time
p4.timeto <- p3.timeto + p3.time
p5.timeto <- p4.timeto + p4.time


# Create dataframe
# df <- data.frame(
#   pc.time, p1.time, p2.time, p3.time, p4.time, p5.time, m1.time,
#   pc.prob, p1.prob, p2.prob, p3.prob, p4.prob, p5.prob,
#   pc.cost, p1.cost, p2.cost, p3.cost, p4.cost, p5.cost, m1.cost,
#   pc.revenue, p1.revenue, p2.revenue, p3.revenue, p4.revenue, p5.revenue, m1.revenue
# )

# Summarize
boxplot(pc.cost, p1.cost, p2.cost, p3.cost, p4.cost, p5.cost, m1.cost, ylab='cost', las=2, main='Cost')
axis(1, at=seq(1,7), labels=seq(0,6))
boxplot(pc.prob, p1.prob, p2.prob, p3.prob, p4.prob, p5.prob, ylab='prob', las=2, main='Prob')
axis(1, at=seq(1,7), labels=seq(0,6))
boxplot(pc.time, p1.time, p2.time, p3.time, p4.time, p5.time, m1.time, ylab='time', las=2, main='Time')
axis(1, at=seq(1,7), labels=seq(0,6))

# WORKING HERE
test <- function(n, x) {
  ifelse(
    (
      n < floor(x$time + x$timeto)
      & n > x$timeto
    ),
    x$cost / floor(x$time),
    ifelse(
      (
        (n - x$timeto - x$time) > -1
        & (n - x$timeto - x$time) < 0
      ),
      x$cost - x$cost / x$time * floor(x$time),
      0)
  )
}

pc <- data.frame(pc.time, pc.cost, pc.prob, pc.revenue, pc.timeto)
names(pc) <- c('time', 'cost', 'prob', 'revenue', 'timeto')

p1 <- data.frame(p1.time, p1.cost, p1.prob, p1.revenue, p1.timeto)
names(p1) <- c('time', 'cost', 'prob', 'revenue', 'timeto')
# TODO: Structure as df's from the start

test(1, pc)
test(1, p1)

test(3, pc)
test(3, p1)

test(5, pc)
test(5, p1)
