set.seed(1)
N = 1000

# sample discount rate
discount.rates <- runif(N, 0.09, 0.24)

# sample costs
p0.cost <- runif(N, min=19, max=23.2)
p1.cost <- runif(N, min=7.3, max=12)
p2.cost <- runif(N, min=7.12, max=18.72)
p3.cost <- runif(N, min=26.88, max=121.68)
p4.cost <- rep(98.297168, N)
p5.cost <- rep(10, N)

# sample probabilities
p0.probs <- runif(N, min=0.175, max=0.69)
p1.probs <- runif(N, min=0.25, max=0.837)
p2.probs <- runif(N, min=0.34, max=0.74)
p3.probs <- runif(N, min=0.314, max=0.786)
p4.probs <- runif(N, min=0.83, max=0.99)
p5.probs <- rep(1, N)


