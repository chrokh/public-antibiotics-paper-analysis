# This file generates a csv file of samples from distributions outlined here.

# ============ CONFIG ===============
N      = 100
OUTPUT = 'output/data/phases.csv'
SEED   = 1
#====================================


set.seed(1)
phases <- data.frame()


# ============ SERTKAYA ET. AL (2014) ===============

# NOTE: Does not model Sertkaya exactly.

# Same across all phases
discount.rate <- runif(N, 0.09, 0.24)

# Samples phases
pc <- data.frame(subject=1:N, phase=factor('PC'))
pc$time             <- runif(N, min=4.3, max=6)
pc$cost             <- runif(N, min=19, max=23.2)
pc$revenue          <- rep(0, N)
pc$prob             <- runif(N, min=0.175, max=0.69)
pc$discount.rate    <- discount.rate
p1 <- data.frame(subject=1:N, phase=factor('P1'))
p1$time             <- runif(N, min=0.75, max=1.8)
p1$prob             <- runif(N, min=0.25, max=0.837)
p1$cost             <- runif(N, min=7.3, max=12)
p1$revenue          <- rep(0, N)
p1$discount.rate    <- discount.rate
p2 <- data.frame(subject=1:N, phase=factor('P2'))
p2$time             <- runif(N, min=0.75, max=2.5)
p2$revenue          <- rep(0, N)
p2$cost             <- runif(N, min=7.12, max=18.72)
p2$prob             <- runif(N, min=0.34, max=0.74)
p2$discount.rate    <- discount.rate
p3 <- data.frame(subject=1:N, phase=factor('P3'))
p3$cost             <- runif(N, min=26.88, max=121.68)
p3$prob             <- runif(N, min=0.314, max=0.786)
p3$time             <- runif(N, min=0.83, max=3.9)
p3$revenue          <- rep(0, N)
p3$discount.rate    <- discount.rate
p4 <- data.frame(subject=1:N, phase=factor('P4'))
p4$time             <- runif(N, min=0.5, max=1.04)
p4$prob             <- runif(N, min=0.83, max=0.99)
p4$cost             <- rep(98.297168, N)
p4$revenue          <- rep(0, N)
p4$discount.rate    <- discount.rate
mp <- data.frame(subject=1:N, phase=factor('MP'))
mp$prob          <- 1
mp$cost          <- 0
mp$revenue       <- runif(N, min=218, max=2500)
mp$time          <- 10
mp$discount.rate <- discount.rate

# Combine all phases into single dataset
phases <- rbind(phases, pc, p1, p2, p3, p4, mp)

# ===================================================


# Write data
write.csv(phases, row.names=FALSE, file=OUTPUT)

