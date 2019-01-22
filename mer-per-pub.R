library(dplyr)

# Configure
set.seed(1)
N     = 3
YEARS = 25


# Create dataset for sampling
obs <- data.frame()

# Same across all phases
discount.rate <- runif(N, 0.09, 0.24)

# Sample: Pre Clinical
pc <- data.frame(subject=1:N, phase='PC', stage=1)
pc$time             <- runif(N, min=4.3, max=6)
pc$cost             <- runif(N, min=19, max=23.2)
pc$revenue          <- rep(0, N)
pc$prob             <- runif(N, min=0.175, max=0.69)
pc$discount.rate    <- discount.rate

# Sample: Phase 1
p1 <- data.frame(subject=1:N, phase='P1', stage=2)
p1$time             <- runif(N, min=0.75, max=1.8)
p1$prob             <- runif(N, min=0.25, max=0.837)
p1$cost             <- runif(N, min=7.3, max=12)
p1$revenue          <- rep(0, N)
p1$discount.rate    <- discount.rate

# Sample: Phase 2
p2 <- data.frame(subject=1:N, phase='P2', stage=3)
p2$time             <- runif(N, min=0.75, max=2.5)
p2$revenue          <- rep(0, N)
p2$cost             <- runif(N, min=7.12, max=18.72)
p2$prob             <- runif(N, min=0.34, max=0.74)
p2$discount.rate    <- discount.rate

# Sample: Phase 3
p3 <- data.frame(subject=1:N, phase='P3', stage=4)
p3$cost             <- runif(N, min=26.88, max=121.68)
p3$prob             <- runif(N, min=0.314, max=0.786)
p3$time             <- runif(N, min=0.83, max=3.9)
p3$revenue          <- rep(0, N)
p3$discount.rate    <- discount.rate

# Sample: Phase 4
p4 <- data.frame(subject=1:N, phase='P4', stage=5)
p4$time             <- runif(N, min=0.5, max=1.04)
p4$prob             <- runif(N, min=0.83, max=0.99)
p4$cost             <- rep(98.297168, N)
p4$revenue          <- rep(0, N)
p4$discount.rate    <- discount.rate

# Sample: Market sales
sales <- data.frame(subject=1:N, phase='MP', stage=6)
sales$prob          <- 1
sales$cost          <- 0
sales$revenue       <- runif(N, min=218, max=2500)
sales$time          <- 10
sales$discount.rate <- discount.rate

# Combine all phases into single dataset
obs <- rbind(obs, pc, p1, p2, p3, sales)
obs$phase <- factor(obs$phase, levels=c('PC','P1','P2','P3','P4','MP'))


# Compute: Cashflow
obs$cashflow <- obs$revenue - obs$cost


# Summarize
boxplot(obs$cost~obs$phase, ylab='USD (million)', main='Cost by phase', las=1)
boxplot(obs$revenue~obs$phase, ylab='USD (million)', main='Revenue by phase', las=1)
boxplot(obs$cashflow~obs$phase, ylab='USD (million)', main='Cashflow by phase', las=1)
boxplot(obs$prob~obs$phase, ylab='Probability', main='Technical probability of success by phase', las=1)
boxplot(obs$time~obs$phase, ylab='Months', main='Duration by phase', las=1)


# Compute: Time to phase
options(tibble.width = Inf) # Always print all tibble cols
obs <- obs %>% group_by(subject) %>%
  mutate(time.to = cumsum(time) - time,
         time.from = sum(time) - cumsum(time) + time)


# Compute: Cashflow slope and offset
# TODO: Separate market into separate dataset and then do the yearly calc for
# two datasets so that we don't have to deal with exceptions and instead simply
# assume that everything is either constantly or linearly distributed in the
# development and market data set respectively.
obs$cashflow.a <- ifelse(obs$phase=='MP', ((obs$cashflow * 2 / (obs$time + 1)) / obs$time), 0)
obs$cashflow.b <- ifelse(obs$phase=='MP', 0, (obs$cashflow / obs$time))
obs$cost.a     <- ifelse(obs$phase=='MP', ((obs$cost * 2 / (obs$time + 1)) / obs$time), 0)
obs$cost.b     <- ifelse(obs$phase=='MP', 0, (obs$cost / obs$time))
obs$revenue.a  <- ifelse(obs$phase=='MP', ((obs$revenue * 2 / (obs$time + 1)) / obs$time), 0)
obs$revenue.b  <- ifelse(obs$phase=='MP', 0, (obs$revenue / obs$time))
obs$prob.a     <- 0
obs$prob.b     <- obs$prob ^ (1/obs$time)

# Compute: Yearly cashflows per phase
cashflows <- data.frame()
for (x in 1:ceiling(max(obs$time) + 1)) {
  # Prepare
  t              <- x + obs$time.to
  within_phase   <- x <= obs$time
  not_whole_year <- x-obs$time>0 & x-obs$time<1
  # Compute: yearly
  base_cashflow      <- ifelse(within_phase, obs$cashflow.a * x + obs$cashflow.b, 0)
  remainder_cashflow <- ifelse(not_whole_year, (obs$time-floor(obs$time))*(obs$cashflow/obs$time), 0)
  base_cost          <- ifelse(within_phase, obs$cost.a * x + obs$cost.b, 0)
  remainder_cost     <- ifelse(not_whole_year, (obs$time-floor(obs$time))*(obs$cost/obs$time), 0)
  base_revenue       <- ifelse(within_phase, obs$revenue.a * x + obs$revenue.b, 0)
  remainder_revenue  <- ifelse(not_whole_year, (obs$time-floor(obs$time))*(obs$revenue/obs$time), 0)
  base_prob          <- ifelse(within_phase, obs$prob.a * x + obs$prob.b, 0)
  remainder_prob     <- ifelse(not_whole_year, obs$prob / ((obs$prob.a*x+obs$prob.b) ^ floor(obs$time)), 0)
  # NOTE: The remainder is computed by assuming that the value is evenly
  # distributed over the whole phase (i.e. constantly). If this is not true
  # then the remainder will be incorrectly computed. However, since we assume
  # that phase properties are indeed constantly distributed over the course of
  # the phase, it is not a problem for phase properties. While we do assume
  # that market sales grow linearly this is not a problem since there will be
  # no remainder given that we assume that all markets span 10 years. However,
  # please be very careful and make sure you get this right!!
  # NOTE: This computation is a bit odd since there will never be a base
  # cashflow and a remainder at the same time.
  cashflow  <- base_cashflow + remainder_cashflow
  cost      <- base_cost + remainder_cost
  revenue   <- base_revenue + remainder_revenue
  prob      <- base_prob + remainder_prob
  # Make data frame
  subject    <- obs$subject
  phase.year <- x
  phase      <- obs$phase
  df        <- data.frame(subject, phase.year, t, phase, cashflow, cost, revenue, prob)
  df        <- df[cashflow != 0 | prob > 0, ] # No need to keep years without cashflow
  cashflows <- rbind(cashflows, df)
}



# Transform: To yearly
cashflows$year <- floor(cashflows$t)
yearly <- cashflows %>% group_by(subject, year) %>%
  summarise(cashflow = sum(cashflow),
            cost     = sum(cost),
            revenue  = sum(revenue),
            prob     = prod(prob))


# Plot: Yearly data
boxplot(yearly$cost ~ yearly$year, xlab='Year', ylab='USD (million)', las=2, main='Cost per year')
boxplot(yearly$revenue ~ yearly$year, xlab='Year', ylab='USD (million)', las=2, main='Revenue per year')
boxplot(yearly$cashflow ~ yearly$year, xlab='Year', ylab='USD (million)', las=2, main='Cashflow per year')
boxplot(yearly$prob ~ yearly$year, xlab='Year', ylab='Probability of success', las=2, main='Technical probability of success, per year')


# Compute: Cumulative yearly properties
cumulative <- yearly %>% group_by(subject) %>%
  mutate(cum.cashflow = cumsum(cashflow),
         cum.revenue  = cumsum(revenue),
         cum.cost = cumsum(cost),
         cum.prob = cumprod(prob),
         rem.prob = prod(prob) / cumprod(prob), # TODO: I'm NOT sure this is correct!
         )

# Plot: Cumulative yearly data
boxplot(cumulative$cum.cost ~ cumulative$year, las=2, xlab='Year', ylab='USD (million)', main='Cumulative cost per year')
boxplot(cumulative$cum.revenue ~ cumulative$year, las=2, xlab='Year', ylab='USD (million)', main='Cumulative revenue per year')
boxplot(cumulative$cum.cashflow ~ cumulative$year, las=2, xlab='Year', ylab='USD (million)', main='Cumulative cashflow per year')
boxplot(cumulative$cum.prob ~ cumulative$year, las=2, xlab='Year', ylab='Probability', main='Probability of reaching year')
boxplot(cumulative$rem.prob ~ cumulative$year, las=2, xlab='Year', ylab='Probability', main='Remaining probability of success per year')

