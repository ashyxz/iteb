library(ggplot2)
library(Lahman)
library(dplyr)
library(reshape2)
theme_set(theme_bw())

## The Beta Distribution

## Fig. 2.1 Code
#  Beta distribution with various parameters
x <- seq(0,1, length=200)
beta_dist <- data.frame(
  cbind(
    x,
    dbeta(x, 1, 2),
    dbeta(x, 3, 3),
    dbeta(x, 20, 20),
    dbeta(x, 50, 10)
  )
)

colnames(beta_dist) <- c(
  "x",
  "alpha = 1, beta = 2",
  "alpha = 3, beta = 3",
  "alpha = 20, beta = 20",
  "alpha = 50, beta = 10"
)

# Creates a new data structure
beta_dist <- melt(beta_dist, x)

beta_dist %>%
  ggplot(aes(x, value, color=variable)) +
  geom_line() +
  labs(
    x = "Batting average",
    y = "Density of beta",
    color = "Parameters")

## Fig. 2.2 Code
# Sample batting average est using beta distribution
# Using params alpha = 81, beta = 219
# We only know for now they were chosen to be realistic mean / variance for BA's
beta_dist <- data.frame(
  cbind(
    x,
    dbeta(x, 81, 219)
  )
)

colnames(beta_dist) <- c(
  "x",
  "alpha = 81, beta = 219"
)

# melt the new ds
beta_dist <- melt(beta_dist, x)

beta_dist %>%
  ggplot(aes(x, value, color=variable)) +
  geom_line() +
  xlim(0.15, 0.5) + ## 'Realistic BAs'
  labs(title = "Sample Batting Average") +
  labs(
    x = "Batting average",
    y = "Density of beta",
    color = "Parameters")

## Fig. 2.3 Code
#  Updating our prior distribution
#  after 1 hit, 1 ab --> Beta(alpha = a1 + hits, beta = b1 + misses)
beta_dist <- data.frame(
  cbind(
    x,
    dbeta(x, 81, 219),
    dbeta(x, 82, 219),
    dbeta(x, 181, 419)
  )
)

colnames(beta_dist) <- c(
  "x",
  "alpha = 81, beta = 219",
  "alpha = 82, beta = 219",
  "alpha = 181, beta = 419"
)

beta_dist <- melt(beta_dist, x)

beta_dist %>%
  ggplot(aes(x, value, color=variable)) +
  geom_line() +
  xlim(0, 0.5) + ## 'Realistic BAs'
  labs(
    x = "Batting average",
    y = "Density of beta",
    color = "Parameters")

## Fig. 2.4 Code
# Histogram of true batting average of all the players who got exactly 100 hits.
# Shown in red is the density of Beta(81 + 100, 219 + 200) --> 100 H / 300 AB
beta_dist <- data.frame(
  cbind(
    x,
    dbeta(x, 181, 419)
  )
)

colnames(beta_dist) <- c(
  "x",
  "alpha = 181, beta = 419"
)

beta_dist <- melt(beta_dist, x)

num_trials <- 10e6

simulations <- tibble(
  true_average = rbeta(num_trials, 81, 219),
  hits = rbinom(num_trials, 300, true_average)
)

hit_100 <- simulations %>%
  filter(hits == 100)

hit_100 %>%
  ggplot(aes(true_average)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.005) + # 10 bins between 0.20 and 0.25
  geom_line(data = beta_dist,
            aes(x = x, y = value, color = "red"),
            show.legend = FALSE) +
  xlim(0.225, 0.375) +
  labs(x = "Batting average of players who got 100 H / 300 AB")

## Fig. 2.5 Code
#  True avg of players with H hits / 300 at-bats

simulations %>%
  filter(hits %in% c(60, 80, 100)) %>%
  ggplot(aes(true_average, color = factor(hits))) +
  geom_density() +
  labs(
    x = "True average of players with H hits / 300 at-bats",
    color = "H"
    )
