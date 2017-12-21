## Empirical Bayes estimation
##   Which of these two proportions is higher: 
##    4 out of 10, or 300 out of 1000?

library(dplyr)
library(tidyr)
library(Lahman)
library(stats4)
library(reshape2) # for melt function
library(ggplot2)
theme_set(theme_bw())

# Filter out pitchers
career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>% # summarize commonly follows group_by
  mutate(average = H / AB)

# Include names along with player IDs
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID") %>%
  dplyr::select(-playerID) # drop unneeded cols with minus (-)

## Figure 3.1 code
## A histogram of the batting avgs of all players
## with more than 500 at-bats

# filter by 500+ ABs
career_filtered <- career %>%
  filter(AB > 500)

# plot
career_filtered %>%
  ggplot(aes(average)) +
    geom_histogram(binwidth = 0.005) + # 10 bins between 0.20 and 0.25
    xlim(0.15, 0.35)

## Figure 3.2 
# log-likelihood function
ll <- function(alpha, beta) {
  x <- career_filtered$H
  total <- career_filtered$AB
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
}

# Maximum likelihood estimation (MLE)
m <- mle(ll, start = list(alpha = 1, beta = 10), method = "L-BFGS-B", lower = c(0.0001, .1))
ab <- coef(m)

alpha0 <- ab[1]
beta0 <- ab[2]

# Beta estimation
x <- seq(0, 1, length=200)
beta_dist <- data.frame(
  cbind(
    x,
    dbeta(x, alpha0, beta0)
  )
)

colnames(beta_dist) <- c(
  "x",
  "a = 102.04, b = 289.80"
)

# melt the new ds
beta_dist <- melt(beta_dist, x)

# plot
career_filtered %>%
  ggplot(aes(average)) +
  geom_histogram(aes(y = ..density..)) +
  geom_line(
    data = beta_dist,
    aes(x, value),
    color = "red") +
  xlim(0.15, 0.35) +
  labs(x = "Batting average")


## Figure 3.3 A comparison between raw batting 
##  average and empirical Bayes estimate for all batters

# 1. Scatter plot of player's EB estimated BA and their actual BA
#     BONUS: 3rd dimension of gradient with more ABs being lighter shade

# First we neeb_estimated to perform the EB estimate calculation for each player
#  and save it in a new column
career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))

career_eb %>%
  ggplot(aes(average, eb_estimate, color = AB)) +
  geom_point() +
  geom_abline(color = "red") +
  scale_color_continuous(
    trans = "log", breaks = c(1, 10, 100, 1000)) + # shade points by AB value (lighter is more)
  geom_hline(
    yintercept = 0.261, # y = alpha0/alpha0+beta0
    color = "red",
    lty = 2) +
  labs(
    x = "Raw batting average",
    y = "Empirical Bayes baatting average")