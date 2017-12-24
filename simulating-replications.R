## Chapter 13: Simulating replications
#
#

# Setup
library(Lahman)
library(dplyr)
library(tidyr)
library(purrr)
library(ebbr)
library(broom)
library(ggplot2)
theme_set(theme_bw())

# Grab career batting avgs of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

# include bats (handedness) and "year" columns for later
career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB))

prior <- ebb_fit_prior(career, H, AB)
alpha0 <- tidy(prior)$alpha
beta0 <- tidy(prior)$beta

## Figure 13.1: Estimated hyperparameters alpha_0 and beta_0 and the mean
#               (a0 / (a0 + b0)) across 50 replications
#
set.seed(2017)

sim_replications <- career %>%
  crossing(replication = 1:50) %>%
  mutate(p = rbeta(n(), alpha0, beta0),
         H = rbinom(n(), AB, p))

sim_replication_models <- sim_replications %>%
  nest(-replication) %>%
  mutate(prior = map(data, ~ ebb_fit_prior(., H, AB)))

# estimation of hyperparameters
# error (i ran oom :( )
sim_replication_priors <- sim_replication_models %>%
  unnest(map(prior, tidy), .drop = TRUE)

## Figure 13.2: Comparison of the MSE on 50 replications, using either the
#               raw BA or the shrunked BA
#

sim_replication_au <- sim_replication_models %>%
  unnest(map2(prior, data, augment))
sim_replication_mse <- sim_replication_au %>%
  rename(Raw = .raw, Shrunken = .fitted) %>%
  gather(type, estimate, Raw, Shrunken) %>%
  group_by(type, replication) %>%
  summarize(mse = mean((estimate - p) ^ 2))

## Figure 13.3: Distribution of the coverage pr of a 95% credible interval
#               across simulations.
#

# code not shown

## Figure 13.4: Comparison of the level of the credible interval to the
#               fraction of players where the interval contains the true
#               value. Each line represents one replication of the simulation;
#               the red line represents x = y

sim_replication_intervals <- sim_replication_models %>%
  crossing(cred_level = c(seq(.5, .9, .05), .95)) %>%
  unnest(pmap(list(prior, data, cred_level = cred_level), augment)) %>%
  select(replication, cred_level, p, .low, .high)
             

## Figure 13.5: Comparison of the q-value threshold and the resulting FDR
#               Each line represents one replication of the simulation; the
#               red line represents x = y.



## Figure 13.6: Estimated hyperparameters a0, b0, and the mean (a0 / (a0 + b0))
#               across 50 replications for each sample size. One condition with
#               much higher a0 and b0 was removed for readability.

# nifty trick for sampling different numbers within each group
# randomly suffle with sample_frac(1), then filter
varying_size_sim <- career %>%
  select(-H) %>%
  crossing(size = c(30, 100, 300, 1000, 3000, 10000),
           replication = 1:50) %>%
  group_by(size, replication = TRUE) %>%
  filter(row_number() <= size) %>%
  ungroup()

varying_size_priors <- varying_size_sim %>%
  mutate(p = rbeta(n(), alpha0, beta0),
         H = rbinom(n(), AB, p)) %>%
  nest(-size, -replication) %>%
  mutate(prior = map(data, ~ ebb_fit_prior(., H, AB)))



## Figure 13.7: Distribution of mean squared error (MSE) of empirical Bayes estimates
#               across 50 replications, for simulated datasets of varying sizes.
#

## Figure 13.8: The coverage probabilities of 95% credible intervals, comparing 50
#               replications of each sample size. Cases where the coverage probability
#               is lower than we'd expect by chance are shown as red points.

## Figure 13.9: The relationship between the estimated value of a0 + b0 in a particular
#               replication and the resulting coverage probabilities. 95% shown as a
#               horizontal dashed line, and the true value of a0 + b0 is shown as a vertical
#               dotted line. Best fit lines are shown in blue.