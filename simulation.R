#
## Chapter 12: Simulation
#
#

# Setup
library(Lahman)
library(dplyr)
library(tidyr)
library(ebbr)
library(purrr)
library(ggplot2)
theme_set(theme_bw())

# Grab career batting average of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

# include the "bats" (handedness) and "year" column for later
career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB))

## Figure 12.1: Distribution of AB, the number of ABs, across
#               all players (note the log x axis)
#
prior <- career %>%
  ebb_fit_prior(H, AB)

alpha0 <- tidy(prior)$alpha
beta0 <- tidy(prior)$beta

career %>%
  ggplot(aes(AB)) +
  geom_histogram() +
  scale_x_log10()

## Figure 12.2: The relationship between the true BA P and
#               either the raw BA (H / AB), or the shrunken
#               estimate P_i

# Emprical Bayes estimation
set.seed(2017)

career_sim <- career %>%
  mutate(p = rbeta(n(), alpha0, beta0),
         H = rbinom(n(), AB, p))

career_sim_eb <- career_sim %>%
  add_ebb_estimate(H, AB)

career_sim_gathered <- career_sim_eb %>%
  rename(Shrunken = .fitted, Raw = .raw) %>%
  gather(type, estimate, Shrunken, Raw)

career_sim_gathered %>%
  filter(AB >= 10) %>%
  ggplot(aes(p, estimate, color = AB)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = "lm", color = "white", lty = 2, se = FALSE) +
  scale_color_continuous(trans = "log",
                         breaks = c(10,100,1000,10000)) +
  facet_wrap(~ type) +
  labs(x = "True batting average (p)",
       y = "Raw or shrunken batting average")

## Figure 12.3: Mean-squared error within bins of AB, using
#               either the raw average or the shrunked estimate.
#               Note that both axes are on a log scale.

career_sim_gathered %>%
  group_by(type) %>%
  summarize(mse = mean((estimate - p) ^ 2))

metric_by_bin <- career_sim_gathered %>%
  group_by(type, AB = 10 ^ (round(log10(AB)))) %>%
  summarize(mse = mean((estimate - p) ^ 2))

metric_by_bin %>%
  ggplot(aes(AB, mse, color = type)) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Number of at-bats (AB)",
       y = "Mean-squared-error within this bin")

#
## Figure 12.4: Bias within bins of AB, using either the raw average
#               or the shrunked estimate. Note that an unbiased estimate
#               would have a slope of 0 (shown as horizontal line)

# Error.
career_sim_gathered %>%
  mutate(AB = 10 ^ (round(log10(AB)))) %>%
  filter(AB > 1) %>%
  nest(-type, -AB) %>%
  unnest(map(data, ~ tidy(lm(estimate ~ p, .)))) %>%
  filter(term == "p") %>%
  ggplot(aes(AB, estimate, color = type)) +
  geom_line() +
  scale_x_log10(breaks = c(10, 100, 1000, 10000)) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = "Number of at-bats (AB)",
       y = "Slope of estimate/p within this bin")

#
## Figure 12.5: The relationship between the true BA P and either the
#               raw BA H/AB, or the shrunken estimate P_i, within
#               particular bins of AB.
career_sim_gathered %>%
  mutate(ab_bin = cut(AB, c(0, 10, 100, 1000, Inf),
                      labels = c("1-10", "11-100", "101-1000", "1000+"))) %>%
  ggplot(aes(p, estimate, color = AB)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = "lm", color = "white", lty = 2, se = FALSE) +
  scale_color_continuous(trans = "log", breaks = c(10, 100, 1000, 10000)) +
  facet_grid(ab_bin ~ type, scales = "free_y") +
  labs(x = "True batting average (p)",
       y = "Raw or shrunken estimate")


#
## Figure 12.6: Credible intervals for 20 randomly selected players, with
#               the true BA of each player shown in red.
#

# using credible intervals calculated by the add_ebb_estimate fn
set.seed(2017)

career_sim_eb %>%
  sample_n(20) %>%
  mutate(playerID = reorder(playerID, .fitted)) %>%
  ggplot(aes(.fitted, playerID)) +
  geom_point() +
  geom_point(aes(x = p), color = "red") +
  geom_errorbarh(aes(xmin = .low, xmax = .high)) +
  theme(axis.text.y = element_blank()) +
  labs(x = "Estimated batting average (w/ 95% credible interval)",
       y = "Player")

## we can examine this across all players and see that 95% of the intervals
#   contained the true probability.
career_sim_eb %>%
  summarize(coverage = mean(.low <= p & p <= .high))

#
## Figure 12.7: Comparison of the level of the credibility interval to the
#               percentage of players where the credible interval contains
#               the true value.

# Does the probability that the parameter is contained within the interval
#  change accordingly along with the level
sim_prior <- ebb_fit_prior(career_sim, H, AB)

# find the coverage probability for each level

# Error: level not found
estimate_by_cred_level <- tibble(level = seq(.5, .98, .02)) %>%
  unnest(map(level, ~ augment(sim_prior, career_sim, cred_level = .)))
estimate_by_cred_level %>%
  group_by(level) %>%
  mutate(cover = .low <= p & p <= .high) %>%
  summarize(coverage = mean(cover)) %>%
  ggplot(aes(level, coverage)) +
  geom_line() +
  geom_abline(color = "red", lty = 2) +
  labs(x = "Level of credible interval",
       y = "Probability credible interval contains the true value")


## Figure 12.8: Comparison of the q-value threshold, meant to control False
#               discovery rate, and the true FDR, defined as the number of
#               players included where p < 3. The red line is x = y

pt <- career_sim_eb %>%
  add_ebb_prop_test(0.3, sort = TRUE)

# Control for FDR of 10%
hall_of_fame <- pt %>%
  filter(.qvalue <= .1)

pt %>%
  mutate(true_fdr = cummean(p < 0.3)) %>%
  ggplot(aes(.qvalue, true_fdr)) +
  geom_line() +
  geom_abline(color = "red") +
  labs(x = "q-value threshold",
       y = "True FDR at this q-value threshold")


## Figure 12.9: Comparison of the true BA and the shrunken BA, using either
#               a single beta as the prior or a prior where P depends on AB.
#

# Beta-binomial regression
bb_reg <- career %>%
  ebb_fit_prior(H, AB, method = "gamlss", mu_predictors = ~ log10(AB))

tidy(bb_reg)

set.seed(2017)

career_sim_ab <- augment(bb_reg, career) %>%
  select(playerID, AB,
         true_alpha0 = .alpha0,
         true_beta0 = .beta0) %>%
  mutate(p = rbeta(n(), true_alpha0, true_beta0),
         H = rbinom(n(), AB, p))

career_ab_prior <- career_sim_ab %>%
  ebb_fit_prior(H, AB, method = "gamlss", mu_predictors = ~ log10(AB))

career_flat_prior <- career_sim_ab %>%
  ebb_fit_prior(H, AB)


# Error again in the line unnest(map(model, augment, data = career_sim_ab))
thing <- tibble(method = c("Flat Prior", "Prior depending on AB"),
                model = list(career_flat_prior, career_ab_prior))

thing2 <-
  unnest(map(thing$model, augment, data = career_sim_ab)) %>%
  ggplot(aes(p, .fitted, color = AB)) +
  geom_point() +
  scale_color_continuous(trans = "log", breaks = c(1, 10, 100, 1000)) +
  geom_abline(color = "red") +
  facet_wrap(~ method) +
  labs(x = "True batting average (p)",
       y = "Shrunken batting average estimate")

