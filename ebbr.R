## Chapter 11: The ebbr package
#
#

# setup
library(Lahman)
library(dplyr)
library(tidyr)
library(ebbr)
library(splines)
library(ggplot2)
theme_set(theme_bw())

# Grab career BA of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

# Add player names
player_names <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  unite(name, nameFirst, nameLast, sep = " ")

# include the "bats" (handedness) and "year" column for later
career_full <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  inner_join(player_names, by = "playerID") %>%
  filter(!is.na(bats))

# we don't need all this data for every step
career <- career_full %>%
  select(-bats, -year)

#
## Figure 11.1: Distribution of player BAs
#
#
career %>%
  filter(AB >= 100) %>%
  ggplot(aes(H / AB)) +
  geom_histogram() +
  xlab("Batting average (H / AB)")

#
## Figure 11.2: Comparison of the raw BA and the EB Shrunked BA
#
#

# Filter for only players with more than 500 at-bats
prior <- career %>%
  filter(AB >= 500) %>%
  ebb_fit_prior(H, AB)

# The second step of empirical Bayes analysis is updating each observation
#  based on the overall statistical model. Based on the philosophy of the broom
#  package, this is achieved with the augment() function
augment(prior, data = career)

# We often want to run these two steps in sequence: estimating a model, then using
#  it as a prior for each observation. The ebbr package provides a shortcut, combining
#  them into one step with add_ebb_estimate()
eb_career <- career %>%
  ebbr::add_ebb_estimate(H, AB, prior_subset = AB >= 500)

# Estimates and credible intervals
eb_career %>%
  ggplot(aes(.raw, .fitted, color = AB)) +
  geom_point() +
  geom_abline(color = "red") +
  scale_color_continuous(trans = "log", breaks = c(1, 10, 100, 1000)) +
  geom_hline(yintercept = tidy(prior)$mean, color = "red", lty = 2) +
  labs(x = "Raw batting average",
       y = "Shrunken batting average")

## Figure 11.3: Credible intervals for seven selected players from 
#               the 98 yankees
#
yankee_1998 <- c("brosisc01", "jeterde01", "knoblch01",
                 "martiti02", "posadjo01", "strawda01", "willibe02")

eb_career %>%
  filter(playerID %in% yankee_1998) %>%
  mutate(name = reorder(name, .fitted)) %>%
  ggplot(aes(.fitted, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = .low, xmax = .high)) +
  labs(x = "Estimated batting average (w/ 95% confidence interval)",
       y = "Player")

## Figure 11.4: The relationship between AB and the raw estimate, as
#               well as with and the shrunked empirical Bayes estimate
#               using a beta-binomial regression model.

# Hierarchical modeling

# Raw
career %>%
  filter(AB >= 10) %>%
  ggplot(aes(AB, H / AB)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10()

# shrunken
eb_career_ab <- career %>%
  ebbr::add_ebb_estimate(H, AB, method = "gamlss",
                   mu_predictors = ~ log10(AB))

# combine
eb_career_ab %>%
  filter(AB > 10) %>%
  rename(Raw = .raw, Shrunken = .fitted) %>%
  gather(type, estimate, Raw, Shrunken) %>%
  ggplot(aes(AB, estimate)) +
  geom_point() +
  facet_wrap(~ type) +
  scale_x_log10()

## Figure 11.5: The prior distribution that would be used for a left or right
#               -handed player at a particular point in time
#

# TODO: error: Argument 2 must be length 1, not 20
#  Has something to do with the mu_predictors arg
eb_career_prior <- career_full %>%
  ebbr::ebb_fit_prior(H, AB, method = "gamlss",
                mu_predictors = ~ 0 + ns(year, df = 5) * bats + log(AB))
  

# Fake data ranging from 1885 to 2013
fake_data <- crossing(H = 300,
                      AB = 1000,
                      year = seq(1885, 2013),
                      bats = c("L", "R"))

# find the mean of the prior, as well as the 95% quantiles,
# for each of these combinations. This does require a bit
# of manual manipulation of alpha0 and beta0:
augment(eb_career_prior, newdata = fake_data) %>%
  mutate(prior = .alpha0 / (.alpha0 + beta0),
         prior.low = qbeta(0.025, .alpha0, .beta0),
         prior.high = qbeta(0.975, .alpha0, .beta0)) %>%
  ggplot(aes(year, prior, color = bats)) +
  geom_line() +
  geom_ribbon(aes(ymin = prior.low, ymax = prior.high), alpha = .1, lty = 2) +
  ylab("Prior distribution (mean + 95% quantiles)")

## Figure 11.6: Histogram of mixture model assignments to the 2 clusters
#
#
test_300 <- career %>%
  add_ebb_estimate(H, AB, method = "gamlss", mu_predictors = ~ log10(AB)) %>%
  add_ebb_prop_test(.300, sort = TRUE)

test_300 %>%
  dplyr::select(name, H, AB, .fitted, .low, .high, .pep, .qvalue)

## Mike Piazza's posterior parameters
piazza  <- eb_career_ab %>%
  filter(name == "Mike Piazza")

piazza_params <- c(piazza$.alpha1, piazza$.beta1)

compare_piazza <- eb_career_ab %>%
  add_ebb_prop_test(piazza_params, approx = TRUE, sort = TRUE)

compare_piazza %>%
  select(name, H, AB, .fitted, .low, .high, .pep, .qvalue)

## Mixture Models
career_w_pitchers <- Batting %>%
  filter(AB >=25, lgID == "NL", yearID >= 1980) %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  mutate(isPitcher = playerID %in% pitchers$playerID) %>%
  inner_join(player_names, by = "playerID")

# The ebbr pkg provides the ebb_fit_mixture() function for fitting a mixture
# model using an iterative expectation algorithm. Like the other estimation
# functions, it takes a table as the first argument, followed by two arguments
# for the "successes" column and the "total" column:

## Error: Numeric 'envir' arg not of length one...
set.seed(1337)
mm <- ebbr::ebb_fit_mixture(career_w_pitchers, H, AB, clusters = 2)

tidy(mm)
mm$assignments %>%
  ggplot(aes(H / AB, fill = .cluster)) +
  geom_histogram(alpha = 0.8, position = "identity")


## Figure 11.7: Histogram of the BAs of players assigned to clusters A and B
#               The iterations component also contains details on the parameters
#               fit at each of the maximization steps, which can then be
#               visualized to see how those parameters converged (Figure 11.8)

assignments <- mm$iterations$assignments
# need to fix ebb_fit_mixture error above
assignments %>%
  ggplot(aes(H / AB, fill = .cluster)) +
  geom_histogram(alpha = 0.8, position = "identity") +
  facet_wrap(~ iteration)


## Figure 11.8: The estimated alpha and beta parameters for each cluster at each
#               iteration of the expectation-maximization algorithm.
#

fits <- mm$iterations$fits
# need to fix ebb_fit_mixture error above
fits %>%
  gather(parameter, value, alpha, beta, mean) %>%
  ggplot(aes(iteration, value, color = parameter, lty = cluster)) +
  geom_line() +
  facet_wrap(~ paramter, scales = "free_y")