#
## Chapter 7: Beta Binomial Regression
#
#  We've been using empirical Bayes method to estimate batting
#  averages of baseball players. Empirical Bayes is useful in these
#  examples because when we don't have a lot of information about a
#  bater, they're "shrunken" towards the average across all players,
#  as a natural consequence of the beta prior.
#  
#  But there's a complication that we haven't yet approached. When
#  players are better, they are given more chances to bat!
#  That means there's a relationship between the number of at-bats
#  and the true batting average. For reasons explained, this makes
#  out estimates systematically inaccurate. In this chapter, we'll
#  adjust our model to a new one where each batter has his own prior,
#  using a method called beta-binomial regression. We show that this
#  new model lets us adjust for the confounding factor while still
#  relying on the empirical Bayes philosophy. We also note that this
#  give us a general framework for allowing a prior to depend on known
#  information, which will become important in Chapter 8
#

# Setup code
library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)
theme_set(theme_bw())

## Grab career BA of non-pitchers
#  (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)
  
# add in player names
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

# Values estimated by MLE in ch3
alpha0 <- 101.4
beta0 <- 287.3
prior_mu <- alpha0 / (alpha0 + beta0)

# for each player, update the beta prior based on the evidence
# to get posterior parameters alpha1 and beta1
career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) %>%
  mutate(alpha1 = H + alpha0,
         beta1 = AB - H + beta0) %>%
  arrange(desc(eb_estimate))

#
## Figure 7.1: Relationship between the number of at-bats (AB) and the raw BA
#               (H / AB) across all players with at least 10 ABs
#               (X axis is scaled to log10)
#
career %>%
  filter(AB >= 10) %>%
  ggplot(aes(AB, average)) +  # We log scale the x axis, not the actual data
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  xlab("Number of at-bats (AB)") +
  ylab("Raw batting-average (H / AB)")


#
## Figure 7.2: Scatter plot of the relationship AB has with raw BA (left)
#               with empirical Bayes shrunken estimates (right). The prior-
#               mean .261 is shown as a horizontal dashed red line, -fit
#               lines are shown in blue
#
career_eb %>%
  filter(AB >= 10) %>%
  gather(
    type, value,
    average, eb_estimate
    ) %>%  # Since we're displaying eb_est and average side-by-side
  mutate(
    type = plyr::revalue(
      type, c(
        average = "Raw",
        eb_estimate = "With EB Shrinkage"
        )
      )
    ) %>%
  ggplot(aes(AB, value)) + # remember, value is eb_est or avg depending on the graph
  geom_point() +
  scale_x_log10() +
  geom_hline(color = "red", lty = 2, size = 1.5, yintercept = prior_mu) +
  facet_wrap(~type) +
  ylab("average") +
  geom_smooth(method = "lm")

#
## Figure 7.3: The density of the prior distribution for a player with particular
#               numbers of at-bats
#

# Fit the model across all players
library(gamlss)
fit <- gamlss(cbind(H, AB - H) ~ log(AB),
              data = career_eb,
              family = BB(mu.link = "identity"))

library(broom)

td <- tidy(fit)

mu_0 <- td$estimate[1]
mu_AB <- td$estimate[2]
sigma <- exp(td$estimate[3])

crossing(
  x = seq(0.08, .35, .001),
  AB = c(1, 10, 100, 1000, 10000)) %>%
  mutate(density = dbeta(x, (mu_0 + mu_AB * log(AB)) / sigma,
                         (1 - (mu_0 + mu_AB * log(AB))) / sigma)) %>%
  mutate(AB = factor(AB)) %>%
  ggplot(aes(x, density, color = AB, group = AB)) +
  geom_line() +
  xlab("Batting average") +
  ylab("Prior density")

#
## Figure 7.4: The relationship between the original Bayes shrunken estimates
#               and the values under the beta-binomial regression model
#
mu <- fitted(fit, parameter = "mu")
sigma <- fitted(fit, parameter = "sigma")

career_eb_wAB <- career_eb %>%
  dplyr::select(name, H, AB, original_eb = eb_estimate) %>%
  mutate(mu = mu,
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H,
         new_eb = alpha1 / (alpha1 + beta1))

career_eb_wAB %>%
  ggplot(aes(original_eb, new_eb, color = AB)) +
  geom_point() +
  geom_abline(color = "red") +
  xlab("Original EB Estimate") +
  ylab("EB Estimate w/ AB term") +
  scale_color_continuous(trans = "log", breaks = 10 ^ (0:4))


#
## Figure 7.5: The relationship between AB and the estimate for 3 methods:
#                Raw BA, Shrunken BA, and Avgs shrunk towards a relationship
#                 found through regression

lev <- c(raw = "Raw H / AB", original_eb = "EB Estimate", new_eb = "EB w/ Regression")

career_eb_wAB %>%
  filter(AB >= 10) %>%
  mutate(raw = H / AB) %>%
  gather(type, value, raw, original_eb, new_eb) %>% # since we are comparing raw, oeb, neb
  mutate(mu = ifelse(type == "original_eb", prior_mu,
                     ifelse(type == "new_eb", mu, NA))) %>%
  mutate(type = factor(plyr::revalue(type, lev), lev)) %>% # ???
  ggplot(aes(AB, value)) +
  geom_point() +
  geom_line(aes(y = mu), color = "red") +
  scale_x_log10() +
  facet_wrap(~type) +
  xlab("At-Bats (AB)") +
  ylab("Estimate")