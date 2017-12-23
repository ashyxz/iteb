#
## Chapter 8: Empirical Bayesian Hierarchical modeling
#
#  Suppose you were a scout hiring a new baseball player,
#  and were choosing between two that have had 100 at-bats each:
#   - a left-handed batter who has hit 30 hits / 100 at-bats
#   - a right-handed batter who has hit 30 hits / 100 at-bats
#  who would you guess was the better batter?
#  This seems like a silly question, they both have the same exact
#  batting record. But what if I told you that historically, left-
#  handed batters are slightly better hitters than right-handed? How
#  could you incorporate that evidence?
#

# Setup
library(gamlss)
library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)
theme_set(theme_bw())

# Grab career BA of non-pitchers
# (Allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
  group_by(playerID) %>% # Pitching table has one instance for each year
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

# we're keeping some extra information for later in the post:
# a "bats" coumn and a "year" column
career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  mutate(average = H/AB)

# add player names
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

fit <- gamlss(cbind(H, AB - H) ~ log(AB),
              data = dplyr::select(career, -bats),
              family = BB(mu.link = "identity"))

# prior alpha0 and beta0 can then be computed for each player based on mu and a dispersion parameter sigma
career_eb <- career %>%
  mutate(mu = fitted(fit, "mu"),
         sigma = fitted(fit, "sigma"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H,
         estimate = alpha1 / (alpha1 + beta1))


# relevel to set right-handed batters as the baseline
career2 <- career %>%
  filter(!is.na(bats)) %>%
  mutate(bats = relevel(bats, "R"))

fit2 <- gamlss(cbind(H, AB - H) ~ log(AB) + bats,
               data = career2,
               family = BB(mu.link = "identity"))

library(broom)
tidy(fit2)

#
## Figure 8.1: The prior distribution according to the hierarchical model
#               for players with particular combinations of AB and handedness
#
sigma <- fitted(fit2, "sigma")[1]

crossing(bats = c("L","R"),
         AB = c(1, 10, 100, 1000, 10000)) %>%
  augment(fit2, newdata = .) %>%
  rename(mu = .fitted) %>%
  crossing(x = seq(.1, .36, .0005)) %>%
  mutate(alpha = mu / sigma,
         beta = (1 - mu) / sigma,
         density = dbeta(x, alpha, beta)) %>%
  ggplot(aes(x, density, color = factor(AB), lty =bats)) +
  geom_line() +
  labs(x = "Batting average",
       y = "Prior density",
       color = "AB",
       lty = "Batting hand")


#
## Figure 8.2: Empirical Bayes estimates and 95% credible intervals for two
#              hypothetical batters with a 30% success rate, one left-handed
#               and one right-handed

crossing(bats = c("L", "R"),
         AB = c(10, 100, 1000, 10000)) %>%
  augment(fit2, newdata = .) %>%
  mutate(H = .3 * AB,
         alpha0 = .fitted / sigma,
         beta0 = (1 - .fitted) / sigma,
         alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H,
         estimate = alpha1 / (alpha1 + beta1),
         conf.low = qbeta(.025, alpha1, beta1),
         conf.high = qbeta(.975, alpha1, beta1),
         record = paste(H, AB, sep = " / ")) %>%
  ggplot(aes(estimate, record, color = bats)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  labs(x = "Estimate w/ 95% credible interval",
       y = "Batting record",
       color = "Batting hand")


#
## Figure 8.3: Boxplot of batting averages within each decade. To reduce
#              the effect of noise, only players with > 500 at-bats
#              were included
#
career2 %>%
  mutate(decade = factor(round(year - 5, -1))) %>% # Clever!
  filter(AB >= 500) %>%
  ggplot(aes(decade, average)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle =  90, hjust = 1)) +
  ylab("Batting average")

#
## Figure 8.4: Prior distribution that would be used for a left or right-
#               handed player at a particular point in time. Show are the
#                mean and the 95% intervals for each prior

library(splines)

fit3 <- gamlss(cbind(H, AB - H) ~ 0 + ns(year, df = 5)
                                    + bats + log(AB),
               data = career2,
               family = BB(mu.link = "identity"))

plot_gamlss_fit <- function(f) {
  career2 %>%
    dplyr::select(year, bats) %>%
    distinct() %>%
    filter(bats != "B") %>%
    mutate(AB = 1000) %>%
    augment(f, newdata = .) %>%
    rename(mu = .fitted) %>%
    mutate(sigma = fitted(fit3, "sigma")[1],
           alpha0 = mu / sigma,
           beta0 = (1 - mu) / sigma,
           conf_low = qbeta(.025, alpha0, beta0),
           conf_high = qbeta(.975, alpha0, beta0)) %>%
    ggplot(aes(year, mu, color = bats, group = bats)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high), linetype = 2, alpha = .1) +
    labs(x = "Year",
         y = "Prior distribution (median + 95% quantiles)",
         color = "Batting hand")
}

plot_gamlss_fit(fit3)

## Figure 8.5: Prior Distribution that would be used for a left or right-handed
#               player at a particular point in time, allowing for an interaction
#               term between time and handedness

fit4 <- gamlss(cbind(H, AB - H) ~ 0 + ns(year, 5) * bats
                                    + log(AB),
               data = career2,
               family = BB(mu.link = "identity"))

plot_gamlss_fit(fit4)

## Games started by left-handed pitchers over time
Pitching %>%
  dplyr::select(playerID, yearID, GS) %>%
  distinct() %>%
  inner_join(dplyr::select(Master, playerID, throws)) %>%
  count(yearID, throws, wt = GS) %>%
  filter(!is.na(throws)) %>%
  mutate(percent = n / sum(n)) %>%
  filter(throws == "L") %>%
  ggplot(aes(yearID, percent)) +
  geom_line() +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("Year") +
  ylab("% of games with left-handed pitcher")

#
## Figure 8.6: Posterior Distributions for batters with a record of 30 / 100, whether
#               left or right-handed and at different points in time
#
players <- crossing(year = c(1915, 1965, 2015),
                    bats = c("L", "R"),
                    H = 30,
                    AB = 100)

players_posterior <- players %>%
  mutate(mu = predict(fit4, what = "mu", newdata = players),
         sigma = predict(fit4, what = "sigma",
                         newdata = players, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H)

players_posterior %>%
  crossing(x = seq(.15, .3, .001)) %>%
  mutate(density = dbeta(x, alpha1, beta1)) %>%
  ggplot(aes(x, density, color = bats)) +
  geom_line() +
  facet_wrap(~ year) +
  xlab("Batting average") +
  ylab("Posterior density")

