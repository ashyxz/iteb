#
## Chapter 9: Mixture models and expectation-maximization
#
#  So far, we've been treating our overall distribution of
#  batting averages as a beta distribution, which is a simple
#  distribution between 0 and 1 that has a single peak. But
#  what if that weren't a good fit? For example, what if we had
#  a multimodal distribution, with multiple peaks?
#
#
#

library(dplyr)
library(tidyr)
library(Lahman)
library(VGAM)
library(ggplot2)
theme_set(theme_bw())

# Identify those who have pitched at least three games
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

career <- Batting %>%
  filter(AB > 0, lgID == "NL", yearID >= 1980) %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  mutate(average = H / AB,
         isPitcher = playerID %in% pitchers$playerID)

# Add player names
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")


fit_bb_mle <- function(x, n) {
  # dbetabinom.ab is the likelihood function for a beta-binomial
  # using n, alpha and beta as parameters
  ll <- function(alpha, beta) {
    -sum(dbetabinom.ab(x, n, alpha, beta, log = TRUE))
  }
  m <- stats4::mle(ll, start = list(alpha = 3, beta = 10),
                   method = "L-BFGS-B", lower = c(0.001, .001))
  ab <- stats4::coef(m)
  tibble(alpha = ab[1], beta = ab[2])
}

batting_w_pitchers <- Batting %>%
  filter(AB >= 50, lgID == "NL", yearID > 1985) %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  mutate(average = H / AB,
         isPitcher = ifelse(playerID %in% pitchers$playerID, "Pitcher", "Non-Pitcher"),
         isPitcher = relevel(factor(isPitcher), "Pitcher")) #???

fit <- fit_bb_mle(batting_w_pitchers$H, batting_w_pitchers$AB)

#
## Figure 9.1: Distribution of BAs when pitchers are included.
#               The beta distribution that would be fit by maximum
#                likelihood is shown as the dashed line.
batting_w_pitchers %>%
  ggplot(aes(average, fill = isPitcher)) +
  geom_histogram(bins = 30) +
  stat_function(fun = function(x) 30 * dbeta(x, fit$alpha, fit$beta), lty = 2) +
  xlim(0, .4) +
  labs(fill = "",
       x = "Batting average (H / AB)")

#
## Figure 9.2: Density of BAs amond players assigned to cluster A or B
#
#

# Expectation maximization clustering algorithm setup
set.seed(2016)

# We'll fit the clusters only with players that have had at least 20 at-bats
starting_data <- career %>%
  filter(AB >= 20) %>%
  select(-year, -bats, -isPitcher) %>%
  mutate(cluster = factor(sample(c("A", "B"), n(), replace = TRUE)))

starting_data %>%
  ggplot(aes(average, color = cluster)) +
  geom_density()

#
## Figure 9.3: Density within each of the randomly assigned clusters,
#              along with a histogram of the cluster assignments
#
fits <- starting_data %>%
  group_by(cluster) %>%
  do(fit_bb_mle(.$H, .$AB)) %>%
  ungroup()

fits %>%
  crossing(x = seq(0, .4, .0001)) %>%
  mutate(density = dbeta(x, alpha, beta)) %>%
  ggplot() +
  geom_histogram(aes(average, y = ..density.., fill = cluster), data = starting_data, alpha = .2) +
  geom_line(aes(x, density, color = cluster))

#
## Figure 9.4: Assignments of players to clusters based on the beta-binomial
#              model.
#
crosses <- starting_data %>%
  select(-cluster) %>%
  crossing(fits) %>%
  mutate(likelihood = VGAM::dbetabinom.ab(H, AB, alpha, beta))

assignments <- starting_data %>%
  select(-cluster) %>%
  crossing(fits) %>%
  mutate(likelihood = VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>%
  group_by(playerID) %>%
  top_n(1, likelihood) %>%
  ungroup()

assignments %>%
  ggplot(aes(average, fill = cluster)) +
  geom_histogram()

#
## Figure 9.5: Distribution of the beta-binomial density, fit to each of the
#              clusters assigned in the last iteration.
#
assignments %>%
  group_by(cluster) %>%
  do(fit_bb_mle(.$H, .$AB)) %>%
  ungroup() %>%
  crossing(x = seq(0, .4, .0001)) %>%
  mutate(density = .01 * nrow(assignments) * dbeta(x, alpha, beta)) %>%
  ggplot() +
  geom_histogram(aes(average, fill = cluster), data = assignments, alpha = .25, binwidth = .01) +
  geom_line(aes(x, density, color = cluster))
  
#
## Figure 9.6: Histogram of the assignments of players to clusters A and B at
#               each iteration of the expectation-maximization algorithm
#
set.seed(1337)

iterate_em <- function(state, ...) {
  # maximization
  fits <- state$assignments %>%
    group_by(cluster) %>%
    do(fit_bb_mle(.$H, .$AB)) %>%
    ungroup()
  
  # expectation
  assignments <- state$assignments %>%
    select(playerID:average) %>%
    crossing(fits) %>%
    mutate(likelihood = VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>%
    group_by(playerID) %>%
    top_n(1, likelihood) %>%
    ungroup()
  
  list(assignments = assignments, fits = fits)
}

library(purrr)
init <- list(assignments = starting_data)
iterations <- accumulate(1:5, iterate_em, .init = init)

assignment_iterations <- iterations %>%
  map_df("assignments", .id = "iteration")

assignment_iterations %>%
  ggplot(aes(average, fill = cluster)) +
  geom_histogram() +
  facet_wrap(~ iteration)

## We notice that only the first few iteration led to a shift in assignments
## after which it appears to converge to stable
assignment_iterations %>%
  crossing(x = seq(.001, .4, .001)) %>%
  mutate(density = dbeta(x, alpha, beta)) %>%
  ggplot(aes(x, density, color = iteration, group = iteration)) +
  geom_line() +
  facet_wrap(~ cluster)

#
## Figure 9.7: The likelihood that cluster A or B would generate each of these
#               six players' records. By Bayes Theorem, we can simply use the
#               ratio of one likelihood (say, A in red) to the sum of the two
#               likelihoods to get the posterior probability
final_parameters <- last(iterations)$fits

batter_100 <- career %>%
  filter(AB == 100) %>%
  arrange(average) %>%
  select(-playerID, -bats)

final_parameters %>%
  crossing(x = 0:45) %>%
  mutate(density = VGAM::dbetabinom.ab(x, 100, alpha, beta)) %>%
  ggplot(aes(x, density)) +
  geom_line(aes(color = cluster)) +
  geom_vline(aes(xintercept = H), data = batter_100, lty = 2) +
  geom_text(aes(x = H, y = -.022, label = name), data = batter_100, hjust = 1, vjust = 1, angle = 270) +
  labs(x = "H (out of 100 at-bats)",
       y = "Likelihood of this H out of 100 hits")


#
## Figure 9.8: The posterior probability that each of the 6 players with 100 ABs
#              is in the pitcher cluster.
#
final_parameters %>%
  crossing(H = 1:40) %>% # ???
  transmute(H, cluster, likelihood = VGAM::dbetabinom.ab(H, 100, alpha, beta)) %>%
  spread(cluster, likelihood) %>%
  mutate(probability_A = A / (A + B)) %>%
  ggplot(aes(H, probability_A)) +
  geom_line() +
  geom_vline(aes(xintercept = H), data = batter_100, lty = 2) +
  geom_text(aes(x = H, y = 0, label = name), data = batter_100, hjust = 1, vjust = 1, angle = 270) +
  labs(x = "H (out of 100 at-bats)",
       y = "(likelihood if pitcher) / (likelihood if pticher + likelihood if not)")

#
## Figure 9.9: Posterior distributions for the BA of each of the six players with
#              100 ABs. Each player's raw BA is shown as a dashed vline
#
career_likelihoods <- career %>%
  filter(AB > 20) %>%
  crossing(final_parameters) %>%
  mutate(likelihood = VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>%
  group_by(playerID) %>%
  mutate(posterior = likelihood / sum(likelihood))

career_assignments <- career_likelihoods %>%
  top_n(1, posterior) %>%
  ungroup()

batting_data <- career_likelihoods %>%
  ungroup() %>%
  filter(AB == 100) %>%
  mutate(name = paste0(name, " (", H, "/", AB, ")"),
         name = reorder(name, H),
         alpha1 = H + alpha,
         beta1 = AB - H + beta)

batting_data %>%
  crossing(x = seq(0, .4, .001)) %>%
  mutate(posterior_density = posterior * dbeta(x, alpha1, beta1)) %>%
  group_by(name, x) %>%
  summarize(posterior_density = sum(posterior_density)) %>%
  ggplot(aes(x, posterior_density, color = name)) +
  geom_line(show.legend = FALSE) +
  geom_vline(aes(xintercept = average), data = batting_data, lty = 2) +
  facet_wrap(~ name) +
  labs(x = "Batting average (actual average shown as dashed line)",
       y = "Posterior density after updating")

#
## Figure 9.10: Effect of empirical Bayes shrinkage towards either a single beta
#               prior or a mixture model.
#

eb_shrinkage <- career_likelihoods %>%
  mutate(shrunken_average = (H + alpha) / (AB + alpha + beta)) %>%
  group_by(playerID) %>%
  summarize(shrunken_average = sum(posterior * shrunken_average))

library(forcats)

cluster_means <- final_parameters$alpha / (final_parameters$alpha + final_parameters$beta)

levs <- c("Raw batting average", "EB estimate", "EB estimate; mixture model")
lines <- tibble(type = factor(c("EB estimate", rep("EB estimate; mixture model", 2)), levs),
                    value = c(fit$alpha / (fit$alpha + fit$beta), cluster_means))

eb_shrinkage %>%
  inner_join(career_assignments) %>%
  filter(AB > 50) %>%
  mutate(eb_estimate = (fit$alpha + H) / (fit$alpha + fit$beta + AB)) %>%
  gather(type, estimate, average, eb_estimate, shrunken_average) %>%
  mutate(type = fct_recode(type, "Raw batting average" = "average",
                           "EB estimate" = "eb_estimate",
                           "EB estimate; mixture model" = "shrunken_average"),
         type = factor(type, levels = levs)) %>%
  ggplot(aes(AB, estimate)) +
  geom_point(aes(color = cluster)) +
  geom_hline(aes(yintercept = value), lty = 2, data = lines) +
  scale_x_log10() +
  facet_wrap(~ type) +
  geom_abline(color = "red") +
  labs(y = "Estimated batting average",
       color = "Assignments")