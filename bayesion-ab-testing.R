## Chapter 6: Bayesion A/B Testing
#      Who is a better batter Mike Piazza or Hank Aaron?
#       Piazza 2127 / 6911 = .308
#       Aaron 3771 / 12364 = .305
#      Can we say with confidence that his skill is actually higher,
#      or is it possible he just got lucky a bit more often?
#

# Setup
library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)
theme_set(theme_bw())

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

# Add player names
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

# values estimated by maximum likelihood in Ch 3
alpha0 <- 101.4
beta0 <- 287.3

# For each player, update the beta prior based on the evidence
# to get posterior parameters alpha1 and beta1
career_eb <- career %>%
  mutate(
    eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) %>%
  mutate(
    alpha1 = (H + alpha0),
    beta1 = (AB - H + beta0)
    ) %>%
  arrange(desc(eb_estimate))

#
## Figure 6.1: Posterior Distrubutions for BAs of Aaron and Piazza
#             "If I picked a random draw from Piazza's distribution
#              and a random draw from Aaron's, what's the probability
#              Piazza is higher?"
#

# Save into seperate objects for further analysis
aaron <- career_eb %>% filter(name == "Hank Aaron")
piazza <- career_eb %>% filter(name == "Mike Piazza")
two_players <- bind_rows(aaron, piazza)

x <- seq(0, 1, length=3000)
beta_dist <- data.frame(
  cbind(
    x,
    dbeta(x, aaron$alpha1, aaron$beta1),
    dbeta(x, piazza$alpha1, piazza$beta1)
  )
)

colnames(beta_dist) <- c(
  "x",
  "Hank Aaron",
  "Mike Piazza"
)

# melt the new ds
beta_dist <- melt(beta_dist, x)

beta_dist %>%
  ggplot(aes(x, value, color = variable)) +
  geom_line() +
  xlim(0.28, 0.33) + ## 'Realistic BAs'
  labs(x = "Batting average", y = "density")


#
## Figure 6.2: Posterior Distributions for BAs of Aaron, Piazza and Matsui
#
#

matsui <- career_eb %>% filter(name == "Hideki Matsui")
beta_dist <- data.frame(
  cbind(
    x,
    dbeta(x, aaron$alpha1, aaron$beta1),
    dbeta(x, matsui$alpha1, matsui$beta1),
    dbeta(x, piazza$alpha1, piazza$beta1)
  )
)

colnames(beta_dist) <- c(
  "x",
  "Hank Aaron",
  "Hideki Matsui",
  "Mike Piazza"
)

beta_dist <- melt(beta_dist, x)

beta_dist %>%
  ggplot(aes(x, value, color = variable)) +
  geom_line() +
  xlim(0.26, 0.33) +
  labs(x = "Batting average", y = "density")

#
## Figure 6.3: Join distribution of Aaron and Piazza visualized as a point cloud
#   Here, we're asking what fraction of the joint probability density lies below
#   that black line, where Piazza's avg is greater than Aaron's. Notice that a bit
#   more of that cloud's mass lies below than above: That's confirming the posterior
#   probability that Piazza is better is about 60%
#
x <- seq(.29, .318, .0002)
crossing(piazza_x = x, aaron_x = x) %>%
  mutate(piazza_density = dbeta(piazza_x, piazza$alpha1, piazza$beta1),
         aaron_density = dbeta(aaron_x, aaron$alpha1, aaron$beta1),
         joint = piazza_density * aaron_density) %>%
  ggplot(aes(piazza_x, aaron_x, fill = joint)) +
  geom_tile() +
  geom_abline() +
  scale_fill_gradient2(low = "white", high = "red") +
  labs(x = "Piazza batting average",
       y = "Aaron batting average",
       fill = "Joint density") +
  theme(legend.position = "none")

#
# 
piazza_simulation <- rbeta(1e8, piazza$alpha1, piazza$beta1)
aaron_simulation <- rbeta(1e8, aaron$alpha1, aaron$beta1)

sim <- mean(piazza_simulation > aaron_simulation)

# Quantify using numerical integration
d <- 0.00002
limits <- seq(.29, .33, d)
sum(outer(limits, limits, function(x, y) {
  (x > y) *
    dbeta(x, piazza$alpha1, piazza$beta1) *
    dbeta(y, aaron$alpha1, aaron$beta1) *
    d ^ 2
}))

## Closed-form solution
h <- function(alpha_a, beta_a, alpha_b, beta_b) {
  j <- seq.int(0, round(alpha_b) - 1)
  log_vals <- (lbeta(alpha_a + j, beta_a + beta_b) - log(beta_b + j) -
                 lbeta(1 + j, beta_b) - lbeta(alpha_a, beta_a))
  1 - sum(exp(log_vals))
}

h(piazza$alpha1, piazza$beta1,
  aaron$alpha1, aaron$beta1)

#
## Figure 6.4: posterior beta distribution of Hank Aaron and Mike Piazza,
#              shown alongside the normal approximation to each as a dashed line
#
#
two_players %>%
  mutate(mu = alpha1 / (alpha1 + beta1),
         var = alpha1 * beta1 / ((alpha1 + beta1) ^ 2 * (alpha1 + beta1 + 1))) %>%
  crossing(x = seq(.28, .33, .00025)) %>%
  mutate(density = dbeta(x, alpha1, beta1),
         normal = dnorm(x, mu, sqrt(var))) %>%
  ggplot(aes(x, density, group = name)) +
  geom_line(aes(color = name)) +
  geom_line(lty = 2)


# Closed-form approximation: when alpha and beta are both fairly large,
#  beta starts looking a lot like a normal distribution, so much so that it
#  can be closely approximated.

h_approx <- function(alpha_a, beta_a, alpha_b, beta_b) {
  u1 <- alpha_a / (alpha_a + beta_a)
  u2 <- alpha_b / (alpha_b + beta_b)
  var1 <- (alpha_a * beta_a) /
    ((alpha_a + beta_a) ^ 2 * (alpha_a + beta_a + 1))
  var2 <- (alpha_b * beta_b) /
    ((alpha_b + beta_b) ^ 2 * (alpha_b + beta_b + 1))
  pnorm(0, u2 - u1, sqrt(var1 + var2))
}

h_approx(piazza$alpha1, piazza$beta1, aaron$alpha1, aaron$beta1)


## Figure 6.5: Confidence and credible intervals comparing 20 random players to Mike Piazza average
#
#

prop.test(two_players$H, two_players$AB)
## non-significant p-value of 0.70 indicating the test couldn't find a difference

## Now we'll use empirical bayes to compute the credible interval about the difference
#    in these two players.

credible_interval_approx <- function(a, b, c, d) {
  u1 <- a / (a + b)
  u2 <- c / (c + d)
  var1 <- a * b / ((a + b) ^ 2 * (a + b + 1))
  var2 <- c * d / ((c + d) ^ 2 * (c + d + 1))
  
  mu_diff <- u2 - u1
  sd_diff <- sqrt(var1 + var2)
  
  tibble(posterior = pnorm(0, mu_diff, sd_diff),
             estimate = mu_diff,
             conf.low = qnorm(0.025, mu_diff, sd_diff),
             conf.high = qnorm(0.975, mu_diff, sd_diff))
}

credible_interval_approx(piazza$alpha1, piazza$beta1,
                         aaron$alpha1, aaron$beta1)


set.seed(42)

intervals <- career_eb %>%
  filter(AB > 10) %>%
  sample_n(20) %>%
  group_by(name, H, AB) %>%
  do(credible_interval_approx(piazza$alpha1, piazza$beta1, .$alpha1, .$beta1)) %>%
  ungroup() %>%
  mutate(name = reorder(paste0(name, " (", H, " / ", AB, ")"), -estimate))

f <- function(H, AB) broom::tidy(prop.test(c(H, piazza$H), c(AB, piazza$AB)))
prop_tests <- purrr::map2_df(intervals$H, intervals$AB, f) %>%
  mutate(estimate = estimate1 - estimate2,
         name = intervals$name)

all_intervals <- bind_rows(
  mutate(intervals, type = "Credible"),
  mutate(prop_tests, type = "Confidence")
)

all_intervals %>%
  mutate(name = reorder(name, -AB, na.rm = TRUE)) %>%
  ggplot(aes(x = estimate, y = name, color = type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  xlab("Piazza average - player average") +
  ylab("Player")