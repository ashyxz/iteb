## Chapter 4. Credible Intervals
# Sometimes we want to know more than just
# our "best guess", and instead wish to know
# how much uncertainty is present in our point
# estimate

library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

## include player's names
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

# values estimated by maximum likelihood in ch3
alpha0 <- 101.4
beta0 <- 287.3

career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))

## Figure 4.1 Posterior Beta distribution for each of 7 yankee batters

career_eb <- career_eb %>%
  mutate(alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H)

## so we really have here 8 plots overlayed. 
#   7 players and 1 posterior beta distribution dotted
faves <- list("willibe02", "strawda01", "posadjo01", 
              "martiti02", "knoblch01", "jeterde01", "brosisc01")

yankee_faves <- career_eb %>%
  filter(playerID %in% faves)

# Setup our beta distribution (probability of probabilities given a, b)
x <- seq(0,1, length=500)
beta_dist <- data.frame(
  cbind(
    x,
    dbeta(x, yankee_faves[1,]$alpha1, yankee_faves[1,]$beta1),
    dbeta(x, yankee_faves[2,]$alpha1, yankee_faves[2,]$beta1),
    dbeta(x, yankee_faves[3,]$alpha1, yankee_faves[3,]$beta1),
    dbeta(x, yankee_faves[4,]$alpha1, yankee_faves[4,]$beta1),
    dbeta(x, yankee_faves[5,]$alpha1, yankee_faves[5,]$beta1),
    dbeta(x, yankee_faves[6,]$alpha1, yankee_faves[6,]$beta1),
    dbeta(x, yankee_faves[7,]$alpha1, yankee_faves[7,]$beta1)
  )
)

colnames(beta_dist) <- c(
  "x",
  "Scott Brosius",
  "Derek Jeter",
  "Chuck Knoblauch",
  "Tino Martinez",
  "Jorge Posada",
  "Darryl Strawberry",
  "Bernie Williams"
)

beta_dist <- melt(beta_dist, x)

g <- ggplot(beta_dist, aes(x, value, color=variable)) +
  geom_line() +
  xlim(0.2, 0.35) + ## 'Realistic BAs'
  labs(x = "Batting average", y = "Density of beta") +
  theme_bw()

g$labels$colour <- "Player"

# Show the graph
g

# Add prior distribution (from ch2)



## Figure 4.2 Posterior beta dist for Derek Jeter
#             with a 95% credible interval highlighted.
#             Prior is show as the dashed curve

# This is gonna be a 3-4 parter
#  1. Posterior Beta Distribution line
#  2. Prior Beta Distribution (ch2)
#  3. Highlighted 95% credible interval

# Calculate credible interval
yankee_1998_career <- yankee_faves %>%
  mutate(low = qbeta(0.025, alpha1, beta1),
         high = qbeta(0.975, alpha1, beta1))


## Figure 4.3 95% Credible intervals for each of seven Yankees
# 
#
yankee_1998_career %>%
  mutate(name = reorder(name, eb_estimate)) %>%
  ggplot(aes(eb_estimate, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0),
             color = "red", lty = 2) +
  xlab("Estimated batting average (w/ 95% interval)") +
  ylab("Player")


## Figure 4.4 Frequentist confidence intervals and Bayesian
#              credible intervals for 20 random players.
#

# Subsample 20 random players in the career_eb set
random_20 <- sample_n(career_eb, 20)

# Calcalate Confidence interval using a method
random_20 <- random_20 %>%
  mutate(cpConf_low = qbeta(0.025, H, (AB - H + 1)),
         cpConf_high = qbeta(0.975, (H + 1), (AB - H)))

# Calculate Credible interval using the qbeta method
random_20 <- random_20 %>%
  mutate(cred_low = qbeta(0.025, alpha1, beta1),
         cred_high = qbeta(0.975, alpha1, beta1))


## Add H / AB to name label
random_20 <- random_20 %>%
  mutate(display_name = paste(name, "(", sep = " "))

random_20 <- random_20 %>%
  mutate(display_name = paste(display_name, H, sep = ""))

random_20 <- random_20 %>%
  mutate(display_name = paste(display_name, AB, sep = "/"))

random_20 <- random_20 %>%
  mutate(display_name = paste(display_name, ")", sep = ""))

# plot using geom_errorbarh
random_20 %>%
  mutate(display_name = reorder(display_name, -AB)) %>%
  ggplot(aes(eb_estimate, display_name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = cred_low, xmax = cred_high, color = "Credible")) +
  geom_errorbarh(aes(xmin = cpConf_low, xmax = cpConf_high, color = "Confidence")) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0),
             color = "red", lty = 2) +
  xlab("Estimated batting average") +
  ylab("Player")