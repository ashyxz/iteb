## Chapter 5. Hypothesis testing and FDR
#      (False Discovery Rate Control)

library(dplyr)
library(tidyr)
library(Lahman)
library(reshape2)
library(ggplot2)
theme_set(theme_bw())

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

# values estimated by maximum likelihood in Ch 3
alpha0 <- 101.4
beta0 <- 287.3


career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0),
         alpha1 = H + alpha0,
         beta1 = AB - H + beta0)


#
## Figure 5.1: The posterior distribution for the true BA
#               of Hank Aaron (3771 H / 12364 AB).
#               The BA of .3 is marked as a dashed red line,
#               and the region where his batting average is
#               less than .3 is shaded
#
hank_aaron <- career_eb %>%
  filter(name == "Hank Aaron")

x <- seq(0, 1, length=3000)
beta_dist <- data.frame(
  cbind(
    x,
    dbeta(x, hank_aaron$alpha1, hank_aaron$beta1)
  )
)

colnames(beta_dist) <- c(
  "x",
  "alpha = 3872, beta = 8880"
)

# melt the new ds
beta_dist <- melt(beta_dist, x)

beta_dist %>%
  ggplot(aes(x, value, color = variable)) +
  geom_line(color = "black") +
  geom_vline(xintercept = 0.3,
             color = "red",
             lty = 2) +
  geom_area(mapping = aes(x = ifelse(x > 0.29 & x < 0.30, x, 0)),
            fill = "red",
            alpha = 0.2,
            show.legend = FALSE) +
  xlim(0.275, 0.325) + ## 'Realistic BAs'
  labs(x = "Batting average", y = "density")

## Figure 5.2: Histogram of posterior error probability (PEP) 
#               values across all players
#

career_eb <- career_eb %>%
  mutate(PEP = pbeta(0.3, alpha1, beta1))

career_eb %>%
  ggplot() +
  geom_histogram(aes(PEP), bins = 24)

## Figure 5.3: Relationship of the shrunken batting average
#                and the PEP of whether the players BA is > 0.3
#
career_eb %>%
  ggplot(aes(x = eb_estimate, y = PEP)) +
  geom_point(aes(color = AB)) +
  scale_color_continuous(
    trans = "log",
    breaks = c(1, 10, 100, 1000)
  ) +
  geom_vline(xintercept = 0.3, linetype = "dashed", color = "red")


## Figure 5.4: Comparison of the q-value threshold (for finding players with an avg > 0.300)
#               and the number of players that would be included at that threshold

# FDR and Q-values: what is the probability that you belong in the HOF if you are in it already?
#  Said another way, we want to ensure that if you're already in the HOF, the probability you
#   belong is at least 95%
#
top_players <- career_eb %>%
  arrange(PEP) %>%
  head(100)

mean(top_players$PEP)

sorted_PEP <- career_eb %>%
  arrange(PEP)

mean(head(sorted_PEP$PEP, 50))
## [1] 0.002381815

mean(head(sorted_PEP$PEP, 200))
## [1] 0.272276

# Q-Values: Cumulative mean of all the (sorted) PEP's
career_eb <- career_eb %>%
  arrange(PEP) %>%
  mutate(qvalue = cummean(PEP))

hall_of_fame <- career_eb %>%
  filter(qvalue < 0.05)

strict_hall_of_fame <- career_eb %>%
  filter(qvalue < 0.01)

# 
career_eb %>%
  ggplot(aes(x = qvalue, y = count(qvalue < x))) +
  geom_line()