#
## Chapter 10: The Multinomial and the Dirichlet
#
#
#

# setup
library(Lahman)
library(dplyr)
library(tidyr)
library(VGAM)
library(ggplot2)
theme_set(theme_bw())

pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

player_names <- Master %>%
  transmute(playerID, name = paste(nameFirst, nameLast))

# include the "bats" (handedness) and "year" column for later
hit_types <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  rename(Double = X2B, Triple = X3B) %>%
  group_by(playerID) %>%
  summarize_each(funs(sum(., na.rm = TRUE)), AB, H, Double, Triple, HR) %>%
  inner_join(player_names, by = "playerID") %>%
  transmute(playerID, name, AB, H,
            Single = H - Double - Triple - HR,
            Double, Triple, HR,
            NonHit = AB - H)

## Figure 10.1: Percentages of each player's hits that are made
#               up of singles, doubles, triples, or hrs
#
hit_type_order <- c("Single", "Double", "Triple", "HR")

hit_types_gathered <- hit_types %>%
  select(-H) %>%
  gather(type, value, -playerID, -name, -AB) %>%
  mutate(percent = value / AB)

hit_types_gathered %>%
  filter(AB > 500, type != "NonHit") %>%
  mutate(type = factor(type, hit_type_order)) %>%
  ggplot(aes(value / AB)) +
  geom_histogram() +
  facet_wrap(~type, scales = "free_y")

## Figure 10.2: Histograms of simulated values from the Dirichlet
#               distribution. Each row of graphs is one set of
#               parameters, such as (1,1,1,1,1), and each column is
#               one of the five categories
hit_types <- hit_types %>%
  mutate(slugging = (Single + 2 * Double + 3 * Triple + 4 * HR) / AB)

# Value from Dirichlet simulation
VGAM::rdiric(3, c(1, 1, 1, 1, 1))

set.seed(2017)
library(purrr)

sim <- tibble(
  parameters = list(
    c(1, 1, 1, 1, 1),
    c(5, 2, 2, 1, 1),
    c(50, 20, 20, 10, 10)),
  name = map_chr(parameters, paste, collapse = ", ")
  ) %>%
  mutate(simulation = map(parameters, ~ VGAM::rdiric(1e5, .))) %>%
  unnest(map(simulation, reshape2::melt, varnames = c("rep", "category"))) # Errors on simulation

sim %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = .05, boundary = 0) +
  facet_grid(name ~ category, scales = "free_y") +
  xlab("Value from Dirichlet simulation")

## Figure 10.3: The density of the Dirichlet distributions as fit by
#               maximum likelihood, compared to the histogram of the
#               percentages for each type of hit.
hit_500 <- hit_types %>%
  filter(AB >= 500)

hit_matrix <- hit_500 %>%
  select(Single, Double, Triple, HR, NonHit) %>%
  as.matrix()

library(broom)
library(gamlss)
dm_fit <- DirichletMultinomial::dmn(hit_matrix, 1)

tidy.DMN <- function(x, ...) {
  ret <- as.data.frame(x@fit)
  tbl_df(fix_data_frame(ret, c("conf.low", "estimate", "conf.high")))
}

dm_params <- tidy(dm_fit)

total <- sum(dm_params$estimate)

dirichlet_density <- hit_types_gathered %>%
  filter(type != "NonHit") %>%
  distinct(type) %>%
  inner_join(dm_params, by = c(type = "term")) %>%
  crossing(percent = seq(0, .3, .005)) %>%
  mutate(type = factor(type, hit_type_order)) %>%
  mutate(density = dbeta(percent, estimate, total - estimate))

hit_types_gathered %>%
  filter(AB > 500, type != "NonHit") %>%
  mutate(type = factor(type, hit_type_order)) %>%
  ggplot(aes(percent)) +
  geom_histogram(aes(y = ..density..), binwidth = .004) +
  geom_line(aes(y = density), color = "red", data = dirichlet_density) +
  facet_wrap(~type, scales = "free_y") +
  xlab("% of at-bats") +
  ylab("Density")


## Figure 10.4: Comparison of the raw slugging % and the shrunken slg% 
#               (for players at least 3 ABs). The mean of the prior
#               distribution is shown as a flat dashed line.

par_total <- sum(dm_params$estimate)
par <- dm_params %>%
  dplyr::select(term, estimate) %>%
  spread(term, estimate)

w <- c(1:4, 0)
slugging_mean <- sum(w * dm_params$estimate) / sum(dm_params$estimate)

hit_types_eb <- hit_types %>%
  mutate(slugging_eb = ((Single + par$Single) +
                          (Double + par$Double) * 2 +
                          (Triple + par$Triple) * 3 +
                          (HR + par$HR) * 4) /
           (AB + par_total))


hit_types_eb %>%
  filter(AB > 3) %>%
  ggplot(aes(slugging, slugging_eb, color = AB)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_hline(yintercept = slugging_mean, lty = 2, color = "red") +
  scale_color_continuous(trans = "log10", breaks = c(1, 10, 100, 1000, 10000)) +
  xlab("Slugging percentage") +
  ylab("Slugging percentage w/ empirical Bayes")
