library(ggplot2)
library(dplyr)
library(tidyr)
library(Lahman)
library(bayestestR)
library(dplyr)
library(R2WinBUGS)
bugsdir <- "C:/Program Files/WinBUGS14"

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

career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

starting_data <- career %>%
  filter(AB >= 20) %>%
  select(-year, -bats, -isPitcher) %>%
  mutate(cluster = factor(sample(c("A", "B"), n(), replace = TRUE)))

N <- dim(starting_data)[1]

data <- list(N = N, AtBat = starting_data$AB, Hit = starting_data$H)

myinits <- list(
       list(alpha_nonpitcher = 81, beta_nonpitcher = 219, alpha_pitcher = 30, beta_pitcher = 140, z = round(runif(N))), 
       list(alpha_nonpitcher = 81, beta_nonpitcher = 219, alpha_pitcher = 25, beta_pitcher = 120, z = round(runif(N)))
)

parameters <- c("alpha_pitcher","beta_pitcher", "alpha_nonpitcher", "beta_nonpitcher")

model.name <- paste0("/models/ModelMixture",".bug") 
model.path <- paste0(getwd(), model.name)

samples <- bugs(data,parameters,inits=myinits , model.file = model.path, 
 n.chains=2,n.iter= 6000, n.burnin=500, n.thin=10, DIC=T, 
bugs.directory=bugsdir, codaPkg=F, debug=T)

alpha_pitcher <- mean(samples$sims.list$alpha_pitcher)
alpha_nonpitcher <- mean(samples$sims.list$alpha_nonpitcher)
beta_pitcher <- mean(samples$sims.list$beta_pitcher)
beta_nonpitcher <- mean(samples$sims.list$beta_nonpitcher)

final_parameters <- tibble(alpha = c(alpha_pitcher, alpha_nonpitcher), beta = c(beta_pitcher, beta_nonpitcher), cluster = c("A","B"))

batter_100 <- career %>%
  filter(AB == 100) %>%
  arrange(average) %>%
  select(-playerID, -bats)

final_parameters %>%
  crossing(H = 1:40) %>%
  transmute(H, cluster, likelihood = VGAM::dbetabinom.ab(H, 100, alpha, beta)) %>%
  spread(cluster, likelihood) %>%
  mutate(probability_A = A / (A + B)) %>%
  ggplot(aes(H, probability_A)) +
  geom_line() +
  geom_vline(aes(xintercept = H), data = batter_100, lty = 2) +
  geom_text(aes(x = H, y = 0, label = name), data = batter_100, hjust = 1, vjust = 1, angle = 270) +
  labs(x = "H (out of 100 at-bats)",
       y = "(Likelihood if pitcher) / (Likelihood if pitcher + Likelihood if not)")

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