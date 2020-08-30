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

hit_types_gathered <- hit_types %>%
  select(-H) %>%
  gather(type, value, -playerID, -name, -AB) %>%
  mutate(percent = value / AB)

hit_type_order <- c("Single", "Double", "Triple", "HR")

hit_500 <- hit_types %>%
  filter(AB >= 500)

N <- dim(hit_500)[1]

hit_matrix <- as.data.frame(hit_500 %>%
  select(Single, Double, Triple, HR, NonHit) %>%
  as.matrix())

HitTypes <- matrix(cbind(hit_matrix[,"Single"],hit_matrix[,"Double"],hit_matrix[,"Triple"],hit_matrix[,"HR"],hit_matrix[,"NonHit"]), N, 5)

data <- list(N = N, K = 5,AtBat = hit_500$AB, HitTypes = HitTypes)

myinits <- list(list(alpha=c(mean(hit_matrix[,"Single"]), mean(hit_matrix[,"Double"]), mean(hit_matrix[,"Triple"]), mean(hit_matrix[,"HR"]), mean(hit_matrix[,"NonHit"]))), 
               list(alpha=c(mean(hit_matrix[,"Single"]), mean(hit_matrix[,"Double"]), mean(hit_matrix[,"Triple"]), mean(hit_matrix[,"HR"]), mean(hit_matrix[,"NonHit"])))
)

parameters <- c("alpha")

model.name <- paste0("/models/ModelDirichlet",".bug") 
model.path <- paste0(getwd(), model.name)

samples <- bugs(data,parameters,inits=myinits , model.file = model.path, 
 n.chains=2,n.iter= 6000, n.burnin=500, n.thin=10, DIC=T, 
bugs.directory=bugsdir, codaPkg=F, debug=T)

alphaSingle <- mean(samples$sims.list$alpha[,1])
alphaDouble <- mean(samples$sims.list$alpha[,2])
alphaTriple <- mean(samples$sims.list$alpha[,3])
alphaHR <- mean(samples$sims.list$alpha[,4])
alphaNonHit <- mean(samples$sims.list$alpha[,5])

data <- list(term = c("Single", "Double", "Triple", "HR", "NonHit"), estimate=c(alphaSingle, alphaDouble, alphaTriple, alphaHR, alphaNonHit))

dm_params <-tbl_df(as.data.frame(data))

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