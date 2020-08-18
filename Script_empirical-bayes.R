library(ggplot2)
library(dplyr)
library(tidyr)
library(Lahman)
library(bayestestR)
library(dplyr)
library(R2WinBUGS)
bugsdir <- "C:/Program Files/WinBUGS14"

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
  inner_join(career, by = "playerID") %>%
  dplyr::select(-playerID)

career_filtered <- career %>%
  filter(AB > 500)

data <- list(N = dim(career_filtered)[1], AtBat = career_filtered$AB, Hit = career_filtered$H)

myinits <- list(list(alpha = 100, beta = 275), 
                list(alpha = 100, beta = 275)) 

parameters <- c("p","alpha","beta")

model.path <- paste0(getwd(),"/models/ModelEmpiricalBayes.bug")

samples <- bugs(data,parameters,inits=myinits , model.file = model.path, 
 n.chains=2,n.iter= 8000, n.burnin=500, n.thin=20, DIC=T, 
bugs.directory=bugsdir, codaPkg=F, debug=T)

alpha0 <- mean(samples$sims.list$alpha)
beta0 <- mean(samples$sims.list$beta)

career_filtered %>%
  filter(AB > 500) %>%
  ggplot() +
  geom_histogram(aes(average, y = ..density..), binwidth = .005) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  xlab("Batting average")

career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))

