source("InitData.R")

career_filtered <- career %>%
  filter(AB > 1000)

data <- list(N = dim(career_filtered)[1], AtBat = career_filtered$AB, Hit = career_filtered$H)

myinits <- list(list(mu0 = 0.14, muAB = 0.0153), 
                list(mu0 = 0.14, muAB = 0.0153)) 

parameters <- c("mu0","muAB","sigma0")

model.id <- ""
model.name <- paste0("/models/ModelBetaBinomialRegression",model.id,".bug") 
model.path <- paste0(getwd(), model.name)

samples <- bugs(data,parameters,inits=myinits , model.file = model.path, 
 n.chains=2,n.iter= 5000, n.burnin=500, n.thin=20, DIC=T, 
bugs.directory=bugsdir, codaPkg=F, debug=T)

mu0 <- mean(samples$sims.list$mu0)
muAB <- mean(samples$sims.list$muAB)
sigma0 <- mean(samples$sims.list$sigma0)

data <- tibble(
  AB = c(1, 10, 100, 1000, 10000),
  Mu0 = mu0,
  MuAB = muAB,
  Sigma0 = sigma0
)

data  %>%
   crossing(x = seq(0, .36, .0005)) %>%
   mutate(mu = Mu0 + MuAB * log(AB),
         sigma = Sigma0,
         alpha = mu / sigma,
         beta = (1 - mu) / sigma,
         density = dbeta(x, alpha, beta))    %>%
    ggplot(aes(x, density, color = factor(AB))) +
    geom_line() +
      labs(x = "Batting average",
       y = "Prior density",
       color = "AB")
