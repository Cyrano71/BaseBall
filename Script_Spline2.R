source("InitData.R")

career_filtered <- career %>%
  filter(AB > 1000)

career2 <- career_filtered %>%
  filter(!is.na(bats)) %>%
  mutate(bats = relevel(bats, "R"))

leftHand <- 0 + (career2$bats == "L")
bothHand <- 0 + (career2$bats == "B")
rightHand <- 0 + (career2$bats == "R")

N <- dim(career2)[1]
year <- career2$year

library(splines)
knot.nb <- 5
spline <- ns(year, df = knot.nb)

z <- matrix(cbind(spline[,1], spline[,2], spline[,3], spline[,4], spline[,5]),N,knot.nb)

data <- list(N = N, AtBat = career2$AB, Hit = career2$H, 
             BatsLeft = leftHand, BatsBoth = bothHand, 
             nknots= knot.nb, Z = z)

myinits <- list(list(mu0 = 0.1, muAB = 0.01, muHandBoth = -0.01, muHandLeft = 0.01,
                     betaZ = rep(0,data$nknots), betaZB = rep(0,data$nknots), betaZL = rep(0,data$nknots)), 
                list(mu0 = 0.1, muAB = 0.01, muHandBoth = -0.01, muHandLeft = 0.01, 
                     betaZ = rep(0,data$nknots), betaZB = rep(0,data$nknots), betaZL = rep(0,data$nknots)) 
)

parameters <- c("mu0","sigma0", "muAB", "muHandBoth", "muHandLeft", "betaZ", "betaZB", "betaZL")

model.id <- 2
model.name <- paste0("/models/ModelSpline",model.id,".bug") 
model.path <- paste0(getwd(), model.name)

samples <- bugs(data,parameters,inits=myinits , model.file = model.path, 
 n.chains=2,n.iter= 6000, n.burnin=500, n.thin=10, DIC=T, 
bugs.directory=bugsdir, codaPkg=F, debug=T)

mu0 <- mean(samples$sims.list$mu0)
muAB <- mean(samples$sims.list$muAB)
muHandBoth <- mean(samples$sims.list$muHandBoth)
muHandLeft <- mean(samples$sims.list$muHandLeft)
sigma <- mean(samples$sims.list$sigma0)

betaZ1 <- mean(samples$sims.list$betaZ[,1])
betaZ2 <- mean(samples$sims.list$betaZ[,2])
betaZ3 <- mean(samples$sims.list$betaZ[,3])
betaZ4 <- mean(samples$sims.list$betaZ[,4])
betaZ5 <- mean(samples$sims.list$betaZ[,5])

betaZB1 <- mean(samples$sims.list$betaZB[,1])
betaZB2 <- mean(samples$sims.list$betaZB[,2])
betaZB3 <- mean(samples$sims.list$betaZB[,3])
betaZB4 <- mean(samples$sims.list$betaZB[,4])
betaZB5 <- mean(samples$sims.list$betaZB[,5])

betaZL1 <- mean(samples$sims.list$betaZL[,1])
betaZL2 <- mean(samples$sims.list$betaZL[,2])
betaZL3 <- mean(samples$sims.list$betaZL[,3])
betaZL4 <- mean(samples$sims.list$betaZL[,4])
betaZL5 <- mean(samples$sims.list$betaZL[,5])

dataPlot <- list(nknots= knot.nb, degree = 2, refyears = year,
mu0 = mu0, muAB = muAB, muHandLeft = muHandLeft,muHandBoth=muHandBoth, sigma = sigma,
Z = z, 
betaZ = c(betaZ1, betaZ2, betaZ3, betaZ4, betaZ5),
betaZB = c(betaZB1, betaZB2, betaZB3, betaZB4, betaZB5),
betaZL = c(betaZL1, betaZL2, betaZL3, betaZL4, betaZL5))

computeMean <- function(AB, year, bats, data){
  N <- length(year)
  mu0 <- data$mu0
  muAB <- data$muAB
  muHandLeft <- data$muHandLeft
  muHandBoth <- data$muHandBoth
  nknots <- data$nknots
  betaZB <- data$betaZB
  betaZL <- data$betaZL
  betaZ <- data$betaZ
  Zref <- data$Z
  yearsRef <- data$refyears
  mu <- c()
  for (i in 1:N)
  { 
      index <- which(yearsRef == year[i])
      Zi <- Zref[index[1],]
      z <- c()
      zl <- c()
      zb <- c()
      for(k in 1:nknots){
          z[k] <- betaZ[k] * Zi[k]
          zl[k] <- betaZL[k] * Zi[k]
          zb[k] <- betaZB[k] * Zi[k]
      }

      handSideLeft <- 0
      if(bats[i] == "L"){
        handSideLeft <- 1
      }

      handSideBoth <- 0
      if(bats[i] == "B"){
        handSideBoth <- 1
      }

      mu[i] <- mu0 + muAB * log(AB[i]) + muHandLeft * handSideLeft + muHandBoth * handSideBoth + sum(z) + sum(zl) * handSideLeft + sum(zb) * handSideBoth
   }

   return(mu)
}

plot_gamlss_fit <- function(dataPlot) {
  career2 %>%
    dplyr::select(year, bats) %>%
    mutate(AB = 1000) %>%
    mutate(mu = computeMean(AB, year, bats, dataPlot),
           sigma = dataPlot$sigma,
           alpha0 = mu / sigma,
           beta0 = (1 - mu) / sigma,
           conf_low = qbeta(.025, alpha0, beta0),
           conf_high = qbeta(.975, alpha0, beta0)) %>%
    filter(bats != "B") %>%
    ggplot(aes(year, mu, color = bats, group = bats)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high), linetype = 2, alpha = .1) +
    labs(x = "Year",
         y = "Prior distribution (median + 95% quantiles)",
         color = "Batting hand")
}

#debug(computeMean)
#undebug(computeMean)

plot_gamlss_fit(dataPlot)

#Posterior Distribution 
players <- crossing(year = c(1915, 1965, 2015),
                    bats = c("L", "R"),
                    H = 30,
                    AB = 100)

players_posterior <- players  %>%
mutate(mu = computeMean(AB, year, bats, dataPlot),
           sigma = dataPlot$sigma,
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
dbeta(1000,12.1,43.3)

 