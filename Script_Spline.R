source("InitData.R")

career_filtered <- career %>%
  filter(AB > 1000)

career2 <- career_filtered %>%
  filter(!is.na(bats)) %>%
  mutate(bats = relevel(bats, "R"))

leftHand <- 0 + (career2$bats == "L")
bothHand <- 0 + (career2$bats == "B")
rightHand <- 0 + (career2$bats == "R")

knot.nb <- 3
knot.quantile <- quantile(career2$year)
knot <- c()
knot[1] <- 1933.000
knot[2] <- 1972.389
knot[3] <- 1998.619

N <- dim(career2)[1]
degree <- 2
year <- career2$year

z <- matrix(numeric(),N,knot.nb)
for (i in 1:N)
{
  for (k in 1:knot.nb)
  {
	if(year[i]-knot[k] > 0){
	     u <- (year[i]-knot[k])
	     z[i,k] <- u^degree
      }else {
	     z[i,k] <- 0
      }
  }
}

x <- matrix(numeric(),N,2)
for (i in 1:N)
{
  x[i,1] <- year[i]^0
  x[i,2] <- year[i]
}

data <- list(N = N, AtBat = career2$AB, Hit = career2$H, 
BatsLeft = leftHand, BatsBoth = bothHand, 
nknots= knot.nb, degree = degree, X = x, Z = z)

myinits <- list(list(mu0 = 0.14, muAB = 0.0153, muHandBoth = 0, muHandLeft = 0, betaX = c(0,0), betaZ = rep(0,data$nknots)), 
                list(mu0 = 0.14, muAB = 0.0153, muHandBoth = 0, muHandLeft = 0, betaX = c(0,0), betaZ = rep(0,data$nknots)) 
)

parameters <- c("mu0","sigma0", "muAB", "muHandBoth", "muHandLeft", "betaX", "betaZ")

model.id <- 1
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

betaX1 <- mean(samples$sims.list$betaX[,1])
betaX2 <- mean(samples$sims.list$betaX[,2])

betaZ1 <- mean(samples$sims.list$betaZ[,1])
betaZ2 <- mean(samples$sims.list$betaZ[,2])
betaZ3 <- mean(samples$sims.list$betaZ[,3])
betaZ4 <- mean(samples$sims.list$betaZ[,4])
betaZ5 <- mean(samples$sims.list$betaZ[,5])

dataPlot <- list(knot = knot, nknots= knot.nb, degree = 2,
mu0 = mu0, muAB = muAB, muHandLeft = muHandLeft, sigma = sigma,
betaX = c(betaX1, betaX2), betaZ = c(betaZ1, betaZ2, betaZ3, betaZ4, betaZ5))

ConvertHandSideStringToBool <- function(handSide){
 output <- c()
 j <- 1
 for(i in 1:length(handSide)){
   	if(handSide[i] == "R"){
   	  output[j] <- 0
 	}
 	else {
   	  output[j] <- 1
 	}
  j <- j + 1
 }
 return(output)
}

computeMean <- function(AB, year, handSide, data){
  N <- length(year)
  mu0 <- data$mu0
  muAB <- data$muAB
  muHandLeft <- data$muHandLeft
  nknots <- data$nknots
  knot <- data$knot
  betaX <- data$betaX
  betaZ <- data$betaZ
  mu <- c()
  for (i in 1:N)
  {
      x <- c()
      x[1] <- 0 #betaX[1]
      x[2] <- betaX[2] * year[i]

      u <- c()
      z <- c()
      for (k in 1:nknots)
      {
	    if(year[i] - knot[k] > 0){
                 u[k] <- year[i] - knot[k]
                 z[k] <- betaZ[k] * u[k]^2
           }else{
      		     u[k] <- 0
	           z[k] <- 0
           }  	
      }

      mu[i] <- mu0 + muAB * log(AB[i]) + muHandLeft * handSide[i] + sum(x) + sum(z)
   }

   return(mu)
}

plot_gamlss_fit <- function(dataPlot) {
  career2 %>%
    dplyr::select(year, bats) %>%
    distinct() %>%
    filter(bats != "B") %>%
    mutate(AB = 1000, handSide = ConvertHandSideStringToBool(bats)) %>%
    mutate(mu = computeMean(AB, year, handSide, dataPlot),
           sigma = dataPlot$sigma,
           alpha0 = mu / sigma,
           beta0 = (1 - mu) / sigma,
           conf_low = qbeta(.025, alpha0, beta0),
           conf_high = qbeta(.975, alpha0, beta0)) %>%
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