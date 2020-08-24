source("InitData.R")

career_filtered <- career %>%
  filter(AB > 1000)

career2 <- career_filtered %>%
  filter(!is.na(bats)) %>%
  mutate(bats = relevel(bats, "R"))

leftHand <- 0 + (career2$bats == "L")
bothHand <- 0 + (career2$bats == "B")
rightHand <- 0 + (career2$bats == "R")

knot.nb <- 5
knot.quantile <- quantile(career2$year)
knot <- c()
for(i in 1:knot.nb){
 knot[i] <- knot.quantile[i][[1]]
}

data <- list(N = dim(career2)[1], AtBat = career2$AB, Hit = career2$H, 
BatsLeft = leftHand, BatsBoth = bothHand, Year = career2$year,
knot = knot, nknots= knot.nb, degree = 2)

myinits <- list(list(mu0 = 0.14, muAB = 0.0153, muHandBoth = 0, muHandLeft = 0, betaX = rep(0,data$degree+1), betaZ = rep(0,data$nknots)), 
                list(mu0 = 0.14, muAB = 0.0153, muHandBoth = 0, muHandLeft = 0, betaX = rep(0,data$degree+1), betaZ = rep(0,data$nknots)) 
)

parameters <- c("sigma0", "muAB", "muHandBoth", "muHandLeft", "betaX", "betaZ")

model.id <- ""
model.name <- paste0("/models/ModelSpline",model.id,".bug") 
model.path <- paste0(getwd(), model.name)

samples <- bugs(data,parameters,inits=myinits , model.file = model.path, 
 n.chains=2,n.iter= 5000, n.burnin=500, n.thin=20, DIC=T, 
bugs.directory=bugsdir, codaPkg=F, debug=T)

mu0 <- mean(samples$sims.list$betaX[,l])
muAB <- mean(samples$sims.list$muAB)
muHandBoth <- mean(samples$sims.list$muHandBoth)
muHandLeft <- mean(samples$sims.list$muHandLeft)
sigma0 <- mean(samples$sims.list$sigma0)

betaX2 <- mean(samples$sims.list$betaX[,2])
betaX3 <- mean(samples$sims.list$betaX[,3])

betaZ1 <- mean(samples$sims.list$betaZ[,1])
betaZ2 <- mean(samples$sims.list$betaZ[,2])
betaZ3 <- mean(samples$sims.list$betaZ[,3])
betaZ4 <- mean(samples$sims.list$betaZ[,4])
betaZ5 <- mean(samples$sims.list$betaZ[,5])

side <- function(bats){
 output <- c()
 j <- 1
 for(i in 1:length(bats)){
   	if(bats[i] == "R"){
   	  output[j] <- 0
 	}
 	else {
   	  output[j] <- 1
 	}
  j <- j + 1
 }
 return(output)
}

zfunction <- function(betaZ1, betaZ2, betaZ3, betaZ4, betaZ5){


}

plot_gamlss_fit <- function(mu,sigma, knots) {
  career2 %>%
    dplyr::select(year, bats) %>%
    distinct() %>%
    filter(bats != "B") %>%
    mutate(AB = 1000, batsBool = side(bats), sigma = sigma0,
           mu0 = mu0, muAB = muAB,muHandLeft = muHandLeft,
           betaX2 = betaX2, betaX3 = betaX3,
           betaZ1 = betaZ1, betaZ2 = betaZ2, betaZ3 = betaZ3, betaZ4 = betaZ4, betaZ5 = betaZ5) %>%
    mutate(sigma = sigma,
           mu = mu0 + muAB * log(AB) + muHandLeft * batsBool + betaX2 * year + betaX3 * year^2 + ,
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

plot_gamlss_fit(samples$sims.list$mu, samples$sims.list$sigma0)
