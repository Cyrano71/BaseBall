source("InitData.R")

career_filtered <- career %>%
  filter(AB > 1000)

career2 <- career_filtered %>%
  filter(!is.na(bats)) %>%
  mutate(bats = relevel(bats, "R"))

leftHand <- 0 + (career2$bats == "L")
bothHand <- 0 + (career2$bats == "B")
rightHand <- 0 + (career2$bats == "R")

data <- list(N = dim(career2)[1], AtBat = career2$AB, Hit = career2$H, 
BatsLeft = leftHand, BatsBoth = bothHand)

myinits <- list(list(mu0 = 0.1, muAB = 0.01, muHandBoth = 0, muHandLeft = 0.01), 
                list(mu0 = 0.1, muAB = 0.01, muHandBoth = 0, muHandLeft = 0.01)) 

parameters <- c("mu0","muAB","sigma0","muHandBoth","muHandLeft")

model.name <- paste0("/models/ModelHierarchical",".bug") 
model.path <- paste0(getwd(), model.name)

samples <- bugs(data,parameters,inits=myinits , model.file = model.path, 
 n.chains=2,n.iter= 5000, n.burnin=500, n.thin=20, DIC=T, 
bugs.directory=bugsdir, codaPkg=F, debug=T)

mu0 <- mean(samples$sims.list$mu0)
muAB <- mean(samples$sims.list$muAB)
muHandBoth <- mean(samples$sims.list$muHandBoth)
muHandLeft <- mean(samples$sims.list$muHandLeft)
sigma0 <- mean(samples$sims.list$sigma0)

R <- 0
L <- 1

side <- function(bats){
 output <- c()
 j <- 1
 for(i in 1:length(bats)){
   	if(bats[i] == 0){
   	  output[j] <- "R"
 	}
 	else {
   	  output[j] <- "L"
 	}
  j <- j + 1
 }
 return(output)
}

crossing(bats = c(R, L),
         AB = c(10, 100, 1000, 10000),
         muHandLeft = muHandLeft,
         muAB = muAB, mu0 = mu0,
         sigma = sigma0) %>%
  mutate(H = .3 * AB,
         batsChr = side(bats),
         mu = mu0 + muAB * log(AB) + muHandLeft * bats,
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H,
         estimate = alpha1 / (alpha1 + beta1),
         conf.low = qbeta(.025, alpha1, beta1),
         conf.high = qbeta(.975, alpha1, beta1),
         record = paste(H, AB, sep = " / ")) %>%
  ggplot(aes(estimate, record, color = batsChr )) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  labs(x = "Estimate w/ 95% credible interval",
       y = "Batting record",
       color = "Batting hand")
