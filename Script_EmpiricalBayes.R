source("InitData.R")

career_filtered <- career %>%
  filter(AB > 500)

data <- list(N = dim(career_filtered)[1], AtBat = career_filtered$AB, Hit = career_filtered$H)

myinits <- list(list(alpha = 100, beta = 275), 
                list(alpha = 100, beta = 275)) 

parameters <- c("p","alpha","beta")

model.id <- 2
model.name <- paste0("/models/ModelEmpiricalBayes",model.id,".bug") 
model.path <- paste0(getwd(), model.name )

samples <- bugs(data,parameters,inits=myinits , model.file = model.path, 
 n.chains=2,n.iter= 5000, n.burnin=500, n.thin=20, DIC=T, 
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

if(model.id == 2){
  pAaron <-   samples$sims.list$p[,which(career_filtered$name == "Hank Aaron")]
  hist(pAaron)
  PEP_Aaron <- sum(pAaron < 0.3) / length(pAaron)
}



