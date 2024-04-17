####### Changes:
## assume you only collect data for 8 nights (not 10) data from 40 subjects
## you anticipate weaker association between Day and Reaction, that is, β1=3
## you anticipate larger error due to heterogeneity of the new sample, that is, ϵij∼N(0,30^2)
## you anticipate random intercept and random slope to be independent, that is, cor(b0,i,b1,i)=0

# Code Review by Elise Chen on 04/17/2024
# Comments: 1. could not find function mvrnorm
#           2. could not find function lmer
#           3. could not compile the code 



###################################################################
### Problem 1

library(MASS) ### Elise added
library(lme4) ### Elise added
# We keep sample size 40
p.matrix = c()
for (N in 40) { #gdr change: 40 instead of seq(5,40,by=5)
  p.temp.vector = c()
  # We repeat simulation for 500 times for each N (Step 3)
  for (i in 1:500) {
    # Set seed
    set.seed(i)
    # Step 1: generate data
    # Generate id 
    dat = data.frame(id = paste0(615,1:N))
    # Long format: each subject was followed by 8 days
    dat = as.data.frame(dat[rep(1:N, each = 8), ]) #gdr change: 8 instead of 10
    names(dat) = 'id'
    # Make Day variable
    dat$Days = rep(1:8, times = N) #gdr change: 8 instead of 10
    # Simulate random error
    dat$err = rnorm(8*N,mean=0,sd=30) #gdr change: ϵij∼N(0,30^2) instead of sd=25.592, change 8 instead of 10
    
    # Simulate (correlated) subject-level random effects for intercepts and slopes
    ## Covariance matrix
    S1 = diag(c(24.741, 5.922)) %*% matrix(c(1, 0, 0, 1), nrow=2) %*% diag(c(24.741, 5.922)) #gdr change: cor(b0,i,b1,i)=0 instead of 0.07
    ## Simulate realization of random intercept and slope
    U1 = as.data.frame(mvrnorm(N, mu=c(0,0), Sigma=S1))
    ## Add identifier (subject id)
    U1$id = paste0(615,1:N)
    ## Merge subject-level random effects back to data
    dat = merge(dat,U1,by='id')
    
    # Simulate the outcome: Reaction_ij
    dat$Reaction = (251.405 + dat$V1) + (3 + dat$V2)*dat$Days + dat$err #gdr change: β1=3 instead of 5
    
    # Step 2: test the null hypothesis
    mod = lmer(Reaction ~ Days + (Days | id), dat)
    p.value = summary(mod)$coef["Days","Pr(>|t|)"]
    # Save p value
    p.temp.vector = c(p.temp.vector,p.value)
  }
  # Save p value vector for each N
  p.matrix = cbind(p.matrix,p.temp.vector)
}
# Step 4: calculate power
power = apply(p.matrix,2,function(x) mean(x<0.05))

power
#p.temp.vector 
#0.698 


