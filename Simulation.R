library(dplyr)
library(MASS)

install.packages('lmerTest')
library(lmerTest)

set.seed(1)
# Generate id
dat = data.frame(id = paste0(615,1:100))
# Make baseline age variable
dat$baseline.age = runif(100,40,80)
# Simulate the exposure
dat$X = rnorm(100,25,4)
# Long format: each subject was followed by 10 years
dat = dat[rep(1:100, each = 10), ]
# Make time on study variable
dat$time = rep(1:10, times = 100)
# Simulate random error
dat$err = rnorm(1000,mean=0,sd=1)

# Simulate (correlated) subject-level random effects for intercepts and slopes
## Covariance matrix 
S1 = diag(c(2, 0.5)) %*% matrix(c(1, -0.1, -0.1, 1), nrow=2) %*% diag(c(2, 0.5))
## Simulate realization of random intercept and slope
U1 = as.data.frame(mvrnorm(100, mu=c(0,0), Sigma=S1))
## Add identifier (subject id)
U1$id = paste0(615,1:100)
## Merge subject-level random effects back to data
dat = merge(dat,U1,by='id')

# Simulate the outcome
dat$Y = (100 + dat$V1) + (-1+dat$V2)*dat$time - dat$baseline.age + dat$err

attach(dat)
plot(time,Y,type="l")


data(sleepstudy)
lmer(Reaction ~ Days + (Days | Subject), sleepstudy)


###################################################################
### A simple example: simulation

# We vary sample size from 5 to 40
p.matrix = c()
for (N in seq(5,40,by=5)) {
  p.temp.vector = c()
  # We repeat simulation for 500 times for each N (Step 3)
  for (i in 1:500) {
    # Set seed
    set.seed(i)
    # Step 1: generate data
    # Generate id 
    dat = data.frame(id = paste0(615,1:N))
    # Long format: each subject was followed by 10 days
    dat = as.data.frame(dat[rep(1:N, each = 10), ])
    names(dat) = 'id'
    # Make Day variable
    dat$Days = rep(1:10, times = N)
    # Simulate random error
    dat$err = rnorm(10*N,mean=0,sd=25.592)
    
    # Simulate (correlated) subject-level random effects for intercepts and slopes
    ## Covariance matrix
    S1 = diag(c(24.741, 5.922)) %*% matrix(c(1, 0.07, 0.07, 1), nrow=2) %*% diag(c(24.741, 5.922))
    ## Simulate realization of random intercept and slope
    U1 = as.data.frame(mvrnorm(N, mu=c(0,0), Sigma=S1))
    ## Add identifier (subject id)
    U1$id = paste0(615,1:N)
    ## Merge subject-level random effects back to data
    dat = merge(dat,U1,by='id')
    
    # Simulate the outcome: Reaction_ij
    dat$Reaction = (251.405 + dat$V1) + (5 + dat$V2)*dat$Days + dat$err
    
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

plot(seq(5,40,by=5),power,xlab = "Sample size (N)",ylab="Power",type = "b", pch = 19)
abline(h=0.8,lty=2)


