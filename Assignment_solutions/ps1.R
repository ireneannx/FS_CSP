##Assignment 1 - Computational Statistics & Probability
## Irene Iype - irene.iype@fs-students.de

library(rethinking)
#Question 1 ---------------------------- 

#define grid
p_grid <- seq( from=0 , to=1 , length.out=1000 ) 
#define prior
prior <- rep( 1 , 1000 )
#compute liklihood at each value in grid
likelihood <- dbinom( 6 , size=9 , prob=p_grid ) 
#compute product of prior & liklihood
posterior <- likelihood * prior
#standardise posterior
posterior <- posterior / sum(posterior)
set.seed(215)
samples <- sample( p_grid , prob=posterior , size=10000 ,
                   replace=TRUE )
head(samples)
plot(samples ,
     ylab = 'proportion of water (p)',
     xlab = 'sample number')

#shows where most of the samples are likely to lie 
plot(p_grid, posterior,
     xlab = 'p (probability of water)' ,
     ylab = 'posterior probability')

#######
#a) How much posterior probability lies below p = 0.2?

#= number of samples less than 0.2 / total num of samples
sum( samples < 0.2 ) / 1e4 #can be calculated using mean()

#b) How much posterior probability lies above p = 0.8?
sum(samples >0.8) /1e4

#c) How much posterior probability lies between p = 0.2 and p = 0.8? d) 20% of the posterior probability lies below which value of p?
1 - mean( samples < 0.2 ) - mean(samples>0.8)

#d) 20% of the posterior probability lies below which value of p?
quantile(samples, 0.20)

#Question 2 ---------------------------- 
# Suppose the globe tossing experiment yielded the following sequence of 15 observations:
#   [W,L,W,W,L,L,W,L,W,L,L,W,L,W,W]


# a) Using grid approximation, construct the posterior distribution with the same flat
# prior as before.
p_grid <- seq( from=0 , to=1 , length.out=1000 )
flat_prior <- rep( 1 , 1000 )
# liklihood (binomial probability mass function)
# 15 tosses of the globe, returned W 8 times. Func returns prob of getting water based on p grid
prob_data <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- prob_data * flat_prior #posterior = prob of p conditional on data 
posterior <- posterior / sum(posterior)
samples_flat <- sample(p_grid, prob=posterior, size = 1000, replace= TRUE)
plot(p_grid, posterior,
     type = 'l',
     ylab = "posteritor probability",
     xlab = "probability of water")

# b) Using grid approximation, construct the posterior distribution with a prior that is
# 0 below p = 0.5 and constant above p = 0.5.
p_grid <- seq( from=0 , to=1 , length.out=1000 )
step_prior <- ifelse(p_grid < 0.5 , 0, 1)
prob_data <- dbinom( 8 , size=15 , prob=p_grid ) #liklihood
posterior <- prob_data * step_prior
samples_step <- sample(p_grid, prob=posterior, size = 1000, replace= TRUE)
plot(p_grid, posterior,
     type="l",
     ylab = "posterior probability",
     xlab = "probability of water")

# · What is the difference between these two models? How does each posterior 
#distribution compare to the true value of p = 0.7? Which prior is better and why?

#The difference can be seen from the shape of the posterior distributions constructed. 
#We can calculate the mean of the samples:
#For sample1 = 
mean(samples_flat) 
#For sample2 = 
mean(samples_step)
# The original value of p is around 0.7 in the real world. The mean of sample 2 is larger than 
# the mean of sample 1 since the probability of all values of p below 0.5 has been set to 0. 
# This pushes the 2nd posterior distribution closer to the original value of p. So, the mean
# of the 2nd set of samples is closer to true p compared to the mean of the first samples which 
#shows that the step prior. 

# This is also the case if we take a 99% interval of each distribution:
PI( samples_flat , prob=0.99 )
PI( samples_step , prob=0.99 )
# The 99% PI range is narrower and more precise for the 2nd posterior distribution. 
# The step prior is thus a better selection for the prior in this case.



#Question 3 ---------------------------- 

# Suppose you want a very precise estimate of the proportion of the Earth’s surface
# covered in water. Specifically, you want the 99% percentile interval of the posterior
# distribution of p to be only 0.05 wide, that is, the distance between the lower and
# upper bound on p should be 0.05. How many times will you have to toss the globe to do this ?
# A precise answer is unnecessary. I am primarily interested in your approach.

# To figure out the correct value of N (number of globe tosses), we can try the grid approximation 
# method with various values of N to see how the 99% PI interval changes.
# We can do this by creating a function first which would take a specific value of N:
  
f <- function(N){
  # num of similations = 1
  W <- rbinom(1, N, 0.7) #using prob of W (p) = 0.7, size = N, sampling from binom dist
  p_grid <- seq(from=0, to=1, length.out = 1e4)
  prior <- rep( 1 , 1000 )
  prob_data <- dbinom( W , size=N , prob=p_grid )
  posterior <- prob_data * prior
  posterior <- posterior / sum(posterior)
  samples <- sample(p_grid, prob=posterior, size = 1000, replace= TRUE)
  
  #calculating 99% interval width
  PIval = PI(samples, prob = 0.99)
  as.numeric(PIval[2]-PIval[1])
}


#trying different values of N
Nvals <- c(50, 100, 200, 400, 800, 1600, 2000, 2500)
width <- sapply(Nvals, f)

#plotting values
plot(Nvals, width, xlab= 'N', ylab= 'width')

#width at 0.05
abline(h = 0.05)

#As we can see, N needs to be somewhere between 2000 and 2500 in order for the 
#99% interval to have a width of 0.05
