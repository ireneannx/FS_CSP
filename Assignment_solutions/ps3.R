#Assignment 3 - Computational & Statistical Probability 
#Irene Ann Iype - irene.iype@fs-students.de

library(rethinking)

# Q1a
#Does territory size have a causal influence the weight of foxes? 
#Construct a quap model to infer the total causal influence of area on weight. 
#Does increasing the area available to each fox make it healthier (i.e., heavier)?

data(foxes)
d <- foxes
precis( d )
d

library(dagitty)
dag1 <- dagitty("dag{area -> avgfood -> weight; avgfood -> groupsize ->weight}")
plot(dag1)
impliedConditionalIndependencies(dag1)

# area _||_ grps | avgf
# area _||_ wght | avgf

# We look at the relationship between area and weight. From the DAG provided, the paths are:  
#area -> avgfood -> weight (open path)
#area -> avgfood -> groupsize -> weight (open path)

#Both are causal paths with no counfounding. i.e the estimate of the effect of area on weight is 
# not confounded by any other variable. No backdoor paths because there are no arrows entering area.

#Doing prior predictive simulation to show that the priors used make sense 
set.seed(2971)
N <- 100                  
a <- rnorm( N , 0 , 0.2 )
bA <- rnorm( N , 0 , 0.5 )

plot( NULL , xlim=range(d$area) , ylim=c(-3,3) ,
      xlab="standardised area" , ylab="standardised weight" )

#minimum & maximum line 
abline( h= max(d$weight) , col=2 ) #max weight
abline( h= min(d$weight) , col=3 ) #min weight

mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d$area)
for ( i in 1:N ) curve( a[i] + bA[i]*x,
                        from=min(d$area) , to=max(d$area) , add=TRUE ,
                        col=col.alpha("black",0.2) )


#Using multiple regression

#standardise variables - mean 0, sd1
d$area <- standardize( d$area )
d$weight <- standardize( d$weight )
d

m1a <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + bA * area ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

precis(m1a)
# mean   sd  5.5% 94.5%
# a     0.00 0.08 -0.13  0.13
# bA    0.02 0.09 -0.13  0.16
# sigma 0.99 0.06  0.89  1.09

# bA is 0.02 which shows there is no correlation between area and weight. No causal effect of area on weight.

# Q1b 
# Now infer the causal impact of adding food (avgfood) to a territory. 
# Would this make foxes heavier? Which covariates do you need to adjust to estimate the total
# causal influence of food?

#We look at the relationship between avgfood and weight. 
# Paths: 
# 1) avgfood -> weight (open path)
# 2) avgfood -> groupsize -> weight (open path)

# No backdoors since there are no arrows entering avgfood. The effect of food on weight can be 
# inferred by considering both the paths above.

#standardising food
d$avgfood <- standardize(d$avgfood)

#using multiple regression like before
m1b <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + bF * avgfood ,
    a ~ dnorm( 0 , 0.2 ) ,
    bF ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ), data = d )

precis(m1b)
#       mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bF    -0.02 0.09 -0.17  0.12
# sigma  0.99 0.06  0.89  1.09

# bF is almost 0. Once again, there is no causal relationship between the average food and weight. Intuitively, 
# we would think that avgfood would impact weight, however considering the DAG provided this result is expected 
# since area impacts food. And since area does not show any correlation with weight, the same can be expected 
# for food.

#Q1c 
# Now infer the causal impact of group size (groupsize). Which covariates do you need to adjust
# to make this estimate? Inspect the posterior distribution of the resulting model. What do you
# think explains these data? Specifically, explain the estimates of the effects of area, avgfood,
# and groupsize on weight. How do they make sense together? (Hint: we covered an example in class
# which exhibited a similar relationship between predictors and outcome variable.)

#there are 2 paths between group size and weight 
# 1) groupsize -> weight (open)
# 2) groupsize <- avgfood -> weight (backdoor path)

# the second one is a backdoor path since there is an arrow going into groupsize (from avgfood). So we
# need to close this backdoor -> we need to use avgfood in our model as well 

#standardise 
d$groupsize <- standardize(d$groupsize)

#using standard priors from book 
m1c <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + bG*groupsize + bF*avgfood ,
    a ~ dnorm( 0 , 0.2 ) ,
    bF ~ dnorm(0, 0.5),
    bG ~ dnorm(0, 0.5),
    sigma ~ dexp( 1 )
  ),
  data=d )

precis( m1c )
# mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bF     0.48 0.18  0.19  0.76
# bG    -0.57 0.18 -0.86 -0.29
# sigma  0.94 0.06  0.84  1.04

#From the results in m1c, we can see that bG = -0.57 which indicates that group size is 
# correlated negatively with weight (when food is held constant) while food is showing 
# a positive correlation with weight (when group size is held constant). This makes sense 
# since more food means more food per fox and thus increased weight while a larger group size means 
# less food per fox (more sharing) which leads to a decrease in weight.
# 
# However 1b showed that food doesnt have any correlation with weight and this is because its effects
# are being cancelled out through groupsize. The total causal effect of food is minimal since it also causes 
# larger groups. This shows a masking effect.
# 
# The causal explanation is that as food increases, the groupsize increases until the food per fox is more 
# or less the same among all territories. 

#------------------------------------------------------------------------------
# Q2
# Explain the difference between model selection and model comparison. What information
#is lost under model selection?

# Model selection has to do with choosing criteria which help in choosing model structure. A common form 
# of model selection is to choose a model which has the best information criteria value or where every coefficient
# is statistically significant ('stargazing' as mentioned in the book). However, this kind of selection procedure 
#discards the information about relative model accuracy contained in the differences among the CV/PSIS/WAIC values.

# Another downside to model selection using these criterions alone is that they dont tell you anything about the causal
# structure of the parameters. Its is totally possible for a model to be a great predictor or forcaster but be
# highly confounded. These models do not help us if our aim is to make some intervention and figure out what the
# result of these interventions might be. So it can be especially problematic when one model outperforms its alternatives
# to a small degree and we simply pick it purely on its IC values.

# On the plus side, these criteria do account for overfitting and help in testing model implications, given a set of causal models. 
# This leads to a better approach: Model comparison, which is a more general approach where we use multiple models to understand
# how different variables influence predictions. This, along with a causal model with implied conditional independencies among
# variables help us infer causal relationships. 

#------------------------------------------------------------------------------
#Q3
# Can you explain the relative differences in WAIC scores, using the fox DAG from
# above? Be sure to pay attention to the standard error of the score differences (dSE).

# Constructing the models 
data(foxes)
d <- foxes 

#standardising weights
d$weight <- standardize(d$weight)
d$area <- standardize(d$area)
d$avgfood <- standardize(d$avgfood)
d$groupsize <- standardize(d$groupsize)

#model 1: avgfood + groupsize + area

m1 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bF*avgfood + bG*groupsize + bA*area, #mlr
    a ~ dnorm(0,0.2),
    c(bF,bG,bA) ~ dnorm(0,0.5), #using priors from earlier
    sigma ~ dexp(1)
  ), data=d )

#model 2: avgfood + groupsize

m2 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bF*avgfood + bG*groupsize, #mlr
    a ~ dnorm(0,0.2),
    c(bF,bG) ~ dnorm(0,0.5), #using priors from earlier
    sigma ~ dexp(1)
  ), data=d )

#model 3: avgfood + area
m3 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bF*avgfood + bA*area, #mlr
    a ~ dnorm(0,0.2),
    c(bF,bA) ~ dnorm(0,0.5), #using priors from earlier
    sigma ~ dexp(1)
  ), data=d )

#model 4: avgfood 
m4 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bF*avgfood, #mlr
    a ~ dnorm(0,0.2),
    bF ~ dnorm(0,0.5), #using priors from earlier
    sigma ~ dexp(1)
  ), data=d )

#model 5: area
m5 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA*area, #mlr
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5), #using priors from earlier
    sigma ~ dexp(1)
  ), data=d )

compare(m1,m2,m3,m4,m5)

# WAIC      SE    dWAIC  dSE  pWAIC weight
# m1 323.6 16.41   0.0   NA   5.0   0.51
# m2 323.7 16.12   0.2 3.57   3.7   0.48
# m4 333.4 13.78   9.8 7.27   2.4   0.00
# m5 333.5 13.81   9.9 7.32   2.5   0.00
# m3 334.6 14.09  11.0 7.06   3.6   0.00

# Columns from left to right are: WAIC, standard error (SE) of WAIC, difference of each WAIC from the best model, standard error (dSE) of this difference, prediction penalty (pWAIC), and finally the Akaike weight
# 
# The best models based on the WAIC values are m1 & m2. Their WAIC values are also 
# very close to each other. The dWAIC column is the difference between each model’s WAIC and the best WAIC in the set.To figure out if these models can be easily distinguished by their
# out of sample accuracy, we need to consider the error in the WAIC estimates. For this we use SE & dSE. 

plot( compare( m1 , m2 , m3, m4, m5 ) )

#The differences of in the WAIC scores all fall well within the 99% intervals of the differences

#m1 and m2 are very close to each other as shown in the plot for their out of sample deviance. This makes sense since both of these models have groupsize in it which means we can end up with the same results 
#when including the other parameters. This has to do with the DAG structure from the previous question. The first model includes food, area and groupsize while the second only involves 
#food and groupsize. However, food has a direct causal relationship to area which is routed entirely through food. So the affect of area while adjusting for groupsize is the same as the effect of food while adjusting 
#for groupsize. Including groupsize essentially closes the backdoor path so both models have similar predictive power.

#m4,5,3 also perform similarly. This is because they all do not include groupsize. So for model 5(only area) would be similar to model 4(only food) since area has to pass through food -> leads to the same effect.
#model 3 includes both area and food, but again because of the way area is routed through food, this also leads to a very similar result as the other two. 
#The total causal influence of area or food are about 0 as shown in Q1a and b. 



