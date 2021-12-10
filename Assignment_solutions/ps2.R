##Assignment 2 - Computational Statistics & Probability
## Irene Iype - irene.iype@fs-students.de

library(rethinking)
#Question 1 ---------------------------- 
#Consider the following model from the textbook to use the !Kung census to predict height from weight of adults.
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
xbar <- mean(d2$weight) #defining the avg weight

#fitting model
m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - xbar ),
    a ~ dnorm( 178 , 20 ),
    b ~ dlnorm( 0 , 1 ),
    sigma ~ dunif( 0 , 50 ) ),
  data=d2 )

precis( m4.3 )

#Using this model, provide the predicted heights and 89% credibility intervals for each of the following individuals:
#Individual, weight, expected height, 89% interval
#1 46 
#2 61 
#3 35 
#4 52 
#5 56

#For each of the numbers given above, we make a posterior prediction for the model.
weights <-  c(46, 61, 35, 52, 56)
w <- data.frame(weight = weights) #data frame of results

sim.height <- sim( m4.3 , data = w) 
Eheight <- apply(sim.height, 2, mean)
height.CI <- apply( sim.height , 2 , PI , prob=0.89 )

#table form
w$Expected_height <- Eheight
w$L89 <- height.CI[1,]
w$H89 <- height.CI[2,]
w

#output 
"""
weight Expected_height      L89      H89
1     46        155.6520 147.6134 163.6799
2     61        169.0014 160.2521 177.1480
3     35        145.7174 137.5714 153.5506
4     52        161.1209 152.9024 169.2298
5     56        164.6460 156.6293 172.8116
"""

#Question 2 ----------------------------
#Plot the prior predictive distribution for the polynomial regression model in Chapter 4.
#Use extract.prior to inspect the prior, and modify the code that simulates and plots
#prior predictive distributions for linear regression to perform prior predictive
#simulations. Plotting between 30 and 50 parabolas from the prior should suffice
#to show where the prior probability resides. Can you modify the prior distributions 
#of α,β1 and β2 so that the prior predictions stay within the biologically reasonable
#outcomes? You should not attempt to fit the data by hand. Instead, try to keep the
#curves consistent with what you know about height and weight before seeing the !Kung data.

data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

#standardising weights
d2$weight_s <- (d2$weight - mean(d2$weight))/sd(d2$weight)
d2$weight_s2 <- d2$weight_s^2

#polynomial regression model from book with the priors used
m4.5 <- quap(
  alist(
    height ~ dnorm( mu, sigma),
    mu <- a + b1*weight_s + b2*weight_s2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0,1),
    sigma ~dunif(0,50)
  ), data=d
)

#plotting points
# plot(d2$weight, d2$height, xlim=range(d2$weight), ylim=range(d2$height), 
#      col=rangi2, xlab = "weight", ylab = "height")
# mtext(concat("N = ", N))

priors = extract.prior(m4.5, 50) #samples from prior
priors
#tried out various priors based on this

#simulate & plot
for (i in 1:N)
  curve()
set.seed(2971)
N <- 50   #50 parabolas
a <- rnorm(N, 140, 30) #modified 
b1 <- rlnorm( N , 1 , 1 ) #modified 
b2 <- rnorm(N, 0, 0.5) #modified 

#adjusted prior on b1 since we know there is a positive relationship between height and weight. So, we make the lines look 'more positive'
# b2 prior affects the curvature of the line. We could just force it to be positive by using a narrow log-normal distribution.
# for a's prior, we increased the standard deviation so the lines cover more space. We also decrease its mean so it is not going outside the range of the worlds tallest person

#plotting the lines
plot(NULL, xlim=range(d2$weight), ylim=c(-10,280), xlab = "weight", ylab="height")
abline(h=0, lty=2) # height = 0 line
abline(h=272, lty=1, lwd=0.5) # height = 272 cm line
text(50, 282, "World's Tallest Person (272 cm)")
text(50, 10, "Embryo (0cm)")
xbar <- mean(d2$weight)
xsd <- sd(d2$weight)
for (i in 1:N) curve( 
  a[i] + b1[i] * (x - xbar)/xsd + b2[i]*((x - xbar)/xsd)^2, 
  from=min(d2$weight), 
  to=max(d2$weight) , 
  add=TRUE , 
  col=col.alpha("black",0.2)
)

#image attached


#Question 3 ----------------------------
"""Write down a multiple regression to evaluate the claim: 

The price of houses in Frankfurt is linearly related to size, but only after 
controlling for location (i.e., postal code).

You only need to write down the model definition. There are 41 postal codes in 
Frankfurt. For this exercise, consider houses to belong to one of four postal 
code regions: 603**, 604**, 605**, and 659**. """

#Solution: 
"""
p = a + b1*size +b2*p_code1 + b3*p_code2 + b4*p_code3 + b5*p_code4
where,
p = price of house in frankfurt (dependent variable)
a = y-intercept, price when all other variables are 0 
b1,b2,b3,b4 = regression coefficients. b1 measures the unit change in price when size changes.
              b2 measures the unit change in price when p_code1 changes (from 0 to 1).
p_code1,2,3,4 = 1 if house belongs to that postal code category, zero otherwise. For any house, only
one of the p_code variables will be turned 'on' i.e value of 1 while the rest will be 0.
p_code1 for 603**, p_code2 for 604**, p_code3 for 605**, p_code4 for 659**
size = size of house """

#Question 4 -----------------------------

#In the divorce example, suppose the DAG is: M → A → D. 
#What are the implied conditional independencies of this graph? Are the data consistent with it?

library(dagitty)
dag1 <- dagitty("dag{M -> A; A -> D}")
# M - marriage rate, D - divorce rate, A - median age at marriage
impliedConditionalIndependencies(dag1)
# D is conditionally independent of M given A
#D _||_ M | A
#which translates to:
  "Once we know median age at marriage for a State, there is little or no additional predictive 
  power in also knowing the rate of marriage in that State."

#equivatent DAG
equivalentDAGs(dag1)
#M -> A -> D equivalent to D -> A -> M which is also equivalent to M <- A -> D (in book)

data(WaffleDivorce)
d <-WaffleDivorce

# standardizevariables
d$D <-standardize(d$Divorce)
d$M <-standardize(d$Marriage)
d$A <-standardize(d$MedianAgeMarriage)

dag1 <-dagitty("dag{M->A->D}")

m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )
precis( m5.3 )

#Output
"""
mean   sd  5.5% 94.5%
  a      0.00 0.10 -0.16  0.16
bM    -0.07 0.15 -0.31  0.18
bA    -0.61 0.15 -0.85 -0.37
sigma  0.79 0.08  0.66  0.91
"""

#So this is consistent with what the data shows (pg 133). In the plot, bA (posterior
#mean for age at marriage) doesn’t move while bM (posterior mean for marriage rate)
#is only associated with divorce when age at marriage is missing from the model. We can 
# also see that bM is close to 0. 

#this implies there is no important direct causal path from marriage rate to divorce rate. 
#The association between marriage rate and divorce rate is spurious, caused by the influence 
#of age of marriage on both marriage rate and divorce rate. (pg134)


