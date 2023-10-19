#### binomial distribution
# probability that a 10-yr flood happens once in 3 yrs
dbinom(x = 1, size = 3, prob = 0.1)
# probability that a 10-yr flood happens 2 times in 3 yrs
dbinom(x = 2, size = 3, prob = 0.1)
# probability that a 10-yr flood happens 3 times in 3 yrs
dbinom(x = 3, size = 3, prob = 0.1)
# probability that a 10-yr flood does not happen in 3 yrs
dbinom(x = 0, size = 3, prob = 0.1)
# probability that a 10-yr flood happens once in 3 yrs.
dbinom(x = 1, size = 3, prob = 0.1)
# probability that a 10-yr flood happens once in 10 yrs.
dbinom(x = 1, size = 10, prob = 0.1)
# binomial dist. cases
dbinom(x = 3, size = 10, prob = 0.1)

# plot binomial distribution for all cases
x <- seq(0,10,1)
plot(x = x, y = dbinom(x,10,0.1))

#### normal distribution
# normal distribution with mean of 0 and sd of 0.5
x <- seq(-3,3,0.1)
plot(x = x, y = dnorm(x, mean = 0, sd = 0.5), 
     type = 'l',
     ylab = 'density')
# normal dist. with sd of 2
lines(x = x, y = dnorm(x, mean = 0, sd = 2), col = 'blue')
# normal dist. with sd of 1
lines(x = x, y = dnorm(x, mean = 0, sd = 1), col = 'red')
# normal dist. with different mean values
m_value <- 1
plot(x = x, y = dnorm(x, mean = m_value, sd = 0.5), 
     type = 'l',
     ylab = 'density')

lines(x = x, y = dnorm(x, mean = m_value, sd = 2), col = 'blue')
lines(x = x, y = dnorm(x, mean = m_value, sd = 1), col = 'red')

# dnorm() probability density function
x <- seq(-3,3,0.1)
plot(x = x, y = dnorm(x, mean = 0, sd = 1), type = 'l')
lines(x=c(1,1),y=c(-1,1),col='blue')
# pnorm() cumulative density function
plot(x = x, y = pnorm(x, mean = 0, sd = 1), type = 'l')
lines(x=c(1,1),y=c(-1,1),col='blue')
pnorm(1)
# qnorm() quantile function
qnorm(0.8413447)
# rnorm() random deviates
rnorm(10)
hist(rnorm(10000))
x1 = rnorm(10000,mean=0,sd = 1)
x2 = rnorm(10000,mean=0,sd = 2)
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.pink")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.blue")
ax = seq(-10,10,by=1)
hist(x1,breaks = ax,col=c1)
hist(x2,breaks = ax,col=c2,add=T)
library(moments)
kurtosis(x1)
kurtosis(x2)
#probability between [-1,1]
x <- seq(-3,3,0.1)
plot(x = x, y = dnorm(x, mean = 0, sd = 1), type = 'l')
lines(x=c(1,1),y=c(-1,1),col='blue')
lines(x=c(-1,-1),y=c(-1,1),col='blue')
pnorm(1) - pnorm(-1)
lines(x=c(2,2),y=c(-1,1),col='red')
lines(x=c(-2,-2),y=c(-1,1),col='red')
pnorm(2) - pnorm(-2)
qnorm(0.99)

# binomial and normal dist.
n = 50 
p = 0.5
x <- seq(0,n,1)
plot(x = x, y = dbinom(x,n,p))
var_binom = n*p*(1-p)
sd_binom = var_binom^0.5
lines(x=x, y=dnorm(x,mean=25,sd = sd_binom),
      col='green')
