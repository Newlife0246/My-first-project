#random correlation test
x<-c(50,90,70,80,60,90,80,70,70,60,90,50)
y<-c(65,80,75,75,45,95,80,65,70,65,85,55)
cor(x,y) #this yield a correlation of 0.8 indication strong relation between x and y

#Regression
a=c(1,2,2,3,3,4,4,5,5,5,6,6,6,7,8)
b=c(50,50,70,50,70,70,80,60,90,80,70,90,100,80,90)
lm(a~b)

#residual test
n=c(18,20,20,22,24,25,26,28,29,30,31,32,34,36,37)
m=c(55,49,51,47,48,44,39,36,37,39,38,32,33,30,29)
bv=lm(n~m)
resid(bv)

56.-0.7*30  #


#load data trees from R built-in dataset
data(trees)
head(trees)
nrow(trees)
pairs(trees)  ##scatter plot tree of height,girth and volume


##plot the residuals
lm1 <- lm(Volume ~ Height + Girth, data=trees)
summary(lm1)

X = cbind(1, trees$Height, trees$Girth)

beta = qr.solve(X, trees$Volume)

set.seed(153524)
x <- rnorm(1000)
Lx <- dnorm(x)
min(Lx)    # likelihood for each observation is positive
prod(Lx)   # total likelihood appears to be zero

sum(log(Lx))   # log-likelihood is managable
sum(dnorm(x,log=TRUE))

log(exp(1000) - exp(999) + exp(998) - exp(997))

log(exp(3) - exp(2) + exp(1) - exp(0)) + 997

log(factorial(500))
lfactorial(500)

dnorm(10, 0, 1)
dnorm(10, 0, 1, log=TRUE)
