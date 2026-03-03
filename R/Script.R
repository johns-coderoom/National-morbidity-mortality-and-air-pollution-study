install.packages("INLA")
R.version.string
Sys.which("make")
pkgbuild::has_build_tools(debug = TRUE)
install.packages(
  "INLA",
  repos = c(
    getOption("repos"),
    INLA = "https://inla.r-inla-download.org/R/stable"
  ),
  dependencies = TRUE
)
install.packages("maptools")
install.packages("sf")
install.packages("terra")
install.packages("lattice")
### R code for Chapter 4
### Last update: 14/08/2014

###################################################
### Set working directory and load packages
###################################################
remove(list=ls())
my.dir <- paste(getwd(),"/",sep="")

require(INLA)
require(maptools)
require(lattice)

### R code for Chapter 4
### Last update: 14/08/2014

###################################################
### Set working directory and load packages
###################################################
remove(list=ls())
my.dir <- paste(getwd(),"/",sep="")

require(INLA)
require(maptools)
require(lattice)

###################################################
### Code for Section 5.1.2
###################################################
# You neeed a folder called "NMMAPS" inside your working directory (my.dir)
# with the data downloaded from
# https://sites.google.com/a/r-inla.org/stbook/datasets

dataNMMAPS <- read.csv(paste("C:/Users/lab_u/OneDrive/Desktop/pollut/NMMAPS/NMMAPSraw.csv",sep=""))

formula <-  pm10 ~ 1 + temperature
model.linear <- inla(formula,family="gaussian",data=dataNMMAPS)
round(model.linear$summary.fixed[,1:5],3)

# *** Code for Figure 5.2
plot(model.linear$marginals.fixed[[1]],type="l",main="",ylab="",xlab=expression(beta[0]))
plot(model.linear$marginals.fixed[[2]],type="l",main="",ylab="",xlab=expression(beta[1]))
# ***

summary(lm(formula,data=dataNMMAPS))

# Change the prior for beta0 and beta1
model.linear <- inla(formula,family="gaussian", data=dataNMMAPS,
                     control.fixed=list(mean=0, prec=1, mean.intercept=0, prec.intercept=0.0001))

# Change the prior for the precision
model.linear <- inla(formula,family="gaussian", data=dataNMMAPS,
                     control.family=list(hyper=list(prec=list(prior="gaussian",param=c(0,1)))))

# How to set the prior on the standard deviation 
# Set parameters for sigma 
a1 <- 2
b1 <- 14
# Simulate sigma from a Uniform distribution
sigma <- runif(n=10000,min=a1,max=b1)
# Check the mean and variance of sigma 
mean(sigma); var(sigma)
# Obtain the precision 
tau <- 1/sigma^2
# Calculate the values of a and b for the distribution of the precision
a2 <- mean(tau)^2/var(tau)
b2 <- a2/mean(tau)

# *** Code for Figure 5.3
plot(density(tau),main="")
curve(dgamma(x,a2,rate=b2), from=0, to=max(tau), add=T, lty=2,lwd=2)
legend("topright",legend=c(expression(tau),expression(paste("Gamma(",a[2],",",b[2],")",sep=""))),
       lwd=c(1,2),lty=c(1,2),bty="n", pt.cex=2)
# ***
# Scatter plot with linear fit
plot(dataNMMAPS$temperature, dataNMMAPS$pm10,
     pch=16, col=rgb(0,0,0,0.3),
     xlab="Temperature",
     ylab="PM10",
     main="PM10 vs Temperature")

abline(coef(model.linear$summary.fixed[, "mean"]),
       col="red", lwd=2)
plot(model.linear$marginals.hyperpar[[1]],
     type="l",
     main="Posterior of Precision (Tau)",
     xlab=expression(tau),
     ylab="Density")
sigma.marginal <- inla.tmarginal(function(x) 1/sqrt(x),
                                 model.linear$marginals.hyperpar[[1]])

plot(sigma.marginal,
     type="l",
     main="Posterior of Sigma",
     xlab=expression(sigma),
     ylab="Density")
hist(dataNMMAPS$pm10,
     breaks=30,
     col="blue",
     main="Distribution of PM10",
     xlab="PM10")
hist(dataNMMAPS$temperature,
     breaks=30,
     col="red",
     main="Distribution of Temperature",
     xlab="Temperature")
fitted.values <- model.linear$summary.fitted.values$mean

plot(fitted.values, dataNMMAPS$pm10,
     pch=16,
     col=rgb(0,0,0,0.4),
     xlab="Fitted values",
     ylab="Observed PM10",
     main="Observed vs Fitted")

abline(0,1,col="red",lwd=2) 

