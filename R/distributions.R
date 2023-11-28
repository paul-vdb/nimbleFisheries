####################
## Distributions for fisheries modelling
## From: phenomix in R
####################

## Need to make sure I can add derivatives to this via buildDerivs. 
##This may make me loop instead of vectorize the functions...

## Two-Piece Normal Density Function for asymmetric Gaussian run time model
ddnorm <- nimbleFunction(
  run = function(x = double(1), mean = double(), sigma = double(1), log = integer(0, default = 1)){
    logC <- log(2) - log(sum(sigma)) -  0.5*log(2*pi)
    z <- (x - mean)
    z[z <=0 ] <- z[z <=0 ]/sigma[1]
    z[z > 0 ] <- z[z > 0 ]/sigma[2]
    ans <- logC - 0.5*z^2
    returnType(double(1))
    if(log) return(ans)
    else return(exp(ans))
  }
)

## Two-Piece Normal Quantile Function for asymmetric Gaussian run time model
qdnorm <- nimbleFunction(
  run = function(p = double(1), mean = double(), sigma = double()){
    z <- p
    r <- sigma[1]/(sigma[1] + sigma[2])
    z[p <= r] <- mean + sigma[1] * qnorm(0.5 * p[p <= r] * sum(sigma)/sigma[1], mean = 0, sd = 1)
    z[p > r] <- mean + sigma[2] * qnorm(0.5 * (sum(sigma) * (1 + p[p > r]) - 2 * sigma[1])/sigma[2], mean = 0, sd = 1)
    returnType(double(1))    
    return(z);
  }
)

## Two-Piece Normal Cumulative Density function for asymmetric Gaussian run time model
pdnorm <- nimbleFunction(
  run = function(x = double(1), mean = double(), sigma = double(1)){
    cdf <- x
    z1 <- (x-mean)/sigma[1]
    const <- 2.0 / sum(sigma)
    cdf[z1 <= 0] <- phi(z1[z1 <= 0])*const*sigma[1]
    z2 <- (x-mean)/sigma[2]
    cdf[z2 > 0] <- 0.5*const*sigma[1] + (phi(z2[z2 > 0])-0.5)*const*sigma[2]
    returnType(double(1))    
    return(cdf);
  }
)

## Two-Piece Normal Random numbers for asymmetric Gaussian run time model
rdnorm <- nimbleFunction(
  run = function(n = integer(), mean = double(), sigma = double()){
    x <- numeric(value = 0, length = n)
    p <- sigma/sum(sigma)
    z <- rnorm(n, 0, 1)
    type <- rcat(n, prob = p)
    for( i in 1:n ){
      if(type[i] == 1) x[i] <- mean - abs(z[i])*sigma[1]
      else x[i] <- mean + abs(z[i])*sigma[2]
    }
    returnType(double(1))    
    return(x);
  }
)