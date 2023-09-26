
## Fun with Nimble
library(nimble)

## Copy of Ricker_simple.cpp 
## https://github.com/Pacific-salmon-assess/samEst/blob/main/src/Ricker_simple.cpp
## Some weird things going on in that basic model.

## Make lambertW function. Not super familiar with this code but
## getting same answer as wolfram alpha.
## "An explicit solution for calculating optimum spawning stock size from Ricker’s 
## stock recruitment model"
LambertW <- nimbleFunction(
  setup = function(){}, ## To run in a nimble model can't have setup code.
  ## Seems like alpha is transformed so can just do that here.
  run = function(x = double(), log = integer(0, default = 0)){ 
    if(log == 1) {
      logx <- x
    }else{
        logx <- log(x)
    }
    y <- logx
    if(logx < 0) y <- 0
    done <- 0L
    for (i in 1:100) {
      if(done == 0){
        if ( abs( logx - log(y) - y) < 1e-9) {done <- 1}
        y <- y - (y - exp(logx - y)) / (1 + y);                  
      }
    }
    if (done == 0) print("W: failed convergence");
    returnType(double())
    return(y)
  },
  ## Turn this on to build locally and check AD.
  # methods = list(
  #   gr_lambertW = function(x = double(), log = integer(0, default=1)){
  #       ans <- derivs(run(x, log), wrt=1, order=0:2)
  #       returnType(ADNimbleList())
  #       return(ans)
  #     }
  # ),
  buildDerivs = list(run = list(ignore = c('i', 'done')))
)

## Check the function to make sure it is working.
# lambert <- LambertW()
# lambert$run(1)
# lambert$gr_lambertW(1)
# lambertC <- compileNimble(lambert)
# lambertC$run(1)
# lambertC$gr_lambertW(1)


## I honestly have never run one of these before so don't really know much about it.
## Best interpretation from TMB code.

## Basic model is: log(R/S)=log(a)*S*exp(-b*S)
# a > 0, b > 0

nimbleCode(
  ##priors
  alpha ~ dnorm(mean=1.5, sd=2.5)
  logbeta ~ dnorm(logb_p_mean,logb_p_sd)
  beta <- exp(beta)
  # the spawning stock size leading to maximum recruit production (SMSR) 
  Smax <- 1/beta
  logSigObs ~ dnorm(0, sd=5)  ## Prior from RickerModels code.
  SigObs <- exp(logSigObs)

  ## Model and residuals:
  for(i in 1:timeSteps){
    logRS[i] <- alpha - beta * obs_S[i] ;
    pred_logR(i) <- logRS[i] + log(obs_S[i]);
    residuals[i] <- obs_logR[i] - pred_logR[i];
    obs_logR[i] ~ dnorm(pred_logR[i], sd = SigObs);
  }
  
  ## UMSY=bSMSY
  umsy <- (1 - LambertW(1-alpha,1))
  ## Hilborn version
  # umsy <- 0.5 * alpha - 0.07 * (alpha)^2
  
  ## The maximum sustainable yield
  Smsy <- umsy*Smax
)