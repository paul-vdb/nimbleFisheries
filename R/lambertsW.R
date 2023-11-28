
## Fun with Nimble
library(nimble)

## Copy of Ricker_simple.cpp 
## https://github.com/Pacific-salmon-assess/samEst/blob/main/src/Ricker_simple.cpp
## Some weird things going on in that basic model.

## Make lambertW function. Not super familiar with this code but
## getting same answer as wolfram alpha.
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