####################
## Simulation from posterior to fit Dharma related residuals.
####################

## Need to make sure I can add derivatives to this via buildDerivs. 
##This may make me loop instead of vectorize the functions...

## Currently Vector Only:
## Can break if the correct nodes are not tracked.
simulateObservations <- nimbleFunction(
  setup = function(model, samples, simNodes){
    nodeNames <- colnames(samples)
    nSamples <- nrow(samples)
    simNodesExpanded <- model$expandNodeNames(simNodes, returnScalarComponents = TRUE)
    nNodes <- length(simNodesExpanded)
  },
  run = function(){},
  methods = list(
    simulateObservations = function(n = integer(0, default = 0)){
      if(n == 0) n <- nSamples
      if(n > nSamples) {
        print("Warning: Can only return a maximum of MCMC iterations")
        n <- nSamples
      }
      sims <- matrix(value = 0, nrow = nNodes, ncol = n)
      for( i in 1:n){
        values(model, nodeNames) <<- samples[i,]
        model$simulate(simNodesExpanded, includeData = TRUE)
        sims[,i] <- model[[simNodes]] 
      }

      returnType(double(2))
      return(sims)
    }
  )
)