Mutation <- function(Pop, MutationRate, maxP, minP)
{
  px <- nrow(Pop)
  py <- ncol(Pop)
  newPop=Pop
  r <-c(newPop)
  
  for (j in 1:px*py){
    Mrate=runif(1)
    if (Mrate<MutationRate){  
      r[j]= runif(1)*(maxP-minP) +  minP
    }
  }
  
  return(newPop)
}
