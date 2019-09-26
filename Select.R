Select <- function(Pop,Refitness){
  newPop <- Pop
  res <- c(Refitness)
  res <- res[!is.na(res)]
  for (i in 1:prod(dim(res))){
    #res[i] <- i
    r=runif(1)
    index=1
    for (j in 1:prod(dim(res))){
      if (r<=res[j]){
        index=j
        break
      }
    }
    newPop[i,] = Pop[index,]
  }
  return(newPop)
}

