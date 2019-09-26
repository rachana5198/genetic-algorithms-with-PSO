Xcross <- function(Pop, CrossRate){
  
  px <- nrow(Pop)
  py <- ncol(Pop)
  
  tempRow <- matrix(0,1,py)  
  #Pop <- Pop
  Pop <- matrix(as.numeric(unlist(Pop)),nrow=nrow(Pop))
  for (i in 0:px/2){
    
    Xrate = runif(1)
    
    if (Xrate < CrossRate) {
      first_index=round(runif(1)*(px-2)+1)
      second_index=round(runif(1)*(px-2)+1)
      
      # Make sure first_index and second_index to be different. 
      while (first_index == second_index) {
        second_index = round(runif(1)*(px-2)+1)
      }
      
      if (py > 1) {
        if (py == 2) {
          point = 1
        }
        else {
          point = round(runif(1)*(py-2)+1)
        }
        
        # Swapping the rows [first_index, 1:point] with [second_index, 1:point] in 'Pop'
        tempRow[1,1:point]=Pop[first_index,1:point] 
        
        Pop[first_index,1:point]=Pop[second_index,1:point]
        
        Pop[second_index,1:point]=tempRow[1,1:point]
      }   
    }
  }
  
  return(Pop)
}