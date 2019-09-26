
Live_fn <- function(x){
  
  p=0
  q=0
  for (k in 1:5){
    p=p+k*cos((k+1)*x[1]+k)
    q=q+k*cos((k+1)*x[2]+k)
  }
  
  fposition=p*q+(x[1]+1.42513)^2+(x[2]+.80032)^2
  return(fposition)
}