ponto65 <- function(param){
  ponto65 <- qlogis( (.65 - param[,3])/(1-param[,3]),param[,2],1/param[,1])
  ponto65
}
