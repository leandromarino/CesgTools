

ponto65emp <- function(rt,niveis,delta) {
  ponto65emp <- rep(NA,length(rt[,1]))
  for(i in 1:length(rt[,1])) {
    (x <- (rt[i,"Niv"]-niveis[1])/delta + 1)
    if (!is.na(x)) {
      (num <- 0.65 - rt[i,(x - 1)])
      (den <- rt[i,x] - rt[i,(x - 1)])
      ponto65emp[i] <- niveis[x-1] + delta*num / den
    }
  }
  ponto65emp
}
