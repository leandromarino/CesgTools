


read.xqBlg <- function(file)
{
  #file = paste(dirblg,"M02\\BRA15M02V2.EXP",sep="")
  xq <- read.fwf(file,  skip=2,
                 widths=list(c(-27,10,rep(11,4)),c(-81),c(-81),c(-81),c(-81),c(-81),c(-81),c(-82),
                             c(-27,10,rep(11,4)),c(-81),c(-81),c(-81),c(-81),c(-81),c(-81),c(-82),
                             c(-27,10,rep(11,4)),c(-81),c(-81),c(-81),c(-81),c(-81),c(-81),c(-82),
                             c(-27,10,rep(11,4)),c(-81),c(-81),c(-81),c(-81),c(-81),c(-81),c(-82),
                             c(-27,10,rep(11,4)),c(-81),c(-81),c(-81),c(-81),c(-81),c(-81),c(-82),
                             c(-27,10,rep(11,4)),c(-81),c(-81),c(-81),c(-81),c(-81),c(-81),c(-82),
                             c(-27,10,rep(11,4)),c(-81),c(-81),c(-81),c(-81),c(-81),c(-81),c(-82),
                             c(-27,10,rep(11,4)),c(-81),c(-81),c(-81),c(-81),c(-81),c(-81),c(-82)),
                 colClasses=c(rep("numeric",5*8)),header=F,flush=T,nrows=1)
  rownames(xq) <- 0
  xq
}
