write.prmBlg <- function(dados,file, add = FALSE, menor_item = NULL)
{
  colnames(dados) <- c('it','a','b','c')
  dados[,c('a','b','c')] <- format(round(dados[,c('a','b','c')],5),ndigits =5, nsmall = 5)
  
  dados <- dados[order(dados$it), ]
  
  if(!is.null(menor_item)){
    dados <- dados[dados$it > menor_item,]
  }
  
  if(add == FALSE){
    # dados <- itfixBra1502M[,c('itemblg','a','b','c')]
    # file <- paste0(dirBlgBraPT1602M,'FixparmatBRA14M02V2.PRM')
    write.table(nrow(dados),file,col.names=F,row.names=F,quote=F)
    write.table(dados,file,col.names=F,row.names=F,quote=F,append = T, sep='\t')
  }
  if(add == TRUE){
    aux = readLines(con = file)
    aux[1] <- length(aux)-1 + nrow(dados)
    write.table(aux,file,col.names=F,row.names=F,quote=F,append = F, sep='\t')
    write.table(dados,file,col.names=F,row.names=F,quote=F,append = T, sep='\t')
  }
}

