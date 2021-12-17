
read.blm_txt<- function(file, stru){
  # stru = estrutura (da funcao read.blm.stru)
  #colocar um controle de classes
  
  if('blm_stru' %!in% class(stru)) stop("O objeto de 'stru' não pertence à classe 'blm_stru'.")
  
  data <- read.fwf(file = file, 
                   widths = stru$tamanho,
                   header = FALSE, 
                   colClasses = stru$vartipo, 
                   col.names = stru$nomecol)
  
  class(data) <- c('blm_txt', class(data))
  
  data
}


