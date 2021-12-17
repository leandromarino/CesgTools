

read.blm.data <- function(file, stru){
  # stru = estrutura (da funcao read.blm.stru)
  #colocar um controle de classes
  data <- read.fwf(file = file, widths = stru$stru, header = FALSE, colClasses = 'character', col.names = rownames(stru))
  data
}


