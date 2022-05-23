

read.blm.data <- function(file, stru){
  warning('Função ficando fora de uso. Substituir pela read.blm_txt')
  # stru = estrutura (da funcao read.blm.stru)
  #colocar um controle de classes
  data <- read.fwf(file = file, widths = stru$stru, header = FALSE, colClasses = 'character', col.names = rownames(stru))
  data
}
