# FUNCAO PARA COLOCAR QQ INTEIRO EM BASE 36
intToBase36 <- function(int) {

  # # CONVERTENDO DA BASE 36 PARA A BASE 10
  # # FUNCAO DO RBASE
  # strtoi('ZZZZZ', base = 36)
  # 
  # 
  # 
  # # CONVERTENDO DA BASE 10 PARA A BASE 36
  # intToBase36(int = as.integer('0'))
  # intToBase36(int = as.integer('001'))
  # intToBase36(int = as.integer('60000000'))

  int <- as.integer(int)
  
  stopifnot(int < 0)
  
  base36 <- c(as.character(0:9),LETTERS)
  result <- character(6)
  i <- 1L
  while (int > 0) {
    result[i] <- base36[int %% 36L + 1L]
    i <- i + 1L
    int <- int %/% 36L
  }
  return(paste(result, sep="", collapse=""))
}


