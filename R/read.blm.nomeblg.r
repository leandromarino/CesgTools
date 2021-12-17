read.blm.nomeblg <- function(file, debug = FALSE){
  
  if(debug) browser()
  
  blm <- readLines(con = file)
  check_item <- substr(blm, 1, 5) == ">ITEM"
  check_test <- substr(blm, 1, 5) == ">TEST"
  inicio <- which(check_item == TRUE)
  fim <- c(which(check_item == TRUE)[-1],which(check_test == TRUE)[1])-1
  
  nomeblg <- blm[inicio:fim]
  nomeblg <- trim(nomeblg) %>% .[. != '']
  nomeblg <- paste0(nomeblg, ',') %>% gsub("\n", ',', .) %>% gsub(",,", ',', .)
  nomeblg <- paste0(nomeblg, collapse = '')
  nomeblg <- unlist(nomeblg)
  nomeblg <- substring(nomeblg, stringr::str_locate_all(string = nomeblg, pattern = 'INA[:alpha:]{0,}\\s{0,}=\\s{0,}\\(')[[1]][1,2] + 1)
  nomeblg <- strsplit(x = nomeblg, split = ')')[[1]][1]
  
  nomeblg <- strsplit(nomeblg[[1]], ',')
  nomeblg <- as.list(trim(unlist(nomeblg)))
  
  indices_fortran <- grep(pattern = '.*[(].*',nomeblg) #Elementos do vetor
  
  # nomes que foram feitos com fortran
  
  for( i in indices_fortran){
    aux <- nomeblg[[i]]
    aux <- lapply(strsplit(aux,'[(]|[)]'), '[', i = c(1,3))
    aux <- as.list(unlist(aux))
    aux[[1]] <- stringr::str_match(string = aux[[1]],pattern = '(.*?)(\\d+)(?<=$)')
    aux[[2]] <- stringr::str_match(string = aux[[2]],pattern = '(.*?)(\\d+)(?<=$)')
    aux <- data.frame(do.call(rbind, aux), stringsAsFactors=FALSE)
    num_char <- max(nchar(aux$X3))
    aux$X4 <- as.integer(aux$X3)
    nomeblg[[i]] <- paste0(aux$X2[1],sprintf(paste0("%0",num_char,'d'), aux$X4[1]:aux$X4[2]))
  }
  
  nomeblg <- unlist(nomeblg) %>% .[. != '']
  nomeblg
}

