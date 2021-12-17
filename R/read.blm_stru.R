read.blm_stru <- function(file, debug = FALSE){
  if(debug) browser()
  blm <- readLines(con = file)
  
  # verificando se tem ou não grupo
  if(sum(grepl(pattern = 'NGR', blm)) > 0){ grp = 1 } else{ grp = 0 }
  blm_names <-  if(grp == 1){c('id', 'caderno', 'grupo', 'respostas')} else {c('id', 'caderno', 'respostas')}
  
  blm <- blm[substr(blm, 1, 1) == '(']
  blm <- strsplit(blm, '[(]|[,]|[)]') %>% unlist() %>% .[.!='']
  
  
  
  
  # colocando nome nos X (não utilizados pelo bilog)
  aux <- grep('X', blm)
  names(blm)[aux] <- paste0('lixo', 1:length(aux))
  rm(aux)
  
  # colocando nome nos pesos
  aux <- grep('F', blm)
  names(blm)[aux] <- paste0('peso', 1:length(aux))
  rm(aux)

  names(blm)[is.na(names(blm))] <- blm_names
  
  blm <- gsub(pattern = "A1|X|I|F|[.]\\d{1,}", replacement = '', x = blm) 
  blm <- as.data.frame(blm, stringsAsFactors = FALSE)
  blm <- apply(blm, 1, as.integer) %>% as.data.frame() %>% set_colnames('tamanho')
  blm <- cbind(nomecol = rownames(blm), blm, vartipo = 'NA', stringsAsFactors = FALSE)
  
  aux_integer <- stringr::str_detect(string = blm$nomecol, pattern = "cad|gru")
  blm[aux_integer, 'vartipo'] <- 'integer'
  rm(aux_integer)
  
  aux_numeric <- stringr::str_detect(string = blm$nomecol, pattern = "peso")
  blm[aux_numeric, 'vartipo'] <- 'numeric'
  rm(aux_numeric)
  
  aux_numeric <- stringr::str_detect(string = blm$nomecol, pattern = "id|lix|resp")
  blm[aux_numeric, 'vartipo'] <- 'character'
  rm(aux_numeric)
  
  class(blm) <- c('blm_stru', class(blm))
  
  blm
}
