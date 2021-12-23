read.blm.stru <- function(file, debug = FALSE){
  if(debug) browser()
  blm <- readLines(con = file)
  
  if(sum(grepl(pattern = 'NGR', blm)) > 0){ grp = 1 }else{ grp = 0 }
  
  blm <- blm[substr(blm, 1, 1) == '(']
  blm <- blm[length(blm)]
  blm <- strsplit(blm, '[(]|[,]|[)]') %>% unlist() %>% .[.!='']
  
  if(length(grep('X', blm)) > 0){
    names(blm)[grep('X', blm)] <- paste0('lixo', 1:length(grep('X', blm)))
    names(blm)[-grep('lixo', names(blm))] <-  if(grp == 1){c('id', 'caderno', 'grupo', 'respostas')}else{c('id', 'caderno', 'respostas')}
  }else{
    names(blm) <-  if(grp == 1){c('id', 'caderno', 'grupo', 'respostas')}else{c('id', 'caderno', 'respostas')}
  }
  
  blm <- gsub(pattern = "A1|X|I", replacement = ' ', x = blm)
  blm <- as.data.frame(blm, stringsAsFactors = FALSE)
  blm <- apply(blm, 1, as.integer) %>% as.data.frame
  colnames(blm) <- 'stru'
  blm
}


read.blm_stru <- function(file, debug = FALSE){
  if(debug) browser()
  blm <- readLines(con = file)
  
  if(sum(grepl(pattern = 'NGR', blm)) > 0){ grp = 1 }else{ grp = 0 }
  
  blm <- blm[substr(blm, 1, 1) == '(']
  blm <- strsplit(blm, '[(]|[,]|[)]') %>% unlist() %>% .[.!='']
  
  if(length(grep('X', blm)) > 0){
    names(blm)[grep('X', blm)] <- paste0('lixo', 1:length(grep('X', blm)))
    names(blm)[-grep('lixo', names(blm))] <-  if(grp == 1){c('id', 'caderno', 'grupo', 'respostas')}else{c('id', 'caderno', 'respostas')}
  }else{
    names(blm) <-  if(grp == 1){c('id', 'caderno', 'grupo', 'respostas')}else{c('id', 'caderno', 'respostas')}
  }
  
  blm <- gsub(pattern = "A1|X|I", replacement = ' ', x = blm)
  blm <- as.data.frame(blm, stringsAsFactors = FALSE)
  blm <- apply(blm, 1, as.integer) %>% as.data.frame
  colnames(blm) <- 'stru'
  
  class(blm) <- c('blm_stru', class(blm))
  blm
}
