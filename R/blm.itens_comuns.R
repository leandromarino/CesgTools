
blm.itens_comuns <- function(file, compara_grupo){
  # file = paste0(dirBLG_CH_EM, 'Encc18_CH_EM_V1.BLM')
  # compara_grupo = 8
  # rm(file, blm_group, grp, group_ref, compara_grupo, i, x, itens_comuns, grupo_de_comparacao)
  
  blm_group <- read.blm.grou(file, group.names = T)
  
  max_nchar_blm_group <- max(nchar(names(blm_group)))
  
  grupo_de_comparacao <- blm_group[[compara_grupo]]
  
  group_ref <- c(1:length(blm_group))[-compara_grupo]
  
  grp=1
  
  itens_comuns <- list()
  i=1
  for(grp in group_ref){
    x <- intersect(x = blm_group[[grp]], y = grupo_de_comparacao)
    if(length(x) > 0){
      itens_comuns[[i]] <- x
      names(itens_comuns)[i] <- paste(stringr::str_pad(grp, width = 2), 
                                      "vs", 
                                      stringr::str_pad(compara_grupo, width = 2), 
                                      "||",
                                      stringr::str_pad(names(blm_group)[grp], width = max_nchar_blm_group), 
                                      'x', 
                                      names(blm_group)[compara_grupo])
      i = i+1
    }
  }
  class(itens_comuns) <- c('blm_itcom', class(itens_comuns))
  itens_comuns
}



print.blm_itcom <-function(x, i = NULL){
  # i pode ser um vetor
  saida <- vector(mode = 'character', length = length(x))
  
  for(seq in 1:length(x)){
    saida[seq] <- paste0('   * ', names(x)[seq],
                         " ||---- itemblg: ",
                         paste0(x[[seq]], collapse = ', '), 
                         ".")
  }
  if(is.null(i)){
    write.table(x = saida, quote = FALSE, sep = '', eol = '\n', col.names = FALSE, row.names = FALSE)
  }else{
    write.table(x = saida[i], quote = FALSE, sep = '', eol = '\n', col.names = FALSE, row.names = FALSE)
  }
}
