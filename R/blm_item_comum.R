#adiconar blm_item_comum como uma parte da função de leitura do blm 
blm_item_comum <- function(blm_data, grp_ref = NULL){
  
  if (is.null(grp_ref)) {
    grp_ref <- length(blm_data$item_comum)
    cat(paste0('Grupo de Referência não informado (grp_reg = NULL) fazendo com o grupo: ', grp_ref, '.'))
  } 
  
  result <- list()
  grp_names <- names(blm_data$grupos)
  
  for (grp_compara in grp_names) {
    gab1 <- blm_data$grupos[[grp_compara]] %>% .[.$aban != 1, ]
    gab2 <- blm_data$grupos[[grp_ref    ]] %>% .[.$aban != 1, ]
    
    inter <- intersect(rownames(gab1), rownames(gab2)) %>% .[. != '0']
    
    if (length(inter) == 0) {
      temp <- NA
    }
    
    if (length(inter) > 0) {
      
      temp <- data.frame(nomeblg = inter, stringsAsFactors = FALSE)
      rownames(temp) <- temp$nomeblg
      cols <- c('nomeblg', 'itemblg', 'seq', 'gab')
      grp_names1 <- grp_compara
      grp_names2 <- grp_names[grp_ref]
      
      temp <- merge(temp, gab1, by = 'nomeblg', all.x = T)[, cols]
      colnames(temp)[3] <- grp_names1
      rownames(temp) <- temp$nomeblg
      temp[inter, grp_names2] <- gab2[inter, 'seq']
      
      temp <- temp[order(temp[,grp_names2]), c('itemblg', 'nomeblg', 'gab', grp_names1, grp_names2)]
      
    }
    result[[grp_compara]] <- temp
  }
  
  names(result) <- grp_names
  result <- result[-grp_ref]
  result
}




