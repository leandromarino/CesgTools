
escore_r <- function(respostas, gabarito,  NumCad, NumItens, CodAcer, CodErro, CodNaoAp, nblform, tbl){
  
  result <- data.frame(matrix(NA, ncol = 4 + nblform, nrow = nrow(respostas))  )
  colnames(result) <- c('padrao','nacer','ntried','pacer',paste0('nbl',sprintf('%02d',1:nblform)))
  
  aux_respostas <- do.call(rbind,strsplit(respostas$respostas,''))
  aux_gabaritos <- do.call(rbind,strsplit(gabarito$gabarito,''))
  aux_correcao <- matrix(NA, nrow = nrow(aux_respostas), ncol = ncol(aux_respostas))
  
  for(i in 1:nrow(gabarito)){
    aux <- respostas$caderno == gabarito$caderno[i]
    aux_correcao[aux,] <- t(apply(aux_respostas[aux,], 1, function(x) x == aux_gabaritos[1,]))
  }
  rm(i, aux)
  
  result$nacer <- rowSums(aux_correcao)
  result$ntried <- nchar(respostas$respostas) - rowSums(aux_respostas == CodNaoAp)
  result$pacer <- (result$nacer / result$ntried * 100)
  
  for(i in 1:nblform){
    itens_bl <- seq(from = ((i - 1)*tbl + 1), to = i*tbl, by = 1) 
    result[,paste0('nbl',sprintf('%02d',i))] <- rowSums(aux_correcao)
  }
  rm(i, itens_bl)
  
  aux_correcao[aux_correcao  == TRUE    ] <- CodAcer
  aux_correcao[aux_respostas == CodNaoAp] <- CodNaoAp
  aux_correcao[aux_correcao  == FALSE   ] <- CodErro
  
  result$padrao <- apply(aux_correcao, 1, paste0, collapse = '')
  
  result 
}


