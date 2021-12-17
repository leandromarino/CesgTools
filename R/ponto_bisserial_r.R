
ponto_bisserial_r <- function(respostas, scores, itempos, resposta_possivel, CodNaoAp, Peso, mostra_napres){
  
  scores$seq <- rownames(scores)
  
  busca_resposta_item <- function(x){
    cad = x[[1]]
    pos = x[[2]]
    aux <- respostas[respostas$caderno == cad, 'respostas', drop = FALSE]
    aux$seq <- rownames(aux)
    aux$respostas <- substr(aux$respostas, pos, pos)
    aux
  }
  
  aux_resp <- apply(X = itemposNum[[i]], MARGIN = 1, FUN = busca_resposta_item) %>% 
    do.call(rbind, .) %>%
    magrittr::set_rownames(., value = .$seq)
  aux_resp <- cbind(scores, resp = aux_resp[scores$seq, 'respostas'])[, c('seq', 'resp', 'pacer')]
  aux_resp <- aux_resp[aux_resp$resp %in% resp_possible,]
  aux_resp <- cbind(aux_resp, model.matrix(~ aux_resp$resp + 0))
  colnames(aux_resp) <- gsub(pattern = 'aux_resp\\$resp', replacement = 'alt', x = colnames(aux_resp))
  aux_resp[1:10,]
  
  
  aa <- apply(X = aux_resp[, substr(colnames(aux_resp), 1, 3) == 'alt'], 2, function(x) prop.table(table(x))[2]*100)
  bb <- apply(X = aux_resp[, substr(colnames(aux_resp), 1, 3) == 'alt'], 2, function(x) cor(x = aux_resp$pacer, y = unlist(x)))
  names(aa) <- gsub(pattern = 'alt', replacement = 'Perc', names(aa))
  names(bb) <- gsub(pattern = 'alt', replacement = 'PBise', names(bb))
  
  c(aa,bb)
}
