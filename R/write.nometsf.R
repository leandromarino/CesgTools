write.nometsf <- function(nomeblg, ncol = 8){
  
  (aux <- length(nomeblg))
  
  # numero de linhas que vao existir na matriz de nomeblg
  (nlin <- ifelse(aux %% ncol == 0, aux/ncol, ceiling(aux/ncol)))
  
  # numero de linhas em branco adicionadas para poder ter uma matriz retangular
  (nadicional <- ifelse(aux %% ncol == 0, 0, ncol - aux %% ncol))
  
  nomeblg      <- sprintf("%9s", c(paste0(nomeblg, ','), rep("", nadicional)))
  nomeblg[aux] <- gsub(',', '', nomeblg[aux])
  nomeblg      <- matrix(nomeblg, ncol = ncol, nrow = nlin, byrow = TRUE) %>% 
    apply(., 1, paste0, collapse = '') %>% 
    removeBrancos() %>% 
    matrix(., ncol = 1) %>%
    apply(., 2, paste0, collapse = '\n')
  
  nomeblg
}

