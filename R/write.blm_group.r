# gabpar = gabpar_PT17_CN
# grpini = 6
# grpnome = 'PT17'
# debug = FALSE

write.blm_group <- function(gabpar, grpini, grpnome, debug = FALSE){
  if(debug) browser()
  
  # --- verificando colunas de gabpar
  verifica <- c('it','bl','ob','itemblg','aban') %>% .[. %!in% colnames(gabpar)]
  if(length(verifica) > 0) stop("As colunas: '", 
                                paste0(verifica, collapse = "', '"), 
                                "' não estão definidas em gabpar.")
  rm(verifica)
  
  
  # --- variaveis auxiliares
  tbl   <- gabpar$bl   %>% table() %>% unique() #tamanho do bloco
  nbl   <- gabpar$bl   %>% max()                #numero total de blocos
  naban <- gabpar$aban %>% sum()                #numero de itens abandonados
  nit    <- tbl * nbl                           #numero de itens total no grupo
  
  
  L1 <- paste0(">GROUP", grpini + 1, 
               " GNAME='", grpnome,
               "', LENGTH= ", nit - naban,
               ",  INUMBERS=("
  )
  L1
  L2 <- matrix('', ncol = tbl, nrow = nbl)  
  
  
  # --- padronizando itemblg
  ndig = nchar(gabpar$itemblg) %>% max()
  gabpar$itemblg <- gabpar$itemblg %>% 
    stringr::str_pad(string = ., 
                     width = ifelse(ndig > 3, 4, 3), pad = ' ')
  rm(ndig)
  
  ## criando matriz de itemblg
  for(j in 1:nbl)
  {
    L2[j,] <- paste0(gabpar[gabpar$bl == j, 'itemblg'],", ")
  }
  L2[j,tbl] <- gsub(',',');',L2[j,tbl])  
  
  
  
  # --- colocando espacos em branco quando item ? abandonado
  if(naban > 0)
  {
    aux <- gabpar[gabpar$aban == 1,]
    for(i in 1:naban)
    {
      L2[aux$bl[i],aux$ob[i]] <- paste0(rep(' ',6), collapse = '')
    }
  }
  
  
  ### acertando matriz para quando o ultimo item eh abandonado
  if(gabpar[nit,'aban']==1)
  {
    L2[j,tbl] <- '    );'
    
    # verificando ultimo item nao abandonado para remover a virgula ','
    cont <-  nit
    stop <- 0
    while(stop == 0)
    {
      if(gabpar[cont,'aban'] == 1)
      {
        cont <- cont - 1
      }else{
        stop <-  1
        cont <-  cont
      }
    }
    L2[gabpar$bl[cont],gabpar$ob[cont]] <- gsub(",",' ',L2[gabpar$bl[cont],gabpar$ob[cont]])
  }
  
  
  
  if(tbl * 5 > 79 & tbl <= 30){
    if(tbl%%2 == 0){
      L2 <- matrix(as.vector(t(L2)), nrow = nbl*2, ncol = tbl/2, byrow = T)
    }else{
      NROW <- c((nbl+1):100)
      #p1 = verifica se o tamanho da prova ? divisivel por nrow
      p1 = (nbl*tbl) %% NROW == 0
      # p2 = verifica se o nrow testado pode gera um objeto de comprimento menor que 78 posicoes (BLM)
      p2 = ((nbl*tbl) / NROW) * 5 < 78
      aux  <- ( p1 + p2 )
      NROW <- NROW[aux==2][1]
      L2 <- matrix(as.vector(t(L2)), nrow = NROW, byrow = T)
    }
  }
  
  if(tbl > 30 & nbl == 1){
    L2 <- as.vector(L2)
    if(tbl %% 15 > 0){
      L2 <- matrix(data = c(L2, rep('', (15 - tbl %% 15))),
                   nrow = ceiling(tbl / 15),
                   ncol = 15, 
                   byrow = T)
    }else{
      L2 <- matrix(data = L2,
                   nrow = tbl / 15,
                   ncol = 15, 
                   byrow = T)
    }
  }
  
  write.table(L1,quote = F,row.names = F,col.names = F, sep='')
  write.table(L2,quote = F,row.names = F,col.names = F, sep='')
  
}




