# gabpar  = gabpar_PT17_CN
# bib     = bib_PT17
# formini = 148


write.blm_form <- function(gabpar, bib, formini)
  {
  
  # --- verificando colunas de gabpar
  verifica <- c('it', 'bl', 'ob', 'itemblg') %>% .[. %!in% colnames(gabpar)]
  if(length(verifica) > 0) stop("As colunas: '", 
                                paste0(verifica, collapse = "', '"), 
                                "' não estão definidas em gabpar.")
  rm(verifica)
  
  
  # --- verificando colunas de bib
  verifica <- c('caderno', paste0('bl', 1:(ncol(bib)-1))) %>% .[. %!in% colnames(bib)]
  if(length(verifica) > 0) stop("As colunas: '", 
                                paste0(verifica, collapse = "', '"), 
                                "' não estão definidas em bib.")
  rm(verifica)

  
  # --- criando variaveis auxiliares
  tbl <- unique(table(gabpar$bl))   # tamanho do bloco
  tfo <- tbl * (ncol(bib) - 1)      # tamando do caderno/form
  nblfo <-     (ncol(bib) - 1)      # numero de blocos no caderno/form
  
  
  # --- padronizando itemblg
  ndig = nchar(gabpar$itemblg) %>% max()
  gabpar$itemblg <- gabpar$itemblg %>% 
    stringr::str_pad(string = ., 
                     width = ifelse(ndig > 3, 4, 3), pad = ' ')
  
  
  apply(bib, 1, FUN = function(x){
    form = x[[1]] %>% unlist()
    L1 <- paste0(">FORM", sprintf("%03d", form + formini), "  LENGTH= ", tfo, ",  INUMBERS=(")
    L2 <- matrix('', ncol = tbl, nrow = nblfo)
    for(j in 1:nblfo)
    {
      L2[j,] <- paste0(gabpar[gabpar$bl == bib[form, j+1], 'itemblg'],", ")
    }
    L2[j,tbl] <- gsub(',',');',L2[j,tbl])
    
    if(tbl * 6 > 79){
      aux_lin <- ceiling((ncol(L2) * 6) / 79) 
      aux_col <- ceiling(ncol(L2) / aux_lin)
      aux_matrix <- matrix('', ncol = aux_lin, nrow = aux_col, byrow = TRUE)
      aux_matrix[1:ncol(L2)] <- as.vector(L2)
      L2 <- t(aux_matrix)
      rm(aux_matrix, aux_lin, aux_col)
    }
    write.table(L1, quote = F,row.names = F,col.names = F, sep='')
    write.table(L2, quote = F,row.names = F,col.names = F, sep='')
    return(NULL)
  })
 NULL 
}
  
  
