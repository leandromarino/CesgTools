# tbl = 16
# nbl = 4
# bib = bib_PT19D



write.blm_item <- function(bib, tbl, nbl){
  
  # --- verificando colunas de bib
  verifica <- c('caderno', paste0('bl', 1:(ncol(bib)-1))) %>% .[. %!in% colnames(bib)]
  if(length(verifica) > 0) stop("As colunas: '", 
                                paste0(verifica, collapse = "', '"), 
                                "' não estão definidas em bib.")
  rm(verifica)
  
  
  # --- variaveis auxiliares
  ntot  = tbl * nbl        # numero total de itens
  nblfo = ncol(bib) - 1    # numero de blocos nos cadernos 
  nitfo = tbl*nblfo        # numero de itens por caderno
  
  x <- bib[1, ] %>% as.list()
  
  result <- matrix(NA, nrow = nrow(bib), ncol = nitfo)

  item_bib <- matrix(1:ntot, nrow = nbl, ncol = tbl, byrow = TRUE)
  
  result <- apply(bib, 1, function(x){
    aux_blocos <- x[-1] %>% unlist() %>% as.vector()
    item_bib[aux_blocos, ] %>% t() %>% as.vector()
  }) %>% 
    t() %>%
    set_colnames(paste0('it', sprintf('%02d', 1:nitfo))) %>%
    set_rownames(1:nrow(.))
  
  result
}
