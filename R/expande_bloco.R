
expande_bloco <- function(x, data, tbl, nbl, ncad, caderno = 'caderno', respostas = 'respostas'){
  
  # browser()
  # x é uma linha de bib com caderno e blocos
  aux_cad = x[[1]]
  nblcad  = length(x) - 1
  
  cat('Expandindo Caderno: ', aux_cad, '\n')
  
  # verificando se a variavel 'caderno' e 'respostas' está definida em data
  if(caderno %!in% colnames(data))   stop("Não existe a coluna: '", caderno  , "' não está definida em data.")
  if(respostas %!in% colnames(data)) stop("Não existe a coluna: '", respostas, "' não está definida em data.")
  
  data[, caderno] <- as.integer(data[, caderno])
  
  # verificando se o número de cadernos faz sentido
  if(data[, caderno] %>% max() != ncad) stop("A quantidade de cadernos em data: '", 
                                             data[, caderno] %>% max(), 
                                             "' não é compatível com o número de cadernos informado na função: '", 
                                             ncad, "'.")
  
  # filtrando dados apenas do caderno atual (aux_cad)
  temp <- data %>% dplyr::filter(!!rlang::sym(caderno) == aux_cad)
  
  
  # criando vetor com o nome das colunas dos blocos
  aux_cols <- paste0('rsp_bl', sprintf("%02d", 1:nbl))
  
  # criando matriz de respostas não apresentadas
  aux_9 <- matrix('999999999', ncol = nbl, nrow = nrow(temp)) %>%
    as.data.frame(, stringsAsFactors = FALSE) %>%
    set_colnames(aux_cols)
  
  # juntando os dados com as respostas
  temp <- cbind(temp, aux_9)
  rm(aux_9)
  
  # preparando arquivo para loop 
  aux <- unlist(x[-1]) %>% data.frame(i = 1:length(.), bl = .)
  
  apply(aux, 1, function(y){
    i  <- y[[1]]
    bl <- y[[2]]
    cat('   * Exp bloco: ', i, '\n')
    colu <- paste0('rsp_bl', sprintf("%02d", bl))
    temp[, colu] <<- substr(x     = temp[, respostas], 
                            start = (i-1) * tbl + 1,
                            stop  = i*tbl)
    return(NULL)
  }) 
  
  
  temp$rsp_TSF <- apply(temp[ , aux_cols], 1, paste, collapse = "")
  temp <- temp[, which(colnames(temp) %!in% aux_cols)]
  temp
  
}


