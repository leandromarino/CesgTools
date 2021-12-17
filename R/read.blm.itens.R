
read.blm.itens <- function(blm, dados, prm, gab, naoapres){
  #dir_blm <- 'C:/Users/leandro/Documents/ENCCEJA18/BLG/MT_EF/'
  
  # blm <- paste0(dir_blm, 'Encc18_MT_EF_V2.BLM')
  # dados <- paste0(dir_blm, 'Encc18_MT_EF_V1.txt')
  # prm <- paste0(dir_blm, 'ItemFixEncc18_MT_EF_V1.prm')
  # gab <- paste0(dir_blm, 'gabEncc18_MT_EF_V1.txt')
  # naoapres <- paste0(dir_blm, 'naEncc18_MT_EF_V1.txt')
  # 
  cadernos <- read.blm.form(file = blm)
  nomeblg  <- read.blm.nomeblg(file = blm)
  estrut   <- read.blm.stru(file = blm)
  param    <- read.table(file = prm, sep = '\t', skip = 1, col.names = c('itemblg', 'a', 'b', 'c'))
  
  
  cat('\rLocalizando gabaritos e código de itens não apresentados..\r')
  
  #criando arquivo com parametros e gabaritos
  gabs <- cbind.data.frame(itemblg = 1:length(nomeblg), nomeblg, stringsAsFactors = FALSE)
  gabs <- merge(gabs, param, by = 'itemblg', all.x = TRUE)
  gabs$gab <- gabs$naoapres <- NA
  rm(param)
  
  
  #lendo gabarito
  gabarito <- read.blm.data(file = gab, stru = estrut)
  #adicionando espaços a esquerda de gabarito. (apenas para poder fazer a separacao das colunas)
  gabarito$respostas <- stringr::str_pad(gabarito$respostas, width = estrut['respostas',], side = 'right', pad = ' ')
  #quebrando os gabaritos em colunas separadas 
  gabarito <- do.call(rbind, strsplit(x = gabarito$respostas, split = ''))
  
  
  nao_apres <- read.blm.data(file = naoapres, stru = estrut)
  #adicionando espaços a esquerda de gabarito. (apenas para poder fazer a separacao das colunas)
  nao_apres$respostas <- stringr::str_pad(nao_apres$respostas, width = estrut['respostas',], side = 'right', pad = ' ')
  #quebrando os nao_apress em colunas separadas 
  nao_apres <- do.call(rbind, strsplit(x = nao_apres$respostas, split = ''))
  
  #adicionando os gabaritos no gabs
  for(i in 1:length(cadernos)){
    gabs[cadernos[[i]], 'gab'] <- gabarito[i, 1:length(cadernos[[i]])]
    gabs[cadernos[[i]], 'naoapres'] <- nao_apres[i, 1:length(cadernos[[i]])]
  }
  rm(i)
  
  
  #ler o conjunto de dados gerador das provas - apenas para pegar o relacionamento entre grupo e caderno
  cat('\rLendo dados dos participantes...\r')
  participantes <- read.blm.data(file = dados, stru = estrut)
  
  grupocaderno <- participantes[, c('grupo', 'caderno')] %>% 
    apply(., 2, as.integer) %>% 
    unique %>% 
    as.data.frame %>%
    .[order(.$grupo, .$caderno), ]
  
  str(grupocaderno)
  rm(participantes)
  
  
  # itens abandonados em cada grupo
  grupos   <- read.blm.grou(file = blm, group.names = T)
  aban     <- read.blm.grou(file = blm, group.names = T)
  
  #todos os itens dos grupos e apenas os abandonados em cada grupo
  for(i in 1:length(grupos)){
    aux <- unlist(cadernos[grupocaderno[grupocaderno$grupo==i,'caderno']])
    aux <- aux[!duplicated(aux)]
    grupos[[i]] <- aux
    aban[[i]] <- setdiff(grupos[[i]], aban[[i]])
  };rm(aux,i)
  
  cat('\rExportando parâmetros por caderno...\r')
  result <- list()
  for(i in 1:length(cadernos)){
    grp = grupocaderno[i,'grupo']
    cad = grupocaderno[i,'caderno']
    result[[i]] <- data.frame(itemblg = cadernos[[i]], grupo = grp, caderno = cad, aban = 0, grpname = names(grupos)[grp])
    result[[i]] <- merge(result[[i]], gabs, by = 'itemblg', all.x = T)
    result[[i]][result[[i]]$itemblg %in% aban[grp], 'aban'] <- '1'
    rownames(result[[i]]) <- as.character(result[[i]]$itemblg)
    result[[i]]$seq <- 1:nrow(result[[i]])
    cols_result <- c('seq', 'grpname', 'grupo', 'caderno', 'itemblg', 'nomeblg', 'gab', 'naoapres', 'a', 'b', 'c', 'aban')
    result[[i]] <- result[[i]][as.character(cadernos[[cad]]), cols_result]
    rm(cols_result, grp, cad)
  }
  rm(i)
  result
}

