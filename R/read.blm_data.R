read.blm_data <- function (blm, dados, prm, gab, sco, exp = NULL, ph1, naoapres = NULL, par = NULL, debug = FALSE,
                           md_trans = 0, sd_trans = 1)
  # blm      = paste0(dir_blm, 'CHP1V3.blm')
  # dados    = paste0(dir_blm, 'DadoPT12_14_15_16_EN18CH.txt')
  # prm      = paste0(dir_blm, 'parFixPT12PT14CH.PRM')
  # gab      = paste0(dir_blm, 'GabPT12_14_15_16_EN18CH.txt')
  # naoapres = NULL
  # par      = paste0(dir_blm, 'CHP1V3.PAR')
  
  # funcao para leitura dos dados do blm
  
  
  # $cadernos      - parametros por caderno
# $grupos        - parametros por grupo
# $gabarito      - parametros para o blm
# $item_comum    - itens comuns entre todos os grupos
# $participantes - dados iniciais dos participantes

# A desenvolver: 
# leitura dos scores com juncao com os dados iniciais
# leitura do exp 
# leitura dos scores
# automatizar entrada dos parametros para buscar a partir do blm
# preparar essa funcao para ser um generica para rodar toda uma analise.
# usar funcao colorida para mostrar evolucao de cada etapa.
# criar um metodo de print personalizado
# criar Funcoes de extracao de cada item
# verificacao se todos os arquivos de input da funcao existem
# tentar capturar os arquivos de input a partir do proprio blm. tentar extrair essas infos.


{ 
  
  call = match.call()
  
  if(debug) browser()
  
  cadernos <- read.blm.form(file = blm)
  nomeblg <- read.blm.nomeblg(file = blm)
  estrut <- read.blm_stru(file = blm)
  param <- read.table(file = prm, sep = "\t", skip = 1, col.names = c("itemblg", "aorig", "borig", "c"))
  
  #checando se tem que fazer nova versao por algum item ter sido excluido.
  item_nao_calibrou <- grep('WILLNOT BE CALIBRATED', readLines(ph1))
  if(length(item_nao_calibrou) > 0){
    stop(paste0(c('', readLines(ph1)[item_nao_calibrou]), collapse = '\n'))  
  }
  rm(item_nao_calibrou)
  
  # ------------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------
  cat("Localizando gabaritos e codigo de itens nao apresentados...\n")
  
  param$item_fixo <- 1
  # preparando arquivo de gabarito de referencia
  gabs <- cbind.data.frame(itemblg = 1:length(nomeblg), nomeblg, stringsAsFactors = FALSE)
  gabs <- merge(gabs, param, by = "itemblg", all.x = TRUE)
  gabs$gab <- gabs$naoapres <- NA
  gabs[is.na(gabs$item_fixo),'item_fixo'] <- 0
  rownames(gabs) <- gabs$nomeblg
  rm(param)
  gabs[, c('epa','epb','epc')] <- 0
  
  
  # buscando parametros que foram calibrados.
  if(!is.null(par)){
    param <- read.blm_par(file = par)
    rownames(param) <- nomeblg
    param$nomeblg <- nomeblg
    
    # --- exibindo a diferença entre os parâmetros na área de trabalho durante a execução da função    
    not_aux <- gabs[gabs$item_fixo == 1, 'nomeblg'] 
    compara_prm_par <- gabs[not_aux, c('aorig', 'borig', 'c','epa','epb','epc')] - param[not_aux, c('aorig', 'borig', 'c','epa','epb','epc')]
    compara_prm_par <- compara_prm_par %>% 
      apply(., 2, summary) %>%
      as.data.frame()
    rm(not_aux)
    
    
    aux <- gabs[gabs$item_fixo == 0, 'nomeblg'] 
    param <- param[aux, ]
    gabs[aux, c('aorig', 'borig', 'c','epa','epb','epc')] <- param[aux, c('aorig', 'borig', 'c','epa','epb','epc')]
    rm(aux, param)
  }
  
  if(is.null(md_trans) | is.null(sd_trans)){
    gabs$atran <- gabs$aorig %>% round2(., 5)
    gabs$btran <- gabs$borig %>% round2(., 5)
    gabs$epatran <- gabs$epa %>% round2(., 5)
    gabs$epbctran <- gabs$epb %>% round2(., 5)
    
  }else{
    gabs$atran   <- (gabs$aorig / sd_trans) %>% round2(., 5)
    gabs$epatran <- (gabs$epa   / sd_trans) %>% round2(., 5)
    gabs$btran   <- (gabs$borig * sd_trans + md_trans) %>% round2(., 5)
    gabs$epbtran <- (gabs$epb   * sd_trans) %>% round2(., 5)
    
  }
  
  
  
  # buscando gabaritos
  gabarito <- read.blm_txt(file = gab, stru = estrut)
  gabarito$respostas <- stringr::str_pad(gabarito$respostas, width = estrut["respostas", 'tamanho'], side = "right", pad = " ")
  gabarito <- do.call(rbind, strsplit(x = gabarito$respostas, split = "", fixed = TRUE))
  for (i in 1:length(cadernos)) {
    gabs[cadernos[[i]], "gab"] <- gabarito[i, 1:length(cadernos[[i]])]
  }
  rm(i)
  
  # buscando nao apresentado
  if(!is.null(naoapres)){
    nao_apres <- read.blm_txt(file = naoapres, stru = estrut)
    nao_apres$respostas <- stringr::str_pad(nao_apres$respostas, width = estrut["respostas", 'tamanho'], side = "right", pad = " ")
    nao_apres <- do.call(rbind, strsplit(x = nao_apres$respostas, split = ""))
    for (i in 1:length(cadernos)) {
      gabs[cadernos[[i]], "naoapres"] <- nao_apres[i, 1:length(cadernos[[i]])]
    }
    rm(i)
  }
  # ------------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------
  cat("Lendo dados dos participantes...\n")
  participantes <- read.blm_txt(file = dados, stru = estrut)
  grupocaderno <- participantes[, c("grupo", "caderno")] %>% 
    apply(., 2, as.integer) %>% unique %>% as.data.frame %>% 
    .[order(.$grupo, .$caderno), ]
  
  grupos <- read.blm.grou(file = blm, group.names = T)
  aban   <- read.blm.grou(file = blm, group.names = T)
  for (i in 1:length(grupos)) {
    aux <- unlist(cadernos[grupocaderno[grupocaderno$grupo == 
                                          i, "caderno"]])
    aux <- aux[!duplicated(aux)]
    grupos[[i]] <- aux
    aban[[i]] <- setdiff(grupos[[i]], aban[[i]])
  }
  rm(aux, i)
  
  grp_names <- names(grupos)
  # ------------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------
  
  # buscando origem dos itens e adicionando ao objeto gabs
  origem <- blm_busca_origem(grupos = grupos, grp_names = grp_names)
  gabs <- merge(gabs, origem, by = 'itemblg', all = TRUE)
  rm(origem)
  
  taux <- sum(is.na(gabs$origem))
  if(taux > 0) warning("Existem ", taux, ' itens que não estão presentes em nenhum dos grupos!')
  
  
  # ------------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------
  
  
  cat('Leitura do arquivo de Scores...\n')
  score <- CesgTools::read.scoBlg(file = sco, length_id = estrut['id', 'tamanho'])
  
  participantes <- cbind(participantes, score[, 3:8])
  rm(score)
  participantes[1:5,]
  
  if(is.null(md_trans) | is.null(sd_trans)){
    participantes$profict <- participantes$proficor
    participantes$dpt     <- participantes$dpor
  }else{
    participantes$profict <- participantes$proficor * sd_trans + md_trans
    participantes$dpt     <- participantes$dpor     * sd_trans
  }
  
  participantes$caderno <- as.integer(participantes$caderno)
  participantes$grupo <- as.integer(participantes$grupo)
  
  cat('Quebrando dados dos participantes por grupo...\n')
  participantes <- split(participantes, f = participantes$grupo)
  names(participantes) <- names(grupos)
  
  #percpr
  percpr <- lapply(participantes, function(x){quantile(x$profict, probs = c(0,0.05,0.10,0.25,0.50,0.75,0.90,0.95,1.0))})
  names(percpr) <- names(grupos)
  
  
  
  
  # ------------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------
  cat("Exportando parametros por caderno...\n")
  result <- list()
  for (i in 1:nrow(grupocaderno)) {
    
    grp = grupocaderno[i, "grupo"]
    
    cad = grupocaderno[i, "caderno"]
    
    temp_result <- data.frame(itemblg = cadernos[[i]], 
                              grupo = grp, 
                              caderno = cad, 
                              aban = 0,
                              grpname = names(grupos)[grp])
    
    temp_result <- merge(temp_result, gabs, by = "itemblg", all.x = T)
    
    temp_result[temp_result$itemblg %in% aban[[grp]], "aban"] <- "1"
    
    rownames(temp_result) <- as.character(temp_result$itemblg)
    
    temp_result$seq <- 1:nrow(temp_result)
    
    cols_result <- c("seq", "grpname", "grupo", "caderno", 
                     "itemblg", "nomeblg", "gab", "naoapres", "origem", "aban", 
                     "aorig", "borig", "c", 'epa', 'epb', 'epc',
                     'atran', 'btran', 'epatran','epbtran')
    
    temp_result <- temp_result[as.character(cadernos[[cad]]), cols_result]
    
    rm(cols_result, grp, cad)
    
    temp_result$seq <- 1:nrow(temp_result)
    
    result[[i]] <- temp_result
    
    rm(temp_result)
    
  }
  rm(i)
  
  
  gab_grupos <- do.call(rbind,result) %>% .[, -4] %>% split(., f = .$grupo)
  gab_grupos <- lapply(gab_grupos, function(x){ 
    x <- x[!duplicated(x$itemblg), ]
    rownames(x) <- x$nomeblg
    x$seq <- 1:nrow(x)
    x
  })
  names(gab_grupos) <- names(grupos)
  
  
  
  # ------------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------
  if (!is.null(exp)) {
    cat('Lendo arquivos EXP... \n')
    expected <- read.blm_exp(file = exp, gab_grupos = gab_grupos)
    names(expected) <- names(grupos)
    
    if (!is.null(md_trans) & !is.null(sd_trans)) {
      for (i in 1:length(expected)) {
        expected[[i]][1,] <- expected[[i]][1, ] * sd_trans + md_trans
      }
    }
  }else{
    expected <- NA
  }
  
  # ------------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------
  cat('Preparando comparacoes de itens comuns...\n')
  
  itcomum <- list()
  
  for(i in 1:length(grupos)){
    vs_grupos <- expand.grid(i, c(1:length(gab_grupos))[-i])
    itcomum[[i]] <- apply(X = vs_grupos, MARGIN = 1, FUN = function(x){
      grp_a <- x[[1]]
      grp_b <- x[[2]]
      sort(intersect(gab_grupos[[grp_a]]$itemblg, gab_grupos[[grp_b]]$itemblg))
    })
    
    names(itcomum[[i]]) <- paste0('grp ', 
                                  sprintf('%02d', vs_grupos$Var1),
                                  ' (', 
                                  names(grupos)[vs_grupos$Var1],
                                  ') vs grp ', 
                                  sprintf('%02d', vs_grupos$Var2), 
                                  ' (', 
                                  names(grupos)[vs_grupos$Var2],
                                  ')')
    rm(vs_grupos)
  }
  rm(i)
  names(itcomum) <- names(grupos)
  
  
  
  # ------------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------
  
  cat('Leitura do PH1... \n')
  classical <- read.ph1_stats(file = ph1)
  
  for(i in 1:length(gab_grupos)){
    gab_grupos[[i]] <- merge(gab_grupos[[i]], 
                             classical[[i]][, c('itemblg', 'ntried', 'right', 'pct', 'logit', 'corr', 'bise')], by = 'itemblg', all.x = T)
    gab_grupos[[i]] <- gab_grupos[[i]] %>% .[order(.$seq),]
    rownames(gab_grupos[[i]]) <- gab_grupos[[i]]$nomeblg
  }
  rm(classical)
  
  auxiliares <- list(call      = call,
                     call.arg  = blm_call(blm = blm, dados = dados, prm = prm, gab = gab,
                                          sco = sco, exp = exp, ph1 = ph1, par = par, 
                                          debug = debug, naoapres = naoapres, 
                                          md_trans = md_trans, sd_trans = sd_trans),
                     grp_names = grp_names, 
                     blm_txt   = readLines(blm), 
                     modo      = NA,
                     file_info = blm_fileinfo(blm = blm, dados = dados, prm = prm,
                                              gab = gab, sco = sco, ph1 = ph1, 
                                              exp = exp, naoapres = naoapres, 
                                              par = par),
                     compara_prm_par = if(is.null(par)){NULL}else{compara_prm_par},
                     constantes      = c(md_trans = md_trans, 
                                         sd_trans = sd_trans))
  
  result_final <- list()
  result_final[[1]] <- result
  result_final[[2]] <- gab_grupos
  result_final[[3]] <- gabs
  result_final[[4]] <- itcomum
  result_final[[5]] <- percpr
  result_final[[6]] <- expected
  result_final[[7]] <- participantes
  result_final[[8]] <- auxiliares
  
  names(result_final) <- c('caderno', 'grupos', 'gabarito', 'item_comum', 'percpr', 'expected', 'profic', 'aux')
  
  class(result_final) <- c('blm_data', class(result_final))
  
  result_final
  
}




print.blm_data <- function(x){
  separador <- rep(paste0('# ', paste0(rep('-', 70), collapse = ''), ' !'), 3)
  
  cat('\n', '\n')
  
  # call.arg
  write.table(separador, quote = F, col.names = F, row.names = F)
  aux <- paste(names(x$aux$call.arg), '=', unlist(x$aux$call.arg))
  cat(paste0('call: \n\n', paste0(aux, collapse = ', \n')), '\n')
  cat('\n', '\n')
  
  
  # informacoes do arquivo
  write.table(separador, quote = F, col.names = F, row.names = F)
  x$aux$file_info %>%
    dplyr::select(-size, -mode, -ctime, -atime) %>%
    knitr::kable(., format = 'pandoc', 
                 caption = 'Data de criação dos arquivos utilizados nessa calibração') %>%
    print()
  cat('\n')
  
  # par x prm
  write.table(separador, quote = F, col.names = F, row.names = F)

  if(!is.null(x$aux$compara_prm_par)){
    print(knitr::kable(x$aux$compara_prm_par, format = 'pandoc', caption = 'Comparação PRM x PAR para os itens fixos'))
  }else{
    cat('Não existe comparação entre PAR e PRM, pois par não foi definido. par = NULL')
  }
  
}



