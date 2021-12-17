graf_dif_itemcomum <- function(blm_data, gabpar, itcom, modo,
                               grp_calib, versao, disc, disciplina,  proj,
                               dirgrafico, rect = TRUE){

  
  grupos = itcom %>% names() %>% as.list()
  result <- lapply(grupos, function(x){
    x <- unlist(x)
    cat(paste0("# =================================================================== !",
               '\n','Analisando Grupo: ', x, '\n',
               "# =================================================================== !",
               '\n'))
    GrafItemCom5(dirgrf = dirgrafico,
                 nomgrf = paste0('Comuns_', grp_calib, '_', x, '_', disc, '_', versao),
                 grupos = c(grp_calib, x),
                 
                 itcom = itcom[x][[1]],
                 colsitcom = c('coditem', 'nomeblg', 'itemblg', 'gab', grp_calib, x),
                 
                 gabpar1     = gabpar,
                 colsgabpar1 = c('coditem', 'nomeblg', 'it', 'bl', 'ob', 'atran', 'btran', 'c'),
                 percpr1     = blm_data$percpr[grp_calib][[1]],
                 rj1         = blm_data$expected[grp_calib][[1]],
                 gabpar2     = blm_data$grupos[x][[1]],
                 colsgabpar2 = c('nomeblg','seq'),
                 rj2         = blm_data$expected[x][[1]],
                 percpr2     = blm_data$percpr[x][[1]],
                 
                 proj = proj ,
                 anodisc = paste0(disciplina, ' - ', x, ' x ', grp_calib),
                 modo = modo,
                 rect = rect)
  })
  
  names(result) <- grupos %>% unlist()
    
  class(result) <- c('dif_itemcomum', class(result))
  
  result
  
  
}



  
#Store(graf_dif_itemcomum, lib = Bni19Bolsa, lib.loc = dirBNI19Bolsa_R)
