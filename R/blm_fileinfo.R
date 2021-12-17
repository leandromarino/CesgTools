# funcao que apresenta informação sobre os arquivos que foram lidos através da funcao read.blm_data
# o input é o mesmo da read.blm_data

blm_fileinfo <- function(blm, dados, prm, gab, sco, ph1, 
                         exp, naoapres, par){
  
  arquivos <- c(ifelse(is.null(dados)   , NA, dados),
                ifelse(is.null(prm)     , NA, prm  ),
                ifelse(is.null(blm)     , NA, blm),
                ifelse(is.null(gab)     , NA, gab),
                ifelse(is.null(naoapres), NA, naoapres),
                ifelse(is.null(ph1)     , NA, ph1),
                ifelse(is.null(par)     , NA, par),
                ifelse(is.null(sco)     , NA, sco),
                ifelse(is.null(exp)     , NA, exp))
                
  
  file_info <- lapply(as.list(arquivos), function(x){
    x <- unlist(x)
    if(is.na(x)){as.data.frame(NA)}else{as.data.frame(file.info(x), stringsAsFactors = FALSE)}
  }) %>% 
    do.call(plyr::rbind.fill, .) %>%
    .[, -ncol(.)]
  
  file_info$arq <-  arquivos %>% 
    strsplit(., '/') %>% 
    lapply(., function(x) t(x) %>% as.data.frame(stringsAsFactors = FALSE)) %>% 
    do.call(plyr::rbind.fill, .) %>%
    .[, ncol(.)]
  
  file_info$tipo_arq <- c('dados', 'prm', 'blm', 'gab', 'naoapres', 'ph1', 'par', 'sco', 'exp')
  
  rownames(file_info) <- file_info$tipo_arq
  
  file_info %>%
    .[, c('arq', 'tipo_arq', 'size', 'mode', 'mtime', 'ctime', 'atime')]
}
