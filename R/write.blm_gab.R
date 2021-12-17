# gabpar  = gabpar_PT17_CN 
# itens   = itens_PT17
# formini = 148
# grupo   = 6
# blm_str = read.blm_stru(file = 'C:/Users/leandro/Documents/BNI2019/PT_ENEM19_DIGITAL/Blg_PT17/CN/PT19D_CN_V1.blm')


write.blm_gab <- function(gabpar, itens, formini, grupo, grpname, blm_str, file){
 
# --- verificando colunas de gabpar
verifica <- c('it', 'bl', 'ob', 'gab') %>% .[. %!in% colnames(gabpar)]
if(length(verifica) > 0) stop("As colunas: '", 
                              paste0(verifica, collapse = "', '"), 
                              "' não estão definidas em gabpar.")
rm(verifica)

# --- verificando classe do blm_str
if('blm_stru' %!in% class(blm_str)){
  stop("O objeto de 'blm_str' não pertence à classe blm_stru")
  }

# --- ordenando o gabpar por bloco e ordem no bloco
gabpar <- gabpar %>% .[order(.$bl, .$ob), ]


# --- criando o vetor de respostas
respostas <- gabpar$gab[itens] %>%
  matrix(., nrow = nrow(itens), ncol = ncol(itens)) %>%
  as.data.frame(. , stringsAsFactors = FALSE) %>%
  apply(X = ., MARGIN = 1, paste0, collapse = '')



# --- preparando arquivo de gabarito
cat('Preparando arquivo de gabaritos\n')
gab <- matrix(NA, nrow = length(respostas), ncol = nrow(blm_str)) %>% 
  as.data.frame(., stringsAsFactors = FALSE) %>%
  set_colnames(rownames(blm_str)) %>%
  dplyr::mutate(id        = 1:length(!!respostas) %>% 
                  gera_dv() %>% 
                  stringr::str_pad(., width = blm_str$stru[1], pad = '0'),
                caderno   = formini + (1:nrow(itens)),
                grupo     = !!grupo,
                respostas = !!respostas)


if(!is.null(file)){
  cat('Exportando arquivo de gabaritos\n')
  gdata::write.fwf(x = gab, file = file, append = TRUE, quote = FALSE, 
                   sep = '', na = '', rownames = FALSE, colnames = FALSE, 
                   width = blm_str$stru)
}

gab

}


