#' Leitura da sintaxes do BIlog
#'
#' @description
#'   Função para, a partir de um arquivo .blm retornar uma lista com os objetos 
#'   e parâmetros que estão sendo usados como input do Bilog.
#'
#' @param blm caminho do arquivo .blm
#'
#' @return Objeto da classe blm_input contendo uma lista com os parâmetros de
#'  GLOBAL ,SAVE, LENGTH e INPUT. 
#'
#' @export 



read.blm_input <- function(blm){
  txt_blm <- readLines(blm)
  
  
  (l_comments <- grepl(pattern = '>COMMENTS', x = txt_blm) %>% which)
  (l_global   <- grepl(pattern = '>GLOBAL'  , x = txt_blm) %>% which)
  (l_save     <- grepl(pattern = '>SAVE'    , x = txt_blm) %>% which)
  (l_length   <- grepl(pattern = '>LENGTH'  , x = txt_blm) %>% which)
  (l_input    <- grepl(pattern = '>INPUT'   , x = txt_blm) %>% which)
  (l_items    <- grepl(pattern = '>ITEMS'   , x = txt_blm) %>% which)
  (l_test     <- grepl(pattern = '>TEST'    , x = txt_blm) %>% which)
  (l_form     <- grepl(pattern = '>FORM'    , x = txt_blm) %>% which)
  (l_drift    <- grepl(pattern = '>DRIFT'   , x = txt_blm) %>% which)
  (l_calib    <- grepl(pattern = '>CALIB'   , x = txt_blm) %>% which)
  (l_quad     <- grepl(pattern = '>QUAD'    , x = txt_blm) %>% which)
  (l_priors   <- grepl(pattern = '>PRIORS'  , x = txt_blm) %>% which)
  (l_score    <- grepl(pattern = '>SCORE'   , x = txt_blm) %>% which)
  (l_quads    <- grepl(pattern = '>QUADS'   , x = txt_blm) %>% which)
  
  
  # global
  if(length(l_save) == 0){
    l_global <- l_global:(l_length[1] - 1)
  }else{
    l_global <- l_global:(l_save[1] - 1)
  }
  (txt_global <- read.blm_commands(x = txt_blm[l_global]))
  rm(l_global)
  
  
  # save
  if(length(l_save) > 0){
    (l_save <- l_save:(l_length[1] - 1))
    (txt_save <- read.blm_commands(x = txt_blm[l_save]))
  }
  rm(l_save)
  
  
  # length
  if(length(l_length) > 0){
    (l_length <- l_length:(l_input[1] - 1))
    (txt_length <- read.blm_commands(x = txt_blm[l_length]))
  }
  rm(l_length)
  
  
  
  # input
  if(length(l_input) > 0){
    (l_input <- l_input:(l_items[1] - 1))
    (txt_input <- read.blm_commands(x = txt_blm[l_input]))
  }
  rm(l_input)
  
  
  result <- c(txt_global, txt_save, txt_length, txt_input)
  names(result) <- toupper(names(result))
  class(result) <- c('blm_input', class(result))
  
  result
}

