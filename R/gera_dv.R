#' Função para calcular o dígito verificador 
#'
#' @description 
#'    Função que calcula o digito verificador de módulo 11. O módulo 11 é
#'    utilizado para o cálculo dos digitos verificadores do CPF.
#'    
#' @param ids vetor com os números que serão utilizados para o cálculo do digito 
#'    verificador
#' @param sep character que indica o separador que será utilizado no resultdo 
#'    final com o digito verificador (\code{Default: NULL}).
#' @param apenas_dv boleano indicando se a saída da função deve ser apenas o
#'    dígito verificador ou o código inicial (\code{ids}) e o dv 
#'    (\code{Default: FALSE}).
#' @param dv10 character que indica como deve ser processado o resto da divisão
#'    quando igual a 10. (\code{Default = '0'})
#'    
#' @return vetor com o ids concatenado ao dv (se \code{apenas_dv = FALSE}) ou 
#'    apenas o dígito verificador (se \code{apenas_dv = TRUE}).
#'    
#' @examples 
#' set.seed(100)
#' sequenciais <- sprintf('%05d', sample(x = 1:99999, size = 100))
#'
#' gera_dv(sequenciais)
#' gera_dv(sequenciais, sep = '-')
#' gera_dv(sequenciais, sep = '-', apenas_dv = TRUE)
#' gera_dv(sequenciais, sep = '-', apenas_dv = FALSE, dv10 = 'X')
#'
#' # se fosse para geração de CPF.
#' sequenciais %>% gera_dv() %>% gera_dv()
#'
#'
#' @export


gera_dv <- function(ids, sep = NULL, apenas_dv = FALSE, dv10 = '0'){
  
  if(!is.null(sep) & apenas_dv) warning(" 'sep' não definido e 'apenas_dv' = TRUE.\nExibindo apenas o DV!")
  
  if(is.null(sep)) sep <- ''
  
  # removendo os brancos do começo e do fim
  (ids <- CesgTools::removeBrancos(ids))
  
  # identificando o maior id para padronizar os tamanhos
  (tam <- max(nchar(ids)))
  
  # adicionando '0' na frente para padronizar os tamanhos
  (ids <- stringr::str_pad(string = ids, width = tam, pad = '0'))
  
  
  
  # funcao para o calculo dos dvs no lapply
  cria_dv <- function(x, multi, dv10){
    (sep_id <- strsplit(x, split = '') %>%
       unlist() %>%
       as.integer())
    (multi_id = sep_id * multi)
    (dv <- (sum(multi_id) * 10) %% 11)
    (dv <- ifelse(dv == 10, dv10, dv))
    dv
  }
  
  
  multi <- c((tam + 1):2)
  
  dv <- lapply(X = as.list(ids), FUN = cria_dv, multi = multi, dv10 = dv10) %>% unlist()
  
  if(apenas_dv){
    return(dv) 
  }else {
    return(paste(ids, dv, sep = sep))
  }
}


