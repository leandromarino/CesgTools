#' Conversão das sintaxes de comando do Bilog
#'
#' @description
#'   Função para, a partir de um vetor de texto de um comando de input do BilogMG,
#'   como: >GLOBAL, >SAVE, etc retorna os argumentos dos comandos em uma lista 
#'   nomeada.
#'
#' @param x vetor com a string do bilog a ser processada
#'
#' @return A saída desta função é uma lista nomeada com os parâmetros do BilogMG.
#'
#' @examples 
#' x <- c(">GLOBAL  DFNAME='E19_CH_V0.txt',","         NPARM = 3,","         SAVE;")
#'
#' read.blm_commands(x)
#'
#' @seealso \code{\link{read.blm_input}}
#' @export



read.blm_commands <- function(x){
  x <- paste0(x, collapse = ' ') %>%      # juntando as possíveis múltiplas linhas em uma string
    gsub('\\s{2,}', ' ', .) %>%           # removendo espaços adicionais
    gsub('\\s{0,}=\\s{0,}', '=', .) %>%   # removendo espaços entre o simbolo do igual
    strsplit(., split = '\\s|,') %>%      # quebrando a string a cada espaço ou vírgula
    unlist(.) %>% .[-1] %>%               # transformando em vetor
    gsub("[']|[\"]|[;]", "", .) %>%       # removendo aspas duplas, aspas simples e ponto-virgula
    .[. != '']                            # removendo elementos vazios
  
  
  (com_igual <- grepl('=', x) %>% which)
  
  a <- strsplit(x[ com_igual], '=') %>% lapply(X = ., '[', i = 1) %>% unlist()
  b <- strsplit(x[ com_igual], '=') %>% lapply(X = ., '[', i = 2)
  names(b) <- a
  
  c <- as.list(x[-com_igual])
  names(c) <- c
  
  result <- c(b,c)
  rm(a, b, c, x)
  result  
}




