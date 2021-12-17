#' Not in
#' @param  x vetor
#' @param  y vetor
#'
#' @return vetor de valores lógicos, se x está em y, etc.
#'
#' @examples
#' \dontrun{
#' x %nin% y}
#'
#' @export

`%!in%` <- function (x, table){
  !(match(x, table, nomatch = 0L) > 0L)
}

