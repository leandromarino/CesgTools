
write.blm_aban_comment <- function(gabpar){
  cols <- c('it', 'bl', 'ob', 'itemblg', 'coditem', 'gab', 'aban', 'aban_motivo')
  gabpar <- gabpar[gabpar$aban == 1,cols]
  knitr::kable(x = gabpar, row.names = FALSE, format = 'pandoc')
}

