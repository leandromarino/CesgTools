
graf_erro_ordenado <- function(gabpar, erro = 'epatran', proj, 
                               ylim = c(0, 2), paleta = NULL, max_exib = 30){
  
  if(is.null(paleta)) paleta <- grey(seq(0, 1, length.out = 5))
  
  gabpar <- gabpar %>% 
    dplyr::filter(!!rlang::sym(erro) > 0,
                  aban == 0) %>% 
    .[order(.[, erro], decreasing = TRUE),]
  
  if(nrow(gabpar) > max_exib){
    warning(paste0("Exibindo apenas ", max_exib, ' dos ', nrow(gabpar), ' itens novos.'))  
    gabpar <- gabpar[1:max_exib,]
  }
  
  if(erro == 'epc'){
    ylim = c(0, 0.5)
  }  
  
  barplot(height = gabpar[, erro], space = c(1), ylim = ylim, col = paleta[3])
  text(x = seq(1.5, nrow(gabpar) * 2, by = 2),
       y = gabpar[, erro] + ifelse(erro == 'epc', .015, 0.05),
       labels = gabpar$it, font = 2)
  abline(h = 1, lty = 2)
  if(erro == 'epc') abline(h = 0.1, lty = 2)
  box()
  title(main = paste0(proj, '\nItens ordenados por: ', erro), 
        sub  = paste0('Exibindo apenas os primeiros ', max_exib, ' itens.'))
  
}

