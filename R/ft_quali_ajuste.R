ft_quali_ajuste <- function(qualiaju, ndec = 3, sep = ',', cores){
  
  
  (cols_percemp_txt <- colnames(qualiaju) %>% .[grep(pattern = 'ponto', .)])
  (cols_percemp_num <- attr(x = qualiaju, which = 'cols_percemp') %>% as.numeric)
  
  #definindo colunas
  cols <- c('coditem', 'nomeblg', cols_percemp_txt, 'AjuMax')
  
  #filtrando arquivo
  aux_qualiaju <- qualiaju[,cols]
  
  titulos <- data.frame(chave = cols,  
                        colB = toupper(c('CÓDIGO', 'NOMEBLG', rep('PERCENTUAIS EMPÍRICOS', length(cols_percemp_num)), 'AJU MÁX')),
                        colA = toupper(c('CÓDIGO', 'NOMEBLG', cols_percemp_num, 'AJU MÁX')),
                        stringsAsFactors = FALSE)
  
  formata_ctt <- function(x, ...){
    x = CesgTools::formataNum(valor = x, ...)
    x[x == "NaN"] <- '-'
    x
  }
  
  cols_num <- c(cols_percemp_txt, 'AjuMax')
  aux_qualiaju[,cols_num] <- formata_ctt(aux_qualiaju[,cols_num], dig = ndec, dec = sep)
  rm(cols_num)
  
  aux_qualiaju$coditem <- as.character(aux_qualiaju$coditem)
  
  ft_qualiaju <- flextable::regulartable(aux_qualiaju)
  ft_qualiaju <- flextable::set_header_df(x = ft_qualiaju, mapping = titulos, key = 'chave')
  ft_qualiaju <- flextable::merge_h(ft_qualiaju, part = "header")
  ft_qualiaju <- flextable::merge_v(ft_qualiaju, part = "header")
  ft_qualiaju <- flextable::theme_zebra(ft_qualiaju, 
                                        odd_header = cores[5], odd_body = cores[1], 
                                        even_header = cores[5], even_body = cores[2])
  ft_qualiaju <- flextable::hline(ft_qualiaju, border = officer::fp_border(width = 1.5, color = 'white'), part = 'all')
  ft_qualiaju <- flextable::vline(ft_qualiaju, border = officer::fp_border(width = 1.5, color = 'white'), part = 'all')
  ft_qualiaju <- flextable::align(ft_qualiaju, align = "center", part = "all" )
  ft_qualiaju <- flextable::fontsize(ft_qualiaju, size = 9, part = 'header')
  ft_qualiaju <- flextable::fontsize(ft_qualiaju, size = 8, part = 'body')
  ft_qualiaju <- flextable::font(ft_qualiaju, fontname = 'Arial Narrow', part = "all")
  ft_qualiaju <- flextable::color(ft_qualiaju, color = 'white', part = "header")
  ft_qualiaju <- flextable::padding(ft_qualiaju, padding.top = 0.1, padding.bottom = 0.1, padding.left = 0, padding.right = 0, part = "all" )
  ft_qualiaju <- flextable::width(ft_qualiaju, width = c(1.4, 1.4, rep(1, length(cols_percemp_num)), 1.4) / 2.54)
  ft_qualiaju
}





