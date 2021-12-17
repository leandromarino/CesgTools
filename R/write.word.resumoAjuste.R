
write.word.resumoAjuste <- function(
   gabpar, ctt, qualiaju, titulo, sufixo, 
   numalt, dirgraf, dirsaida,
   cores = c("#E6E6E6", "#CCCCCC", "#B3B3B3", "#999999", "#4C4C4C", "#333333", "#191919")
 ){
  
  if(!('quali_ajuste' %in% class(qualiaju))){
    stop("O objeto de 'quali_ajuste' não pertence à classe 'quali_ajuste'.")
  }
  
  
  
  grafs <- dir(dirgraf)
  
  pega_ultimo_elemento <- function(x){
    result <- x[nrow(x),] %>% unlist %>% unique
    if(length(result) > 1){
      stop('Existem dois símbolos em sequência!')
    }
    
    result
  }
  
  ini <- stringr::str_locate_all(string = grafs, pattern =  "_")   %>% lapply(., pega_ultimo_elemento) %>% unlist() + 1
  fim <- stringr::str_locate_all(string = grafs, pattern =  "\\.") %>% lapply(., pega_ultimo_elemento) %>% unlist() - 1
  
  grafs_coditem <- substring(grafs, ini, fim)
  
  rm(ini, fim)
  
  
  
  output <- officer::read_docx(path = paste0(dirsaida, 'Resumo_Template.docx')) 
  officer::styles_info(output)
  output <- officer::body_add_par(x = output, 
                                  value = titulo, 
                                  style = "heading 1",
                                  pos = 'on')
  
  output <- officer::body_add_par(x = output, value = '', style = "Normal")
  output <- officer::body_add_par(x = output, value = '', style = "Normal")
  
  output <- officer::body_add_par(x = output, value = "Arquivo gerado em:", style = "Normal")
  output <- officer::body_add_par(x = output, value = Sys.time(), style = "Normal")
  
  output <- officer::body_add_par(x = output, value = '', style = "Normal")
  
  output <- officer::body_add_break(output)
  
  
  # itematual = 22
  for(itematual in 1:nrow(gabpar)){
    
    cat(paste0('Exportando Item : ', sprintf("%02d", itematual), ' de ', sprintf('%02d', nrow(gabpar)), '\r'))
    
    (.aux_ctt <- ctt[itematual, ])
    (.aux_gabpar <- gabpar[itematual, ])
    (.aux_grafs <- grafs[grafs_coditem == .aux_gabpar$coditem])
    
    (.aux_grafcci <- .aux_grafs[substr(.aux_grafs,1,3) == "CCI"])
    
    (.aux_qualiaju <- qualiaju[itematual,])
    
    if(nrow(.aux_qualiaju) > 0){
      (.verificaItem <- sum(.aux_qualiaju[, 'AjuMax'] > .15) > 0)
    }else{
      .verificaItem <- FALSE
    }
    
    # inicio do item
    output <- officer::body_add_par(x = output, 
                                    value = paste0("Item: ", sprintf('%02d', itematual),
                                                   " de ", nrow(gabpar),
                                                   " | Código: ", .aux_gabpar$coditem,
                                                   ifelse(.verificaItem, ' (ITEM COM POTENCIAL PROBLEMA)', '')), 
                                    style = "heading 2")
    
    output <- officer::body_add_par(x = output, value = '', style = "Normal")
    
    ### adicionar tirinha da estatística classica
    output <- flextable::body_add_flextable(x = output,
                                            value = ft_ctt(ctt = .aux_ctt, numalt = numalt, cores = cores), 
                                            align = 'center')
    output <- officer::body_add_par(x = output, value = '', style = "Normal")
    
    ### curva caracteristica do item 
    output <- officer::body_add_img(x = output, 
                                    src = paste0(dirgraf, .aux_grafcci), 
                                    width = 16.36 / 2.54,
                                    height = 8.18 / 2.54)
    output <- officer::body_add_par(x = output, value = '', style = "Normal")
    
    
    output <- officer::body_add_par(x = output,
                                    value = 'Qualidade do Ajuste', 
                                    style = "Normal")
    
    output <- flextable::body_add_flextable(x = output,
                                            value = ft_quali_ajuste(qualiaju = .aux_qualiaju, cores = cores), 
                                            align = 'center')
    
    output <- officer::body_add_break(output)
    
  }
  
  ### adicionar graficos de itens comuns
  ### adicionar tabela de dif pct
  ### adicionar tabela de dif tri
  
  
  print(output, target = paste0(dirsaida, 'ResumoAjuste_',sufixo,'.docx')) 
  
  
  
}

# 
# 
# gabpar = gabparEncc19LC_EF_PPLvs
# ctt = cbind(cttCalib_EnccEN19_LC_EF_PPL[[1]],
#             BL = as.integer(1), OB = as.integer(1:30),
#             coditem = as.integer(gabparEncc19LC_EF_PPLvs$coditem))
# qualiaju = qualiAjuEnnc19LC_EF_PPL_vs
# titulo = 'ENCCEJA 2019 - Linguagens e Códigos - Ensino Fundamental - BRAPPL - Qualidade do Ajuste'
# sufixo = 'LC_EF_PPL_vs'
# numalt = 4
# dirgraf = 'C:/Users/leandro/Documents/ENCCEJA19/RESULTADOS/GraficosItens/LC_EF_PPLvs/'
# dirsaida = 'C:/Users/leandro/Documents/ENCCEJA19/RESULTADOS/ResumoAjuste/'
# cores = c("#E6E6E6", "#CCCCCC", "#B3B3B3", "#999999", "#4C4C4C", "#333333", "#191919")
