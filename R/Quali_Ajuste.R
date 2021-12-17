# gabpar = gabpar1805d
# colsgabpar = c('it','codit','nomeblg','atran','btran','c','gab')
# percemp = percemp1805d
# percpr = percpr1805d
# modo = 'normal'
# aesc = amat
# besc = bmat
# int = 25
# ndec = 5

# gabpar  = gabparEncc19CN_EF_PPLvs
# percemp = percempEncc19CN_EF_PPLvs
# percpr = percprEncc19CN_EF_PPLvs
# modo = 'normal'
# aesc = NULL
# besc = NULL
# ndec = 3

quali_ajuste <- function(gabpar, percemp, percpr, modo, aesc = NULL, besc = NULL, ndec = NULL, min = 30, debug = FALSE){
  # gabpar  = gabparEncc17CH_EF
  # percemp = percemp_encceja17_EFReg_CH
  # percpr = percpr_encceja17_EFReg_CH
  # modo = 'normal'
  # aesc = aCH_EF
  # besc = bCH_EF
  # ndec = 3
  if(debug) browser()
  
  #verificando colunas dentro do intervalo de 5% a 95%
  (cols_percemp <- as.numeric(colnames(percemp[[1]][[1]])))
  (col_min <- abs(percpr[c(2)] - cols_percemp) %>% which.min)
  (col_max <- abs(percpr[c(8)] - cols_percemp) %>% which.min)
  
  (cols <- seq(from = col_min, to = col_max, by = 1))
  (cols_percemp <- colnames(percemp[[1]][[1]])[cols])
  
  df = data.frame(matrix(vector(), 0, length(cols) + 12, 
                         dimnames = list(c(), c('it', 'coditem', 'nomeblg', 'gab','aban', 'aorig', 'borig', 'c',
                                                'atran', 'btran', paste0('ponto', cols_percemp), 'AjuMin', 'AjuMax'))),
                  stringsAsFactors = F)
  
  for(item in 1:nrow(gabpar)){
    (nmblg <- gabpar[item, 'nomeblg'])
    (codit <- gabpar[item, 'coditem'])
    (gab  <- gabpar[item, "gab"])
    (para <- gabpar[item, "aorig"])
    (parb <- gabpar[item, "borig"])
    (parc <- gabpar[item, "c"])
    (aban <- gabpar[item, 'aban'])
    D = 1
    if(modo == 'normal'){
      D <- 1.7
    }
    
    #ajustando para o modo normal ou logistico
    para <- para * D
    
    
    if(!is.null(aesc)){
      (par_at <- para / aesc)
      (par_bt <- aesc * parb + besc)
    }else{
      par_at <- NA
      par_bt <- NA
    }
    
    print(paste0("Analisando item - It",sprintf('%03d',item), "_", nmblg))
    
    data.frame(tag = c(cols, 'minimo', 'maximo'))
    df[item, 'it'] <- item
    df[item, 'nomeblg'] <- nmblg
    df[item, 'coditem' ] <- codit
    df[item, 'gab'] <- gab
    df[item, c('aban', 'aorig', 'borig', 'c', 'atran', 'btran')] <- c(aban, para, parb, parc, par_at, par_bt)
    
    pts <- as.numeric(colnames(percemp[[1]][[1]]))
    
    (colu <- gsub(pattern = '-', replacement = '.', paste0('ponto', cols_percemp)))
    if(gab != "X"){
      if(is.null(aesc)){
        df[item, colu] <- abs(CesgTools::plogis3(pts, para, parb, parc) - percemp[[2]][[item]][gab,])[cols]
      }else{
        df[item, colu] <- abs(CesgTools::plogis3(pts, par_at, par_bt, parc) - percemp[[2]][[item]][gab,])[cols]
      }
    }else{
      df[item, colu] <- NA
    }
    
    df[item,c('AjuMin','AjuMax')] <- range(df[item, colu])
    
  }
  rm(item, para, parb, parc, par_at, par_bt, nmblg, codit, pts, D, gab)
  
  df$statusAju <- ''
  df$statusAju[df$AjuMax > 0.15] <- '> 0.15 *'
  df$statusAju[df$AjuMax > 0.17] <- '> 0.17 **'
  df$statusAju[df$AjuMax > 0.20] <- '> 0.20 ***'
  
  if(!is.null(ndec)){
    df[,c(colu,'AjuMin','AjuMax')] <- round(df[,c(colu,'AjuMin','AjuMax')], ndec)
  }
  
  df$aban <- gabpar$aban
  df[df$aban == 1, c(colu, 'AjuMin', 'AjuMax')] <- NA
  df[df$aban == 1, c('statusAju')] <- ''

  rm(cols)
  
  attr(x = df, which = 'cols_percemp') <- cols_percemp
  class(df) <- c('quali_ajuste', class(df))
  
  df
}

# Store(quali_ajuste, lib = funcoes, lib.loc = dirEncceja19)
