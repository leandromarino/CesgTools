
read.blm_exp <- function(file, gab_grupos){
  
  # o exp no blg é gerado na ordem do itemblg!!!!!!
  
  xq <- read.xqBlg(file)
  
  rj <- read.blm_rj(file)
  
  rj <- split(rj, rj$grupo)
  
  for(i in 1:length(gab_grupos)){
    
    (temp <- rj[[i]])
    (gabpar <- gab_grupos[[i]] %>% .[order(.$itemblg), ])
    
    temp$nomeblg <- gabpar %>% .[.$aban != 1, 'nomeblg']
    rownames(temp) <- temp$nomeblg
    temp <- temp[, -c(1:2)]
    temp <- temp[gab_grupos[[i]]$nomeblg, ]
    rownames(temp) <- gab_grupos[[i]]$nomeblg
    temp <- rbind(xq, temp)
    rj[[i]] <- temp
  }
  
  rj
  
}
