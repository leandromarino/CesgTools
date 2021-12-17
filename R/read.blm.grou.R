
read.blm.grou <- function(file, group.names = F){
  
  blm <- readLines(con = file)
  check_grou <- substr(blm, 1, 5) == ">GROU"
  check_estr <- substr(blm, 1, 1) == "("
  inicio <- which(check_grou == TRUE)
  fim <- c(which(check_grou == TRUE)[-1],which(check_estr == TRUE)[1])-1
  group <- list()
  for(i in 1:length(inicio)){
    aux <- blm[inicio[i]:fim[i]]
    group[[i]] <- paste0(aux,collapse = '')
  }
  group <- unlist(group)
  group <- lapply(strsplit(group,'[(]|[)]'), '[', i = 2)
  group <- unlist(group)
  group <- strsplit(group,',')
  for(i in 1:length(group)){
    mode(group[[i]]) <- 'integer'
  }
  
  if(group.names){
    blm_group <- blm[ min(which(check_grou)) : which(check_estr) ]
    
    grp_name_ini <- stringr::str_locate_all(string = paste0(blm_group, collapse = ''), 
                                            pattern = "GNA[:alpha:]{0,}\\s{0,}=\\s{0,}\"{0,}\'{0,}")[[1]][,'end']+1
    
    grp_name_fim <- stringr::str_locate_all(string = paste0(blm_group, collapse = ''),
                                            pattern = "\'{0,}\\,{0,}\\s{0,}LEN[:alpha:]{0,}\\s{0,}=\\s{0,}")[[1]][,'start']-1
    group_names <- 
      blm_group %>% 
      paste0(., collapse = '') %>% 
      substring(., first = grp_name_ini, last = grp_name_fim) %>% 
      removeBrancos(.)
    names(group) <- group_names
    
    rm(group_names)
  }
  
  
  group
}

