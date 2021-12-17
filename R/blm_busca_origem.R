# funcao que cria objeto do tipo data.frame com origem e itembl apenas.
# é considerada a origem do item o primeiro grupo em que ele aparece de
# de acordo com o itemblg dele.



blm_busca_origem <- function(grupos, grp_names){

# --- criando objeto com a ordem dos grupos
ordem_grupos <- data.frame(ordem = 1:length(grp_names), grp_names = grp_names)

# --- criando objeto de origem 
origem <- grupos %>%
  unlist() %>% 
  data.frame(itemblg = .,
             nomelinha = names(.),
             origem = '', 
             stringsAsFactors = FALSE)

# --- adicionando coluna origem ao objeto 
for(x in ordem_grupos$grp_names){ 
  taux <- grep(pattern = x, x = origem$nomelinha)
  origem[taux, 'origem'] <- x
  rm(taux)
}

# --- adicionando coluna com a ordem dos grupos 
# para poder ordenar e remover aqueles que tem itemblg duplicado
origem <- merge(origem, ordem_grupos, by.x = 'origem', by.y = 'grp_names', all.x = TRUE) %>% 
  .[order(.$ordem, .$itemblg), ] %>%
  .[!duplicated(.$itemblg), ] %>%
  dplyr::select(-nomelinha, -ordem) %>%
  .[, c('itemblg', 'origem')]

rm(ordem_grupos)

origem
}

