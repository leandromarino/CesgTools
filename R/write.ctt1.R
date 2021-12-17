write.ctt1 <- function (ctt, gabpar = NULL, colsgabpar = NULL, etapa = NULL, 
                        area = NULL, exporta_dir) 
{
  nmctt <- ctt
  ctt <- get(ctt)
  if (!is.null(gabpar)) {
    if (!is.null(colsgabpar)) {
      if (sum(is.element(colnames(gabpar), colsgabpar)) != 
          length(colsgabpar)) 
        stop("Alguma coluna de gabpar não está corretamente definida em colsgabpar")
    }
    if (is.null(colsgabpar)) {
      warning("Nao foram informadas as colunas de gabpar - retornando CTT com todas as colunas de gabpar")
      colsgabpar <- colnames(gabpar)
    }
  }
  cols_remove <- c("PBISE", "PBiseA", "PBiseB", "PBiseC", "PBiseD", "PBiseE", "PBise ", "PBise*", "PBise.")
  for (i in c(1, 3)) ctt[[i]] <- ctt[[i]][, !is.element(colnames(ctt[[i]]), 
                                                        cols_remove)]
  if (!is.null(gabpar)) {
    ctt[[1]] <- cbind(Etapa = etapa, `Área conhecimento` = area, gabpar[, colsgabpar], ctt[[1]])
    ctt[[3]] <- cbind(Etapa = etapa, `Área conhecimento` = area, gabpar[as.integer(ctt[[3]]$IT), colsgabpar], ctt[[3]])
  }
  
  
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "Estatisticas")
  openxlsx::writeData(wb, sheet = "Estatisticas", x =  ctt[[1]], keepNA = FALSE, rowNames = FALSE, colNames = TRUE)
  
  openxlsx::addWorksheet(wb, "VerificarItens")
  openxlsx::writeData(wb, sheet = "VerificarItens", x = ctt[[3]], keepNA = FALSE, rowNames = FALSE, colNames = TRUE)
  
  openxlsx::saveWorkbook(wb = wb, file = paste0(exporta_dir, nmctt, ".xlsx"), overwrite = T)  
  
  rm(wb)
  
}

