

dif_resumo <- function(..., debug = FALSE) {
  
  if(debug) browser()
  
  
  input_list <- as.list(substitute(list(...)))[-1L]
  
  if(length(input_list) == 1){
    input <- input_list[[1]] %>% as.character() %>% get()
    if('dif_itemcomum' %in% class(input)){
      output <- list()
      output <- lapply(input, function(x){
        x <- x$InfoItem
        aux <- colnames(x)[8]
        x$tipodif_G1_x_G2 <- gsub(pattern = "_", replacement = " ", x = aux)
        colnames(x)[1] <- 'coditem'
        colnames(x) <- gsub(pattern = unlist(strsplit(aux,"_"))[1], replacement = "G1", colnames(x))
        colnames(x) <- gsub(pattern = unlist(strsplit(aux,"_"))[3], replacement = "G2", colnames(x))
        colnames(x)[9:14] <- gsub(pattern = 'G1', replacement = "G1_", colnames(x)[9:14])
        colnames(x)[9:14] <- gsub(pattern = 'G2', replacement = "G2_", colnames(x)[9:14])
        x
      })
    }
  } 
  
  
  if(length(input_list) > 1){
    output <- list()
    for(i in 1:length(input_list)){
      x <- get(as.character(input_list[[i]]))$InfoItem
      aux <- colnames(x)[8]
      x$tipodif_G1_x_G2 <- gsub(pattern = "_", replacement = " ", x = aux)
      colnames(x)[1] <- 'coditem'
      colnames(x) <- gsub(pattern = unlist(strsplit(aux,"_"))[1], replacement = "G1", colnames(x))
      colnames(x) <- gsub(pattern = unlist(strsplit(aux,"_"))[3], replacement = "G2", colnames(x))
      colnames(x)[9:14] <- gsub(pattern = 'G1', replacement = "G1_", colnames(x)[9:14])
      colnames(x)[9:14] <- gsub(pattern = 'G2', replacement = "G2_", colnames(x)[9:14])
      output[[i]] <- x
    }}
  
  output <- do.call(rbind, output)
  output <- output[order(output$coditem), ]
  rownames(output) <- 1:nrow(output)
  output
  
}
