write.tsf_file <- function(data, col_id, col_rsp, col_peso, gabpar, tit1, tit2, nfac, arquivo, dire){
  
  arquivo <- paste0(arquivo, '_', nfac, 'D.TSF')
  
  cat('Escrevendo arquivo: ', arquivo, '\n')
  
  call = match.call()
  
  ncad <- data$caderno %>% unique() %>% length()
  
  # alterando tit2 
  tit2 <- paste0(tit2, '  -  ', nfac, ifelse(nfac == 1, " FATOR", " FATORES"))
  
  # preparando nitem
  (nitem <- sum(gabpar$aban == 0))
  
  comment <- paste0('=------------------------------------------------------------------------------=', '\n',
                    '\n',
                    'PROGRAMA GERADO AUTOMATICAMENTE PELO R'                                          , '\n',
                    '\n',
                    'Gerado em: ', format(Sys.time(), "%d-%m-%Y- %H:%M:%S")                           , '\n',
                    '\nBaseado no objeto dos objetos do R:'                                           , '\n', 
                    ' *', call$data %>% as.character()                                                , '\n', 
                    ' *', call$gabpar %>% as.character()                                              , '\n', 
                    '\n',
                    '=------------------------------------------------------------------------------=', '\n',
                    '\n\n',
                    nrow(gabpar), ' itens, ', ncad, ' cadernos, ', max(gabpar$bl), 
                    ' blocos com ', unique(table(gabpar$bl)), ' itens cada.'                          , '\n',
                    '\n', 'total de respondentes: ', nrow(data)                                       , '\n',
                    '\n', 'itens abandonados: ', nitem - length(gabpar)                               , '\n',
                    '\n\n',
                    'obs.: os gabaritos estão estruturados de tal forma que cada linha eh um bloco'   , '\n',
                    '\n\n')
  
  # preparando nomeblg
  nomeblg <- gabpar %>% 
    dplyr::filter(aban == 0) %>%
    dplyr::select(nomeblg)  %>% 
    unlist() %>%
    write.nometsf(nomeblg = ., ncol = 8)
  
  # preparnado key 
  key <- gabpar %>%
    dplyr::filter(aban == 0) %>%
    dplyr::select(bl, gab) %>%
    split(., f = .$bl) %>%
    lapply(., function(x){ paste0(x$gab, collapse = '')}) %>% 
    unlist() %>%
    paste0(., collapse = '\n')
  
  # preparando cparms
  cparms <- gabpar %>%
    dplyr::filter(aban == 0) %>%
    dplyr::select(bl, c) %>% 
    dplyr::mutate(c = format(c, nsmall = 5, digits = 5, dec = '.')) %>% 
    dplyr::select(c) %>% 
    apply(., 2, paste0, collapse = ', ') %>% 
    stringr::str_wrap(string = ., width = 70)
  
  # praparando nid
  nid <- data[, col_id] %>% nchar() %>% max()
  
  # preparando arqtxt
  arqtxt <- paste0(call$data %>% as.character(), '.txt')
  
  # preparando estrutura
  estrutura <- paste0("(",nid,"A1,1X,",nitem,'A1,1X,F3.5)')
  
  TSF_BASE <- ">TITLE   
@tit1
@tit2
>PROBLEM      NITEM=@nitem,  RESPONSE=9, NOTPRES;
>COMMENT
@comment
>NAMES
@nomeblg;
>RESPONSE     '8','A','B','C','D','E','.','*','9';
>KEY          @key;
>RELIABILIY   ALPHA;
>TETRACHORIC  PAIRWISE, NDEC = 3;
>FACTOR       NFAC = @nfac, NROOT = 4;
>FULL         OMIT = RECODE, CYCLES = 30, QUAD = 15, FREQ = 0,
CPARMS = (
@cparms);
>PRIOR        SLOPE = 1.2, INTER = (0,2);
>TECHNICAL    ITER = (100, 100, 0.01);
>INPUT        NIDCHAR = @nid, NFMT = 1, SCORES,
FILE='@arqtxt';
@estrutura
>CONTINUE
>STOP"
  
  
  
  TSF_BASE <- TSF_BASE %>%
    gsub(paste0('@tit1'     ), tit1     , .) %>%
    gsub(paste0('@tit2'     ), tit2     , .) %>%
    gsub(paste0('@nitem'    ), nitem    , .) %>%
    gsub(paste0('@comment'  ), comment  , .) %>%
    gsub(paste0('@nomeblg'  ), nomeblg  , .) %>%
    gsub(paste0('@key'      ), key      , .) %>%
    gsub(paste0('@nfac'     ), nfac     , .) %>%
    gsub(paste0('@cparms'   ), cparms   , .) %>%
    gsub(paste0('@nid'      ), nid      , .) %>%
    gsub(paste0('@arqtxt'   ), arqtxt    , .) %>%
    gsub(paste0('@estrutura'), estrutura, .)
  
  writeLines(text = TSF_BASE, con = paste0(dire, arquivo))
  
  data$lixo1 <- ''
  data$lixo2 <- ''
  
  cat('Preparando arquivo de respostas\n')
  
  cols_remove <- which(gabpar$aban == 1)
  
  data[, col_rsp] <- strsplit(data[, col_rsp], split = "", fixed = TRUE) %>% 
    lapply(., function(x){ x %>% unlist() %>%  .[-cols_remove] %>% paste0(., collapse = '')}) %>% 
    unlist()
  
  cat('Escrevendo arquivo: ', arqtxt, '\n')
  
  write.fwf(x = data[, c(col_id, 'lixo1', col_rsp, 'lixo2', col_peso)],
            file = paste0(dire, arqtxt), append = FALSE, quote = FALSE,
            sep = '', na = '', rownames = FALSE, colnames = FALSE,
            width = c(nid, 1, nitem, 1, 9))
  
}
