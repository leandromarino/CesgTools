blm_call <- function(blm, dados, prm, gab, sco, exp, ph1, par, 
                     debug, naoapres, md_trans, sd_trans){
  
  blm      = ifelse(is.null(blm     ), 'NULL', blm)
  dados    = ifelse(is.null(dados   ), 'NULL', dados)
  prm      = ifelse(is.null(prm     ), 'NULL', prm)
  gab      = ifelse(is.null(gab     ), 'NULL', gab)
  sco      = ifelse(is.null(sco     ), 'NULL', sco)
  exp      = ifelse(is.null(exp     ), 'NULL', exp)
  ph1      = ifelse(is.null(ph1     ), 'NULL', ph1)
  par      = ifelse(is.null(par     ), 'NULL', par)
  debug    = ifelse(is.null(debug   ), 'NULL', debug)
  naoapres = ifelse(is.null(naoapres), 'NULL', naoapres)
  md_trans = ifelse(is.null(md_trans), 'NULL', md_trans)
  sd_trans = ifelse(is.null(sd_trans), 'NULL', sd_trans)

  blm_call <- list(blm = blm, dados = dados, prm = prm, gab = gab,
                   sco = sco, exp = exp, ph1 = ph1, par = par, 
                   debug = debug, naoapres = naoapres, 
                   md_trans = md_trans, sd_trans = sd_trans)
  
}
  
  
