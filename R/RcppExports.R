# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

Escore <- function(respostas, gabarito, NumCad, NumItens, CodAcer, CodErro, CodNaoAp, nblform, tbl) {
    .Call('_CesgTools_Escore', PACKAGE = 'CesgTools', respostas, gabarito, NumCad, NumItens, CodAcer, CodErro, CodNaoAp, nblform, tbl)
}

PontoBisserial <- function(respostas, scores, itempos, resposta_possivel, CodNaoAp, Peso, mostra_napres) {
    .Call('_CesgTools_PontoBisserial', PACKAGE = 'CesgTools', respostas, scores, itempos, resposta_possivel, CodNaoAp, Peso, mostra_napres)
}

RespItem <- function(respostas, itempos) {
    .Call('_CesgTools_RespItem', PACKAGE = 'CesgTools', respostas, itempos)
}

