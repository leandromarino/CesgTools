// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// Escore
DataFrame Escore(DataFrame respostas, DataFrame gabarito, int NumCad, int NumItens, std::string CodAcer, std::string CodErro, std::string CodNaoAp, int nblform, int tbl);
RcppExport SEXP _CesgTools_Escore(SEXP respostasSEXP, SEXP gabaritoSEXP, SEXP NumCadSEXP, SEXP NumItensSEXP, SEXP CodAcerSEXP, SEXP CodErroSEXP, SEXP CodNaoApSEXP, SEXP nblformSEXP, SEXP tblSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type respostas(respostasSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type gabarito(gabaritoSEXP);
    Rcpp::traits::input_parameter< int >::type NumCad(NumCadSEXP);
    Rcpp::traits::input_parameter< int >::type NumItens(NumItensSEXP);
    Rcpp::traits::input_parameter< std::string >::type CodAcer(CodAcerSEXP);
    Rcpp::traits::input_parameter< std::string >::type CodErro(CodErroSEXP);
    Rcpp::traits::input_parameter< std::string >::type CodNaoAp(CodNaoApSEXP);
    Rcpp::traits::input_parameter< int >::type nblform(nblformSEXP);
    Rcpp::traits::input_parameter< int >::type tbl(tblSEXP);
    rcpp_result_gen = Rcpp::wrap(Escore(respostas, gabarito, NumCad, NumItens, CodAcer, CodErro, CodNaoAp, nblform, tbl));
    return rcpp_result_gen;
END_RCPP
}
// PontoBisserial
List PontoBisserial(DataFrame respostas, std::vector<double> scores, NumericMatrix itempos, std::vector<std::string> resposta_possivel, std::string CodNaoAp, std::vector<double> Peso, int mostra_napres);
RcppExport SEXP _CesgTools_PontoBisserial(SEXP respostasSEXP, SEXP scoresSEXP, SEXP itemposSEXP, SEXP resposta_possivelSEXP, SEXP CodNaoApSEXP, SEXP PesoSEXP, SEXP mostra_napresSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type respostas(respostasSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type scores(scoresSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type itempos(itemposSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type resposta_possivel(resposta_possivelSEXP);
    Rcpp::traits::input_parameter< std::string >::type CodNaoAp(CodNaoApSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type Peso(PesoSEXP);
    Rcpp::traits::input_parameter< int >::type mostra_napres(mostra_napresSEXP);
    rcpp_result_gen = Rcpp::wrap(PontoBisserial(respostas, scores, itempos, resposta_possivel, CodNaoAp, Peso, mostra_napres));
    return rcpp_result_gen;
END_RCPP
}
// RespItem
CharacterVector RespItem(DataFrame respostas, NumericMatrix itempos);
RcppExport SEXP _CesgTools_RespItem(SEXP respostasSEXP, SEXP itemposSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type respostas(respostasSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type itempos(itemposSEXP);
    rcpp_result_gen = Rcpp::wrap(RespItem(respostas, itempos));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_CesgTools_Escore", (DL_FUNC) &_CesgTools_Escore, 9},
    {"_CesgTools_PontoBisserial", (DL_FUNC) &_CesgTools_PontoBisserial, 7},
    {"_CesgTools_RespItem", (DL_FUNC) &_CesgTools_RespItem, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_CesgTools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
