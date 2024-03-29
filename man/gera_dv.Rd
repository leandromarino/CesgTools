% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gera_dv.R
\name{gera_dv}
\alias{gera_dv}
\title{Função para calcular o dígito verificador}
\usage{
gera_dv(ids, sep = NULL, apenas_dv = FALSE, dv10 = "0")
}
\arguments{
\item{ids}{vetor com os números que serão utilizados para o cálculo do digito 
verificador}

\item{sep}{character que indica o separador que será utilizado no resultdo 
final com o digito verificador (\code{Default: NULL}).}

\item{apenas_dv}{boleano indicando se a saída da função deve ser apenas o
dígito verificador ou o código inicial (\code{ids}) e o dv 
(\code{Default: FALSE}).}

\item{dv10}{character que indica como deve ser processado o resto da divisão
quando igual a 10. (\code{Default = '0'})}
}
\value{
vetor com o ids concatenado ao dv (se \code{apenas_dv = FALSE}) ou 
   apenas o dígito verificador (se \code{apenas_dv = TRUE}).
}
\description{
Função que calcula o digito verificador de módulo 11. O módulo 11 é
   utilizado para o cálculo dos digitos verificadores do CPF.
}
\examples{
set.seed(100)
sequenciais <- sprintf('\%05d', sample(x = 1:99999, size = 100))

gera_dv(sequenciais)
gera_dv(sequenciais, sep = '-')
gera_dv(sequenciais, sep = '-', apenas_dv = TRUE)
gera_dv(sequenciais, sep = '-', apenas_dv = FALSE, dv10 = 'X')

# se fosse para geraÃ§Ã£o de CPF.
sequenciais \%>\% gera_dv() \%>\% gera_dv()


}
