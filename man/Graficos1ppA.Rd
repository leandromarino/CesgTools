% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Graficos1pp.R
\name{Graficos1ppA}
\alias{Graficos1ppA}
\title{Gera graficos TRI e por alternativa}
\usage{
Graficos1pp(percemp, gabpar, colsgabpar, percmodelo, percpr, item,
    titulo, aesc, besc, s12, m12,modo = 'normal', mincategoria = 30,
    limx = c(-10,510), back = 'transparent',retangulo = F)
}
\arguments{
\item{percemp}{Objeto resultante da funcao \link{PercEmp}.}

\item{gabpar}{Objeto do tipo \code{data.frame} contendo informacoes sobre
os gabaritos e tambem os parametros do item para plotagem dos
graficos}

\item{colsgabpar}{Vetor com o nome das colunas que serao utilizadas no gabpar.
Obrigatoriamente este objeto deve ser na ordem: \emph{item, bloco, ordem no bloco,
numero do item no Bilog, nome do item no Bilog, codigo do item no banco de itens,
gabarito do item, parâmetro a (TRI), parâmetro b (TRI) e parâmetro c (TRI)}.
Normalmente estas colunas nomeiam-se \code{c('it','bl','ob','itemblg','nomeblg',
'codigo','gab','a','b','c')}.}

\item{percmodelo}{Percentuais esperados nos modelos de TRI.}

\item{percpr}{Percentis (objeto gerado a partir da funcao quantile).
Obrigatoriamente deve ter \code{probs =}.}

\item{item}{Sequencial do item desejado para a plotagem das curvas.}

\item{titulo}{Titulo do grafico.}

\item{aesc}{constante 'a' de transformacao para a escala de divulgacao}

\item{besc}{constante 'b' de transformacao para a escala de divulgacao}

\item{s12}{constante 's' de transformacao para a escala saeb ou demais (0,1)}

\item{m12}{constante 'm' de transformacao para a escala saeb ou demais (0,1)}

\item{modo}{Modo em que a calibracao foi realizada, opcoes: \code{c('normal','logistico'}.
Saida sempre em modo logistico. Default: \code{'normal'}.}

\item{mincategoria}{Numero minimo de estudantes para que o percentual empirico seja
plotado. Default: \code{mincategoria = 30}.}

\item{limx}{Limites para o eixo x (proficiencia). Default: \code{limx = c(-10,510)}.}

\item{back}{Cor de fundo do grafico. Default: \code{back = 'transparent'}.
Para gerar arquivos mais leves utilizar \code{back = 'white'}.}

\item{retangulo}{Booleano que indica se o retangulo cinza deve ser plotado
como fundo do grafico (estilo ggplot2). Default: \code{retangulo = F}.}
}
\value{
A saida desta funcao eh um grafico.
}
\description{
Funcao responsavel por gerar um objeto grafico a partir de um conjunto
    de informacoes dos modelos de TRI e dos dados
}
\examples{
## ainda falta adicionar alguns exemplos
}
