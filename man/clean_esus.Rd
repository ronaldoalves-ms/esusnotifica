\name{clean_esus}
\alias{clean_esus}
\title{
Curadoria dos Dados do e-SUS Notifica
}
\description{
\code{clean_esus} é uma função genérica de curadoria dos dados do e-SUS Notifica.
}
\usage{
clean_esus(dados)
}
\arguments{
  \item{dados}{
Um data frame contendo os dados do e-SUS Notifica.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
A função retorna um data frame com dados tratados do e-SUS Notifica.
}
\references{
%% ~put references to the literature/web site here ~
Este é um projeto em desenvolvimento.

Informações adiconais estão disponíveis em \url{https://datasus.saude.gov.br/notifica/}
}
\author{
%%  ~~who you are~~
Ronaldo Fernandes Santos Alves

Coordenação Geral de Informações e Análises Epidemiológicas, Departamento de Análise de Saúde e Vigilância de Doenças Não Transmissíveis, Secretaria de Vigilância em Saúde, Ministério da Saúde, Brasília/DF, Brasil.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
library(esusnotifica)
dados_esus <- clean_esus(dados_esus)

}

