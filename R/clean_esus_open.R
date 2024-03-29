#' Curadoria dos Dados Abertos do e-SUS Notifica 
#'
#' \code{clean_esus_open} e uma funcao generica para o pre-processamento dos dados abertos do e-SUS Notifica.
#'
#' @param dados Um data frame contendo os dados abertos do e-SUS Notifica.
#'
#' @return A funcao retorna um data frame com dados tratados do e-SUS Notifica.
#'
#' @examples
#' dados_esus <- clean_esus_open(dados_esus)
#'
#' @export



# PACOTES INSTALADOS PARALELAMENTE

#usethis::use_package("magrittr", type = "Imports")
#usethis::use_package("dplyr", type = "Imports")
#usethis::use_package("lubridate", type = "Imports")
#usethis::use_package("stringr", type = "Imports")
#usethis::use_package("abjutils", type = "Imports")


#usethis::use_pipe(export = TRUE)



clean_esus_open <- function(dados){
  
  
  nomesVars <- names(dados)
  
  
  # DATA DO INICIO DOS SINTOMAS
  
  if("dataInicioSintomas" %in% nomesVars){
    
    dados$dataInicioSintomas <- lubridate::as_date(dados$dataInicioSintomas)
    
    dados$dataInicioSintomas[dados$dataInicioSintomas < lubridate::as_date("2020-01-01")] <- NA
    dados$dataInicioSintomas[dados$dataInicioSintomas > Sys.Date()] <- NA
    
  }
  
  
  # DATA DA NOTIFICACAO
  
  if("dataNotificacao" %in% nomesVars){
    
    dados$dataNotificacao <- lubridate::as_date(dados$dataNotificacao)
    
    dados$dataNotificacao[dados$dataNotificacao < lubridate::as_date("2020-01-01")] <- NA
    dados$dataNotificacao[dados$dataNotificacao > Sys.Date()] <- NA
    
  }
  
  
  # DATA DO TESTE
  
  if("dataTeste" %in% nomesVars){
    
    dados$dataTeste <- lubridate::as_date(dados$dataTeste)
    
    dados$dataTeste[dados$dataTeste < lubridate::as_date("2020-01-01")] <- NA
    dados$dataTeste[dados$dataTeste > Sys.Date()] <- NA
    
  }
  
  
  # DATA DO TESTE SOROLOGICO
  
  if("dataTesteSorologico" %in% nomesVars){
    
    dados$dataTesteSorologico <- lubridate::as_date(dados$dataTesteSorologico)
    
    dados$dataTesteSorologico[dados$dataTesteSorologico < lubridate::as_date("2020-01-01")] <- NA
    dados$dataTesteSorologico[dados$dataTesteSorologico > Sys.Date()] <- NA
    
  }
  
  
  # DATA DE ENCERRAMENTO
  
  if("dataEncerramento" %in% nomesVars){
    
    dados$dataEncerramento <- lubridate::as_date(dados$dataEncerramento)
    
    dados$dataEncerramento[dados$dataEncerramento < lubridate::as_date("2020-01-01")] <- NA
    dados$dataEncerramento[dados$dataEncerramento > Sys.Date()] <- NA
    
  }
  
  
  # DATA DA CRIACAO DO REGISTRO
  
  if("dataRegistro" %in% nomesVars){
        
    dados$dataRegistro <- lubridate::as_date(dados$dataRegistro)
    
    dados$dataRegistro[dados$dataRegistro < lubridate::as_date("2020-01-01")] <- NA
    dados$dataRegistro[dados$dataRegistro > Sys.Date()] <- NA
    
  }
  
  
  # DATA DA ATUALIZACAO DO REGISTRO
  
  if("dataAtualizacao" %in% nomesVars){
    
    dados$dataAtualizacao <- lubridate::as_date(dados$dataAtualizacao)
    
    dados$dataAtualizacao[dados$dataAtualizacao < lubridate::as_date("2020-01-01")] <- NA
    dados$dataAtualizacao[dados$dataAtualizacao > Sys.Date()] <- NA
    
  }
  
    
  # SEXO
  
  if("sexo" %in% nomesVars){
    
    dados$sexo <- abjutils::rm_accent(dados$sexo)
    dados$sexo <- stringr::str_remove_all(dados$sexo, "[^[:alpha:]]")
    dados$sexo <- stringr::str_to_upper(dados$sexo)
    
    dados$sexo[dados$sexo == ""] <- NA
    dados$sexo[dados$sexo == "INDEFINIDO"] <- NA
    dados$sexo[dados$sexo == "MASCULINO"] <- "1"
    dados$sexo[dados$sexo == "FEMININO"] <- "2"
    
  }
  
  
  # RACA/COR
  
  if("racaCor" %in% nomesVars){
    
    dados$racaCor <- abjutils::rm_accent(dados$racaCor)
    dados$racaCor <- stringr::str_remove_all(dados$racaCor, "[^[:alpha:]]")
    dados$racaCor <- stringr::str_to_upper(dados$racaCor)
    
    dados$racaCor[dados$racaCor == ""] <- NA
    dados$racaCor[dados$racaCor == "BRANCA"] <- "1"
    dados$racaCor[dados$racaCor == "PARDA"] <- "2"
    dados$racaCor[dados$racaCor == "PRETA"] <- "3"
    dados$racaCor[dados$racaCor == "AMARELA"] <- "4"
    dados$racaCor[dados$racaCor == "INDIGENA"] <- "5"
    dados$racaCor[dados$racaCor == "IGNORADO"] <- "9"
    
  }


  # IDADE
  
  if("idade" %in% nomesVars){
    
    dados$idade <- as.integer(dados$idade)
    
  }
  
  
  # PROFISSIONAL DA SAUDE
  
  if("profissionalSaude" %in% nomesVars){
    
    dados$profissionalSaude <- abjutils::rm_accent(dados$profissionalSaude)
    dados$profissionalSaude <- stringr::str_remove_all(dados$profissionalSaude, "[^[:alpha:]]")
    dados$profissionalSaude <- stringr::str_to_upper(dados$profissionalSaude)
    
    dados$profissionalSaude[dados$profissionalSaude == ""] <- NA
    dados$profissionalSaude[dados$profissionalSaude == "SIM"] <- "1"
    dados$profissionalSaude[dados$profissionalSaude == "NAO"] <- "0"
    
  }
  
  
  # PROFISSIONAL DA SEGURANCA
  
  if("profissionalSeguranca" %in% nomesVars){
    
    dados$profissionalSeguranca <- abjutils::rm_accent(dados$profissionalSeguranca)
    dados$profissionalSeguranca <- stringr::str_remove_all(dados$profissionalSeguranca, "[^[:alpha:]]")
    dados$profissionalSeguranca <- stringr::str_to_upper(dados$profissionalSeguranca)
    
    dados$profissionalSeguranca[dados$profissionalSeguranca == ""] <- NA
    dados$profissionalSeguranca[dados$profissionalSeguranca == "SIM"] <- "1"
    dados$profissionalSeguranca[dados$profissionalSeguranca == "NAO"] <- "0"
    
  }
  
  
  # ESTRANGEIRO
  
  if("estrangeiro" %in% nomesVars){
    
    dados$estrangeiro <- abjutils::rm_accent(dados$estrangeiro)
    dados$estrangeiro <- stringr::str_remove_all(dados$estrangeiro, "[^[:alpha:]]")
    dados$estrangeiro <- stringr::str_to_upper(dados$estrangeiro)
    
    dados$estrangeiro[dados$estrangeiro == ""] <- NA
    dados$estrangeiro[dados$estrangeiro == "SIM"] <- "1"
    dados$estrangeiro[dados$estrangeiro == "NAO"] <- "0"
    
  }
  
  
  # ESTADO DE RESIDENCIA
  
  if("estado" %in% nomesVars){
    
    dados$estado <- as.character(dados$estado)
    
    dados$estado <- abjutils::rm_accent(dados$estado)
    dados$estado <- stringr::str_replace_all(dados$estado, "[^[:alpha:]]", " ")
    dados$estado <- stringr::str_to_upper(dados$estado)
    dados$estado <- stringr::str_trim(dados$estado, side = "both")
    dados$estado <- stringr::str_squish(dados$estado)
    
    dados$estado[dados$estado == ""] <- NA
    dados$estado[dados$estado == "ACRE"] <- "12"
    dados$estado[dados$estado == "ALAGOAS"] <- "27"
    dados$estado[dados$estado == "AMAPA"] <- "16"
    dados$estado[dados$estado == "AMAZONAS"] <- "13"
    dados$estado[dados$estado == "BAHIA"] <- "29"
    dados$estado[dados$estado == "CEARA"] <- "23"
    dados$estado[dados$estado == "DISTRITO FEDERAL"] <- "53"
    dados$estado[dados$estado == "ESPIRITO SANTO"] <- "32"
    dados$estado[dados$estado == "GOIAS"] <- "52"
    dados$estado[dados$estado == "MARANHAO"] <- "21"
    dados$estado[dados$estado == "MATO GROSSO"] <- "51"
    dados$estado[dados$estado == "MATO GROSSO DO SUL"] <- "50"
    dados$estado[dados$estado == "MINAS GERAIS"] <- "31"
    dados$estado[dados$estado == "PARA"] <- "15"
    dados$estado[dados$estado == "PARAIBA"] <- "25"
    dados$estado[dados$estado == "PARANA"] <- "41"
    dados$estado[dados$estado == "PERNAMBUCO"] <- "26"
    dados$estado[dados$estado == "PIAUI"] <- "22"
    dados$estado[dados$estado == "RIO DE JANEIRO"] <- "33"
    dados$estado[dados$estado == "RIO GRANDE DO NORTE"] <- "24"
    dados$estado[dados$estado == "RIO GRANDE DO SUL"] <- "43"
    dados$estado[dados$estado == "RONDONIA"] <- "11"
    dados$estado[dados$estado == "RORAIMA"] <- "14"
    dados$estado[dados$estado == "SANTA CATARINA"] <- "42"
    dados$estado[dados$estado == "SAO PAULO"] <- "35"
    dados$estado[dados$estado == "SERGIPE"] <- "28"
    dados$estado[dados$estado == "TOCANTINS"] <- "17"
    
  }
  
  
  # ESTADO DE NOTIFICACAO
  
  if("estadoNotificacao" %in% nomesVars){
    
    dados$estadoNotificacao <- as.character(dados$estadoNotificacao)
    
    dados$estadoNotificacao <- abjutils::rm_accent(dados$estadoNotificacao)
    dados$estadoNotificacao <- stringr::str_replace_all(dados$estadoNotificacao, "[^[:alpha:]]", " ")
    dados$estadoNotificacao <- stringr::str_to_upper(dados$estadoNotificacao)
    dados$estadoNotificacao <- stringr::str_trim(dados$estadoNotificacao, side = "both")
    dados$estadoNotificacao <- stringr::str_squish(dados$estadoNotificacao)
    
    dados$estadoNotificacao[dados$estadoNotificacao == ""] <- NA
    dados$estadoNotificacao[dados$estadoNotificacao == "ACRE"] <- "12"
    dados$estadoNotificacao[dados$estadoNotificacao == "ALAGOAS"] <- "27"
    dados$estadoNotificacao[dados$estadoNotificacao == "AMAPA"] <- "16"
    dados$estadoNotificacao[dados$estadoNotificacao == "AMAZONAS"] <- "13"
    dados$estadoNotificacao[dados$estadoNotificacao == "BAHIA"] <- "29"
    dados$estadoNotificacao[dados$estadoNotificacao == "CEARA"] <- "23"
    dados$estadoNotificacao[dados$estadoNotificacao == "DISTRITO FEDERAL"] <- "53"
    dados$estadoNotificacao[dados$estadoNotificacao == "ESPIRITO SANTO"] <- "32"
    dados$estadoNotificacao[dados$estadoNotificacao == "GOIAS"] <- "52"
    dados$estadoNotificacao[dados$estadoNotificacao == "MARANHAO"] <- "21"
    dados$estadoNotificacao[dados$estadoNotificacao == "MATO GROSSO"] <- "51"
    dados$estadoNotificacao[dados$estadoNotificacao == "MATO GROSSO DO SUL"] <- "50"
    dados$estadoNotificacao[dados$estadoNotificacao == "MINAS GERAIS"] <- "31"
    dados$estadoNotificacao[dados$estadoNotificacao == "PARA"] <- "15"
    dados$estadoNotificacao[dados$estadoNotificacao == "PARAIBA"] <- "25"
    dados$estadoNotificacao[dados$estadoNotificacao == "PARANA"] <- "41"
    dados$estadoNotificacao[dados$estadoNotificacao == "PERNAMBUCO"] <- "26"
    dados$estadoNotificacao[dados$estadoNotificacao == "PIAUI"] <- "22"
    dados$estadoNotificacao[dados$estadoNotificacao == "RIO DE JANEIRO"] <- "33"
    dados$estadoNotificacao[dados$estadoNotificacao == "RIO GRANDE DO NORTE"] <- "24"
    dados$estadoNotificacao[dados$estadoNotificacao == "RIO GRANDE DO SUL"] <- "43"
    dados$estadoNotificacao[dados$estadoNotificacao == "RONDONIA"] <- "11"
    dados$estadoNotificacao[dados$estadoNotificacao == "RORAIMA"] <- "14"
    dados$estadoNotificacao[dados$estadoNotificacao == "SANTA CATARINA"] <- "42"
    dados$estadoNotificacao[dados$estadoNotificacao == "SAO PAULO"] <- "35"
    dados$estadoNotificacao[dados$estadoNotificacao == "SERGIPE"] <- "28"
    dados$estadoNotificacao[dados$estadoNotificacao == "TOCANTINS"] <- "17"
    
  }
  
  
  # MUNICIPIO DE RESIDENCIA
  
  if("municipio" %in% nomesVars){
    
    dados$municipio <- as.character(dados$municipio)
    
    dados$municipio <- abjutils::rm_accent(dados$municipio)
    dados$municipio <- stringr::str_replace_all(dados$municipio, "[^[:alpha:]]", " ")
    dados$municipio <- stringr::str_to_upper(dados$municipio)
    dados$municipio <- stringr::str_trim(dados$municipio, side = "both")
    dados$municipio <- stringr::str_squish(dados$municipio)
    
    dados$municipio[dados$municipio == ""] <- NA
    
  }
  
  
  # MUNICIPIO DE NOTIFICACAO
  
  if("municipioNotificacao" %in% nomesVars){
    
    dados$municipioNotificacao <- as.character(dados$municipioNotificacao)
    
    dados$municipioNotificacao <- abjutils::rm_accent(dados$municipioNotificacao)
    dados$municipioNotificacao <- stringr::str_replace_all(dados$municipioNotificacao, "[^[:alpha:]]", " ")
    dados$municipioNotificacao <- stringr::str_to_upper(dados$municipioNotificacao)
    dados$municipioNotificacao <- stringr::str_trim(dados$municipioNotificacao, side = "both")
    dados$municipioNotificacao <- stringr::str_squish(dados$municipioNotificacao)
    
    dados$municipioNotificacao[dados$municipioNotificacao == ""] <- NA
    
  }
  
  
  # CBO
  
  if("cbo" %in% nomesVars){
    
    dados$cbo <- as.character(dados$cbo)
    
    dados$cbo <- abjutils::rm_accent(dados$cbo)
    dados$cbo <- stringr::str_replace_all(dados$cbo, "[^[:alnum:]]", " ")
    dados$cbo <- stringr::str_to_upper(dados$cbo)
    dados$cbo <- stringr::str_trim(dados$cbo, side = "both")
    dados$cbo <- stringr::str_squish(dados$cbo)
    
    dados$cbo[dados$cbo == ""] <- NA
    
  }
  
  
  # PAIS DE ORIGEM
  
  if("paisOrigem" %in% nomesVars){
    
    dados$paisOrigem <- as.character(dados$paisOrigem)
    
    dados$paisOrigem <- abjutils::rm_accent(dados$paisOrigem)
    dados$paisOrigem <- stringr::str_replace_all(dados$paisOrigem, "[^[:alpha:]]", " ")
    dados$paisOrigem <- stringr::str_to_upper(dados$paisOrigem)
    dados$paisOrigem <- stringr::str_trim(dados$paisOrigem, side = "both")
    dados$paisOrigem <- stringr::str_squish(dados$paisOrigem)
    
    dados$paisOrigem[dados$paisOrigem == ""] <- NA
    
  }
  
  
  # RESULTADO DO TESTE
  
  if("resultadoTeste" %in% nomesVars){
    
    dados$resultadoTeste <- as.character(dados$resultadoTeste)
    
    dados$resultadoTeste <- abjutils::rm_accent(dados$resultadoTeste)
    dados$resultadoTeste <- stringr::str_replace_all(dados$resultadoTeste, "[^[:alpha:]]", " ")
    dados$resultadoTeste <- stringr::str_to_upper(dados$resultadoTeste)
    dados$resultadoTeste <- stringr::str_trim(dados$resultadoTeste, side = "both")
    dados$resultadoTeste <- stringr::str_squish(dados$resultadoTeste)
    
    dados$resultadoTeste[dados$resultadoTeste == ""] <- NA
    dados$resultadoTeste[dados$resultadoTeste == "POSITIVO"] <- "1"
    dados$resultadoTeste[dados$resultadoTeste == "NEGATIVO"] <- "2"
    dados$resultadoTeste[dados$resultadoTeste == "INCONCLUSIVO OU INDETERMINADO"] <- "9"
    
  }
  
  
  # RESULTADO DO TESTE SOROLOGICO IGA
  
  if("resultadoTesteSorologicoIgA" %in% nomesVars){
    
    dados$resultadoTesteSorologicoIgA <- as.character(dados$resultadoTesteSorologicoIgA)
    
    dados$resultadoTesteSorologicoIgA <- abjutils::rm_accent(dados$resultadoTesteSorologicoIgA)
    dados$resultadoTesteSorologicoIgA <- stringr::str_replace_all(dados$resultadoTesteSorologicoIgA, "[^[:alpha:]]", " ")
    dados$resultadoTesteSorologicoIgA <- stringr::str_to_upper(dados$resultadoTesteSorologicoIgA)
    dados$resultadoTesteSorologicoIgA <- stringr::str_trim(dados$resultadoTesteSorologicoIgA, side = "both")
    dados$resultadoTesteSorologicoIgA <- stringr::str_squish(dados$resultadoTesteSorologicoIgA)
    
    dados$resultadoTesteSorologicoIgA[dados$resultadoTesteSorologicoIgA == ""] <- NA
    dados$resultadoTesteSorologicoIgA[dados$resultadoTesteSorologicoIgA == "REAGENTE"] <- "1"
    dados$resultadoTesteSorologicoIgA[dados$resultadoTesteSorologicoIgA == "NAO REAGENTE"] <- "2"
    dados$resultadoTesteSorologicoIgA[dados$resultadoTesteSorologicoIgA == "INCONCLUSIVO OU INDETERMINADO"] <- "9"
    
  }
  
  
  # RESULTADO DO TESTE SOROLOGICO IGG
  
  if("resultadoTesteSorologicoIgG" %in% nomesVars){
    
    dados$resultadoTesteSorologicoIgG <- as.character(dados$resultadoTesteSorologicoIgG)
    
    dados$resultadoTesteSorologicoIgG <- abjutils::rm_accent(dados$resultadoTesteSorologicoIgG)
    dados$resultadoTesteSorologicoIgG <- stringr::str_replace_all(dados$resultadoTesteSorologicoIgG, "[^[:alpha:]]", " ")
    dados$resultadoTesteSorologicoIgG <- stringr::str_to_upper(dados$resultadoTesteSorologicoIgG)
    dados$resultadoTesteSorologicoIgG <- stringr::str_trim(dados$resultadoTesteSorologicoIgG, side = "both")
    dados$resultadoTesteSorologicoIgG <- stringr::str_squish(dados$resultadoTesteSorologicoIgG)
    
    dados$resultadoTesteSorologicoIgG[dados$resultadoTesteSorologicoIgG == ""] <- NA
    dados$resultadoTesteSorologicoIgG[dados$resultadoTesteSorologicoIgG == "REAGENTE"] <- "1"
    dados$resultadoTesteSorologicoIgG[dados$resultadoTesteSorologicoIgG == "NAO REAGENTE"] <- "2"
    dados$resultadoTesteSorologicoIgG[dados$resultadoTesteSorologicoIgG == "INCONCLUSIVO OU INDETERMINADO"] <- "9"
    
  }
  
  
  # RESULTADO DO TESTE SOROLOGICO IGM
  
  if("resultadoTesteSorologicoIgM" %in% nomesVars){
    
    dados$resultadoTesteSorologicoIgM <- as.character(dados$resultadoTesteSorologicoIgM)
    
    dados$resultadoTesteSorologicoIgM <- abjutils::rm_accent(dados$resultadoTesteSorologicoIgM)
    dados$resultadoTesteSorologicoIgM <- stringr::str_replace_all(dados$resultadoTesteSorologicoIgM, "[^[:alpha:]]", " ")
    dados$resultadoTesteSorologicoIgM <- stringr::str_to_upper(dados$resultadoTesteSorologicoIgM)
    dados$resultadoTesteSorologicoIgM <- stringr::str_trim(dados$resultadoTesteSorologicoIgM, side = "both")
    dados$resultadoTesteSorologicoIgM <- stringr::str_squish(dados$resultadoTesteSorologicoIgM)
    
    dados$resultadoTesteSorologicoIgM[dados$resultadoTesteSorologicoIgM == ""] <- NA
    dados$resultadoTesteSorologicoIgM[dados$resultadoTesteSorologicoIgM == "REAGENTE"] <- "1"
    dados$resultadoTesteSorologicoIgM[dados$resultadoTesteSorologicoIgM == "NAO REAGENTE"] <- "2"
    dados$resultadoTesteSorologicoIgM[dados$resultadoTesteSorologicoIgM == "INCONCLUSIVO OU INDETERMINADO"] <- "9"
    
  }
  
  
  # RESULTADO DO TESTE SOROLOGICO TOTAIS
  
  if("resultadoTesteSorologicoTotais" %in% nomesVars){
    
    dados$resultadoTesteSorologicoTotais <- as.character(dados$resultadoTesteSorologicoTotais)
    
    dados$resultadoTesteSorologicoTotais <- abjutils::rm_accent(dados$resultadoTesteSorologicoTotais)
    dados$resultadoTesteSorologicoTotais <- stringr::str_replace_all(dados$resultadoTesteSorologicoTotais, "[^[:alpha:]]", " ")
    dados$resultadoTesteSorologicoTotais <- stringr::str_to_upper(dados$resultadoTesteSorologicoTotais)
    dados$resultadoTesteSorologicoTotais <- stringr::str_trim(dados$resultadoTesteSorologicoTotais, side = "both")
    dados$resultadoTesteSorologicoTotais <- stringr::str_squish(dados$resultadoTesteSorologicoTotais)
    
    dados$resultadoTesteSorologicoTotais[dados$resultadoTesteSorologicoTotais == ""] <- NA
    dados$resultadoTesteSorologicoTotais[dados$resultadoTesteSorologicoTotais == "REAGENTE"] <- "1"
    dados$resultadoTesteSorologicoTotais[dados$resultadoTesteSorologicoTotais == "NAO REAGENTE"] <- "2"
    dados$resultadoTesteSorologicoTotais[dados$resultadoTesteSorologicoTotais == "INCONCLUSIVO OU INDETERMINADO"] <- "9"
    
  }
  
  
  # ESTADO DO TESTE
  
  if("estadoTeste" %in% nomesVars){
    
    dados$estadoTeste <- as.character(dados$estadoTeste)
    
    dados$estadoTeste <- abjutils::rm_accent(dados$estadoTeste)
    dados$estadoTeste <- stringr::str_replace_all(dados$estadoTeste, "[^[:alpha:]]", " ")
    dados$estadoTeste <- stringr::str_to_upper(dados$estadoTeste)
    dados$estadoTeste <- stringr::str_trim(dados$estadoTeste, side = "both")
    dados$estadoTeste <- stringr::str_squish(dados$estadoTeste)
    
    dados$estadoTeste[dados$estadoTeste == "SOLICITADO"] <- "1"
    dados$estadoTeste[dados$estadoTeste == "COLETADO"] <- "2"
    dados$estadoTeste[dados$estadoTeste == "CONCLUIDO"] <- "3"
    dados$estadoTeste[dados$estadoTeste == "EXAME NAO SOLICITADO"] <- "4"
    
    dados$estadoTeste[dados$estadoTeste == ""] <- NA
    
  }
  
  
  # TIPO DE TESTE
  
  if("tipoTeste" %in% nomesVars){
    
    dados$tipoTeste <- as.character(dados$tipoTeste)
    
    dados$tipoTeste <- abjutils::rm_accent(dados$tipoTeste)
    dados$tipoTeste <- stringr::str_replace_all(dados$tipoTeste, "[^[:alpha:]]", " ")
    dados$tipoTeste <- stringr::str_to_upper(dados$tipoTeste)
    dados$tipoTeste <- stringr::str_trim(dados$tipoTeste, side = "both")
    dados$tipoTeste <- stringr::str_squish(dados$tipoTeste)
    
    dados$tipoTeste[dados$tipoTeste == "RT PCR"] <- "1"
    dados$tipoTeste[dados$tipoTeste == "TESTE RAPIDO ANTICORPO"] <- "2"
    dados$tipoTeste[dados$tipoTeste == "TESTE RAPIDO ANTIGENO"] <- "3"
    dados$tipoTeste[dados$tipoTeste == "TESTE SOROLOGICO"] <- "4"
    dados$tipoTeste[dados$tipoTeste == "ENZIMAIMUNOENSAIO ELISA IGM"] <- "4"
    dados$tipoTeste[dados$tipoTeste == "ENZIMAIMUNOENSAIO ELISA"] <- "4"
    dados$tipoTeste[dados$tipoTeste == "IMUNOENSAIO POR ELETROQUIMIOLUMINESCENCIA ECLIA IGG"] <- "4"
    dados$tipoTeste[dados$tipoTeste == "IMUNOENSAIO POR ELETROQUIMIOLUMINESCENCIA ECLIA"] <- "4"
    dados$tipoTeste[dados$tipoTeste == "QUIMIOLUMINESCENCIA CLIA"] <- "4"
    
    dados$tipoTeste[dados$tipoTeste == ""] <- NA
    
  }
  
  
  # CLASSIFICACAO FINAL
  
  if("classificacaoFinal" %in% nomesVars){
    
    dados$classificacaoFinal <- as.character(dados$classificacaoFinal)
    
    dados$classificacaoFinal <- abjutils::rm_accent(dados$classificacaoFinal)
    dados$classificacaoFinal <- stringr::str_replace_all(dados$classificacaoFinal, "[^[:alpha:]]", " ")
    dados$classificacaoFinal <- stringr::str_to_upper(dados$classificacaoFinal)
    dados$classificacaoFinal <- stringr::str_trim(dados$classificacaoFinal, side = "both")
    dados$classificacaoFinal <- stringr::str_squish(dados$classificacaoFinal)
    
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMACAO LABORATORIAL"] <- "1"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMADO LABORATORIAL"] <- "1"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMACAO CLINICO EPIDEMIOLOGICO"] <- "2"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMADO CLINICO EPIDEMIOLOGICO"] <- "2"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMADO CLINICO IMAGEM"] <- "3"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMACAO CLINICO IMAGEM"] <- "3"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMACAO CLINICO"] <- "4"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMADO CLINICO"] <- "4"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMADO POR CRITERIO CLINICO"] <- "4"
    dados$classificacaoFinal[dados$classificacaoFinal == "SINDROME GRIPAL NAO ESPECIFICADA"] <- "5"
    dados$classificacaoFinal[dados$classificacaoFinal == "DESCARTADO"] <- "6"
    
    dados$classificacaoFinal[dados$classificacaoFinal == ""] <- NA
    
  }
  
  
  # EVOLUCAO CASO
  
  if("evolucaoCaso" %in% nomesVars){
    
    dados$evolucaoCaso <- as.character(dados$evolucaoCaso)
    
    dados$evolucaoCaso <- abjutils::rm_accent(dados$evolucaoCaso)
    dados$evolucaoCaso <- stringr::str_replace_all(dados$evolucaoCaso, "[^[:alpha:]]", " ")
    dados$evolucaoCaso <- stringr::str_to_upper(dados$evolucaoCaso)
    dados$evolucaoCaso <- stringr::str_trim(dados$evolucaoCaso, side = "both")
    dados$evolucaoCaso <- stringr::str_squish(dados$evolucaoCaso)
    
    dados$evolucaoCaso[dados$evolucaoCaso == "CURA"] <- "1"
    dados$evolucaoCaso[dados$evolucaoCaso == "OBITO"] <- "2"
    dados$evolucaoCaso[dados$evolucaoCaso == "EM TRATAMENTO DOMICILIAR"] <- "3"
    dados$evolucaoCaso[dados$evolucaoCaso == "INTERNADO"] <- "4"
    dados$evolucaoCaso[dados$evolucaoCaso == "INTERNADO EM UTI"] <- "5"
    dados$evolucaoCaso[dados$evolucaoCaso == "IGNORADO"] <- "6"
    dados$evolucaoCaso[dados$evolucaoCaso == "CANCELADO"] <- "9"
    
    dados$evolucaoCaso[dados$evolucaoCaso == ""] <- NA
    
  }
  
  
  # SINTOMAS
  
  if("sintomas" %in% nomesVars){
    
    dados$sintomas <- as.character(dados$sintomas)
    
    dados$sintomas <- abjutils::rm_accent(dados$sintomas)
    dados$sintomas <- stringr::str_remove_all(dados$sintomas, "[[:digit:]]")
    dados$sintomas <- stringr::str_to_upper(dados$sintomas)
    dados$sintomas <- stringr::str_trim(dados$sintomas, side = "both")
    dados$sintomas <- stringr::str_squish(dados$sintomas)
    
    dados$sintomas[dados$sintomas == ""] <- NA
    
  }
  
  
  # CONDICOES
  
  if("condicoes" %in% nomesVars){
    
    dados$condicoes <- as.character(dados$condicoes)
    
    dados$condicoes <- abjutils::rm_accent(dados$condicoes)
    dados$condicoes <- stringr::str_remove_all(dados$condicoes, "[[:digit:]]")
    dados$condicoes <- stringr::str_to_upper(dados$condicoes)
    dados$condicoes <- stringr::str_trim(dados$condicoes, side = "both")
    dados$condicoes <- stringr::str_squish(dados$condicoes)
    
    dados$condicoes[dados$condicoes == ""] <- NA
    
  }
  
  
  ## PARTE 2
  
  
  # ESTADO DO TESTE (2)
  
  if("estadoTeste" %in% nomesVars){
    
    dados = dados %>% dplyr::mutate(
      estadoTeste = dplyr::case_when(is.na(resultadoTeste) & !is.na(dataTeste) ~ "2",
                                     !is.na(resultadoTeste) ~ "3",
                                     TRUE ~ estadoTeste))
    
  }
  
  
  # CLASSIFICACAO FINAL (2)
  
  if("classificacaoFinal" %in% nomesVars){
    
    dados = dados %>% dplyr::mutate(
      classificacaoFinal = dplyr::case_when(
        resultadoTeste == "1" ~ "1",
        resultadoTesteSorologicoIgA == "1" ~ "1",
        resultadoTesteSorologicoIgG == "1" ~ "1",
        resultadoTesteSorologicoIgM == "1" ~ "1",
        resultadoTesteSorologicoTotais == "1" ~ "1",
        
        resultadoTeste != "1" & 
          resultadoTesteSorologicoIgA != "1" & 
          resultadoTesteSorologicoIgG != "1" & 
          resultadoTesteSorologicoIgM != "1" & 
          resultadoTesteSorologicoTotais != "1" & classificacaoFinal == "1" ~ "2",
        
        TRUE ~ classificacaoFinal))
    
  }
  
  # CLASSIFICACAO FINAL (3)
  # Verificando o resultado dos exames na variável testes 
  # Caso encontre "'resultadoTeste': 'Reagente'" ou "'resultadoTeste': 'Detectável'" modifica a classificação final para "1" - "Confirmado laboratorial"
  
  dados$classificacaoFinal[dados$testes %like% "'resultadoTeste': 'Reagente'" | dados$testes %like% "'resultadoTeste': 'Detectável'"] <- "1"
  
  
  # DATA DO INICIO DOS SINTOMAS (2)
  
  #dados$dataInicioSintomas[is.na(dados$dataInicioSintomas)] <- dados$dataTeste
  #dados$dataInicioSintomas[is.na(dados$dataInicioSintomas)] <- dados$dataTesteSorologico
  #dados$dataInicioSintomas[is.na(dados$dataInicioSintomas)] <- dados$dataNotificacao
  
  dados <- dados %>% 
    mutate(dataInicioSintomas = if_else(is.na(dataInicioSintomas), dataTeste, dataInicioSintomas)) %>% 
    mutate(dataInicioSintomas = if_else(is.na(dataInicioSintomas), dataTesteSorologico, dataInicioSintomas)) %>% 
    mutate(dataInicioSintomas = if_else(is.na(dataInicioSintomas), dataNotificacao, dataInicioSintomas)) %>%
	  mutate(dataInicioSintomas = if_else(is.na(dataInicioSintomas), dataRegistro, dataInicioSintomas))
  
  
  
  ## PARTE 3
  
  
  # ANO/SEMANA EPIDEMIOLOGICA (3)
  
  dados$anoEpiSintomas <- lubridate::epiyear(dados$dataInicioSintomas)
  dados$semEpiSintomas <- lubridate::epiweek(dados$dataInicioSintomas)
  
  
  # REGIAO (3)
  
  dados$regiao <- stringr::str_sub(dados$estado, end = 1)
  

  # FAIXA-ETARIA (3)
  
  dados = dados %>% mutate(faixaEtaria = case_when(idade < 5 ~ "1", idade >= 5 & idade < 10 ~ "2", idade >= 10 & idade < 15 ~ "3", idade >= 15 & idade < 20 ~ "4", idade >= 20 & idade < 25 ~ "5", idade >= 25 & idade < 30 ~ "6", idade >= 30 & idade < 35 ~ "7", idade >= 35 & idade < 40 ~ "8", idade >= 40 & idade < 45 ~ "9", idade >= 45 & idade < 50 ~ "10", idade >= 50 & idade < 55 ~ "11", idade >= 55 & idade < 60 ~ "12", idade >= 60 & idade < 65 ~ "13", idade >= 65 & idade < 70 ~ "14", idade >= 70 & idade < 75 ~ "15", idade >= 75 & idade < 80 ~ "16", idade >= 80 & idade < 85 ~ "17", idade >= 85 & idade < 90 ~ "18", idade >= 90 & idade < 95 ~ "19", idade >= 95 & idade < 100 ~ "20", idade >= 100 ~ "21", TRUE ~ NA_character_))
  

  # FLAG TESTADOS (3)
  
  #
  
  
  # PURGE LEVELS
  
  dados <- droplevels(dados)
  
  
  # RETURN
  
  return(dados)
  
  
}