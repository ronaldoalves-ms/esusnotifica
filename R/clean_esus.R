#' Curadoria dos Dados do e-SUS Notifica 
#'
#' \code{clean_esus} Ã© uma funÃ§Ã£o genÃ©rica para o prÃ©-processamento dos dados do e-SUS Notifica.
#'
#' @param dados Um data frame contendo os dados do e-SUS Notifica.
#'
#' @return A funÃ§Ã£o retorna um data frame com dados tratados do e-SUS Notifica.
#'
#' @examples
#' esusve-uf <- clean_esus(esusve-uf)
#'
#' @export



# PACOTES INSTALADOS PARALELAMENTE

usethis::use_package("dplyr", type = "Imports")
usethis::use_package("lubridate", type = "Imports")
usethis::use_package("stringr", type = "Imports")
usethis::use_package("abjutils", type = "Imports")


usethis::use_pipe(export = TRUE)



clean_esus <- function(dados){
  
  
  nomesVars <- names(dados)
  
  
  # NOME COMPLETO
  
  if("nomeCompleto" %in% nomesVars){
    
    dados$nomeCompleto <- as.character(dados$nomeCompleto)
    
    dados$nomeCompleto[dados$nomeCompleto == ""] <- NA
    dados$nomeCompleto <- abjutils::rm_accent(dados$nomeCompleto)
    dados$nomeCompleto <- stringr::str_replace_all(dados$nomeCompleto, "[^[:alpha:]]", " ")
    dados$nomeCompleto <- stringr::str_to_upper(dados$nomeCompleto)
    dados$nomeCompleto <- stringr::str_trim(dados$nomeCompleto, side = "both")
    dados$nomeCompleto <- stringr::str_squish(dados$nomeCompleto)
    
  }
  
  
  # NOME DA MAE
  
  if("nomeMae" %in% nomesVars){
    
    dados$nomeMae <- as.character(dados$nomeMae)
    
    dados$nomeMae[dados$nomeMae == ""] <- NA
    dados$nomeMae <- abjutils::rm_accent(dados$nomeMae)
    dados$nomeMae <- stringr::str_replace_all(dados$nomeMae, "[^[:alpha:]]", " ")
    dados$nomeMae <- stringr::str_to_upper(dados$nomeMae)
    dados$nomeMae <- stringr::str_trim(dados$nomeMae, side = "both")
    dados$nomeMae <- stringr::str_squish(dados$nomeMae)
    
  }
  
  
  # NOME DO LOGRADOURO DE RESIDENCIA
  
  if("logradouro" %in% nomesVars){
    
    dados$logradouro <- as.character(dados$logradouro)
    
    dados$logradouro[dados$logradouro == ""] <- NA
    dados$logradouro <- abjutils::rm_accent(dados$logradouro)
    dados$logradouro <- stringr::str_replace_all(dados$logradouro, "[^[:alpha:]]", " ")
    dados$logradouro <- stringr::str_to_upper(dados$logradouro)
    dados$logradouro <- stringr::str_trim(dados$logradouro, side = "both")
    dados$logradouro <- stringr::str_squish(dados$logradouro)
    
  }
  
  
  # DATA DE NASCIMENTO
  
  if("dataNascimento" %in% nomesVars){
    
    dados$dataNascimento <- lubridate::as_date(dados$dataNascimento)
    
    dados$dataNascimento[dados$dataNascimento < lubridate::as_date("1901-01-01")] <- NA
    dados$dataNascimento[dados$dataNascimento > Sys.Date()] <- NA
    
  }
  
  
  # DATA DE INICIO DOS SINTOMAS
  
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
  
  
  # DATA DE ENCERRAMENTO
  
  if("dataEncerramento" %in% nomesVars){
    
    dados$dataEncerramento <- lubridate::as_date(dados$dataEncerramento)
    
    dados$dataEncerramento[dados$dataEncerramento < lubridate::as_date("2020-01-01")] <- NA
    dados$dataEncerramento[dados$dataEncerramento > Sys.Date()] <- NA
    
  }
  
  
  # DATA DA CRIACAO DO REGISTRO
  
  if("_created_at" %in% nomesVars){
    
    names(dados)[names(dados) == "_created_at"] <- "dataCriacao"
    
    dados$dataCriacao <- lubridate::as_date(dados$dataCriacao)
    
    dados$dataCriacao[dados$dataCriacao < lubridate::as_date("2020-01-01")] <- NA
    dados$dataCriacao[dados$dataCriacao > Sys.Date()] <- NA
    
  }
  
  
  # DATA DA ATUALIZACAO DO REGISTRO
  
  if("_updated_at" %in% nomesVars){
    
    names(dados)[names(dados) == "_updated_at"] <- "dataAtualizacao"
    
    dados$dataAtualizacao <- lubridate::as_date(dados$dataAtualizacao)
    
    dados$dataAtualizacao[dados$dataAtualizacao < lubridate::as_date("2020-01-01")] <- NA
    dados$dataAtualizacao[dados$dataAtualizacao > Sys.Date()] <- NA
    
  }
  
  
  # SEXO
  
  if("sexo" %in% nomesVars){
    
    dados$sexo[dados$sexo == "Indefinido"] <- NA
    dados$sexo[dados$sexo == ""] <- NA
    
  }
  
  
  # RACA/COR
  
  if("racaCor" %in% nomesVars){
    
    dados$racaCor[dados$racaCor == ""] <- NA
    
  }
  
  
  # IDADE
  
  if("idade" %in% nomesVars){
    
    dados$idade <- as.integer(dados$idade)
    dados$idade[dados$idade == 0] <- NA
    
  }
  
  
  # PROFISSIONAL DA SAUDE
  
  if("profissionalSaude" %in% nomesVars){
    
    dados$profissionalSaude <- abjutils::rm_accent(dados$profissionalSaude)
    dados$profissionalSaude <- stringr::str_replace_all(dados$profissionalSaude, "[^[:alpha:]]", " ")
    dados$profissionalSaude <- stringr::str_to_upper(dados$profissionalSaude)
    dados$profissionalSaude <- stringr::str_trim(dados$profissionalSaude, side = "both")
    dados$profissionalSaude <- stringr::str_squish(dados$profissionalSaude)
    
    dados$profissionalSaude[dados$profissionalSaude == ""] <- NA
    
  }
  
  
  # PROFISSIONAL DA SEGURANCA
  
  if("profissionalSeguranca" %in% nomesVars){
    
    dados$profissionalSeguranca <- abjutils::rm_accent(dados$profissionalSeguranca)
    dados$profissionalSeguranca <- stringr::str_replace_all(dados$profissionalSeguranca, "[^[:alpha:]]", " ")
    dados$profissionalSeguranca <- stringr::str_to_upper(dados$profissionalSeguranca)
    dados$profissionalSeguranca <- stringr::str_trim(dados$profissionalSeguranca, side = "both")
    dados$profissionalSeguranca <- stringr::str_squish(dados$profissionalSeguranca)
    
    dados$profissionalSeguranca[dados$profissionalSeguranca == ""] <- NA
    
  }
  
  
  # ESTRANGEIRO
  
  if("estrangeiro" %in% nomesVars){
    
    dados$estrangeiro <- abjutils::rm_accent(dados$estrangeiro)
    dados$estrangeiro <- stringr::str_replace_all(dados$estrangeiro, "[^[:alpha:]]", " ")
    dados$estrangeiro <- stringr::str_to_upper(dados$estrangeiro)
    dados$estrangeiro <- stringr::str_trim(dados$estrangeiro, side = "both")
    dados$estrangeiro <- stringr::str_squish(dados$estrangeiro)
    
    dados$estrangeiro[dados$estrangeiro == ""] <- NA
    
  }
  
  
  # ESTADO DE RESIDENCIA
  
  if("estado" %in% nomesVars){
    
    dados$estado <- as.character(dados$estado)
    
    dados$estado[dados$estado == ""] <- NA
    dados$estado[dados$estado == "ACRE"] <- "AC"
    dados$estado[dados$estado == "ALAGOAS"] <- "AL"
    dados$estado[dados$estado == "AMAPÁ"] <- "AP"
    dados$estado[dados$estado == "AMAZONAS"] <- "AM"
    dados$estado[dados$estado == "BAHIA"] <- "BA"
    dados$estado[dados$estado == "CEARÁ"] <- "CE"
    dados$estado[dados$estado == "DISTRITO FEDERAL"] <- "DF"
    dados$estado[dados$estado == "ESPÍRITO SANTO"] <- "ES"
    dados$estado[dados$estado == "GOIÁS"] <- "GO"
    dados$estado[dados$estado == "MARANHÃO"] <- "MA"
    dados$estado[dados$estado == "MATO GROSSO"] <- "MT"
    dados$estado[dados$estado == "MATO GROSSO DO SUL"] <- "MS"
    dados$estado[dados$estado == "MINAS GERAIS"] <- "MG"
    dados$estado[dados$estado == "PARÁ"] <- "PA"
    dados$estado[dados$estado == "PARAÍBA"] <- "PB"
    dados$estado[dados$estado == "PARANÁ"] <- "PR"
    dados$estado[dados$estado == "PERNAMBUCO"] <- "PE"
    dados$estado[dados$estado == "PIAUÍ"] <- "PI"
    dados$estado[dados$estado == "RIO DE JANEIRO"] <- "RJ"
    dados$estado[dados$estado == "RIO GRANDE DO NORTE"] <- "RN"
    dados$estado[dados$estado == "RIO GRANDE DO SUL"] <- "RS"
    dados$estado[dados$estado == "RONDÔNIA"] <- "RO"
    dados$estado[dados$estado == "RORAIMA"] <- "RR"
    dados$estado[dados$estado == "SANTA CATARINA"] <- "SC"
    dados$estado[dados$estado == "SÃO PAULO"] <- "SP"
    dados$estado[dados$estado == "SERGIPE"] <- "SE"
    dados$estado[dados$estado == "TOCANTINS"] <- "TO"
  }
  
  
  # ESTADO DE NOTIFICACAO
  
  if("estadoNotificacao" %in% nomesVars){
    
    dados$estadoNotificacao <- as.character(dados$estadoNotificacao)
    
    dados$estadoNotificacao[dados$estadoNotificacao == ""] <- NA
    dados$estadoNotificacao[dados$estadoNotificacao == "ACRE"] <- "AC"
    dados$estadoNotificacao[dados$estadoNotificacao == "ALAGOAS"] <- "AL"
    dados$estadoNotificacao[dados$estadoNotificacao == "AMAPÁ"] <- "AP"
    dados$estadoNotificacao[dados$estadoNotificacao == "AMAZONAS"] <- "AM"
    dados$estadoNotificacao[dados$estadoNotificacao == "BAHIA"] <- "BA"
    dados$estadoNotificacao[dados$estadoNotificacao == "CEARÁ"] <- "CE"
    dados$estadoNotificacao[dados$estadoNotificacao == "DISTRITO FEDERAL"] <- "DF"
    dados$estadoNotificacao[dados$estadoNotificacao == "ESPÍRITO SANTO"] <- "ES"
    dados$estadoNotificacao[dados$estadoNotificacao == "GOIÁS"] <- "GO"
    dados$estadoNotificacao[dados$estadoNotificacao == "MARANHÃO"] <- "MA"
    dados$estadoNotificacao[dados$estadoNotificacao == "MATO GROSSO"] <- "MT"
    dados$estadoNotificacao[dados$estadoNotificacao == "MATO GROSSO DO SUL"] <- "MS"
    dados$estadoNotificacao[dados$estadoNotificacao == "MINAS GERAIS"] <- "MG"
    dados$estadoNotificacao[dados$estadoNotificacao == "PARÁ"] <- "PA"
    dados$estadoNotificacao[dados$estadoNotificacao == "PARAÍBA"] <- "PB"
    dados$estadoNotificacao[dados$estadoNotificacao == "PARANÁ"] <- "PR"
    dados$estadoNotificacao[dados$estadoNotificacao == "PERNAMBUCO"] <- "PE"
    dados$estadoNotificacao[dados$estadoNotificacao == "PIAUÍ"] <- "PI"
    dados$estadoNotificacao[dados$estadoNotificacao == "RIO DE JANEIRO"] <- "RJ"
    dados$estadoNotificacao[dados$estadoNotificacao == "RIO GRANDE DO NORTE"] <- "RN"
    dados$estadoNotificacao[dados$estadoNotificacao == "RIO GRANDE DO SUL"] <- "RS"
    dados$estadoNotificacao[dados$estadoNotificacao == "RONDÔNIA"] <- "RO"
    dados$estadoNotificacao[dados$estadoNotificacao == "RORAIMA"] <- "RR"
    dados$estadoNotificacao[dados$estadoNotificacao == "SANTA CATARINA"] <- "SC"
    dados$estadoNotificacao[dados$estadoNotificacao == "SÃO PAULO"] <- "SP"
    dados$estadoNotificacao[dados$estadoNotificacao == "SERGIPE"] <- "SE"
    dados$estadoNotificacao[dados$estadoNotificacao == "TOCANTINS"] <- "TO"
    
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
    
  }
  
  
  # ESTADO DO TESTE
  
  if("estadoTeste" %in% nomesVars){
    
    dados$estadoTeste <- as.character(dados$estadoTeste)
    
    dados$estadoTeste <- abjutils::rm_accent(dados$estadoTeste)
    dados$estadoTeste <- stringr::str_replace_all(dados$estadoTeste, "[^[:alpha:]]", " ")
    dados$estadoTeste <- stringr::str_to_upper(dados$estadoTeste)
    dados$estadoTeste <- stringr::str_trim(dados$estadoTeste, side = "both")
    dados$estadoTeste <- stringr::str_squish(dados$estadoTeste)
    
    dados$estadoTeste[dados$estadoTeste == "SOLICITADO"] <- "1-Solicitado"
    dados$estadoTeste[dados$estadoTeste == "CONCLUIDO"] <- "2-Concluido"
    dados$estadoTeste[dados$estadoTeste == "COLETADO"] <- "3-Coletado"
    dados$estadoTeste[dados$estadoTeste == "EXAME NAO SOLICITADO"] <- "4-Nao Solicitado"
    
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
    
    dados$tipoTeste[dados$tipoTeste == "RT PCR"] <- "1-RT_PCR"
    dados$tipoTeste[dados$tipoTeste == "TESTE RAPIDO ANTICORPO"] <- "2-TR_Anticorpo"
    dados$tipoTeste[dados$tipoTeste == "TESTE RAPIDO ANTIGENO"] <- "3-TR_Antigeno"
    dados$tipoTeste[dados$tipoTeste == "ENZIMAIMUNOENSAIO ELISA IGM"] <- "4-ELISA"
    dados$tipoTeste[dados$tipoTeste == "ENZIMAIMUNOENSAIO ELISA"] <- "4-ELISA"
    dados$tipoTeste[dados$tipoTeste == "IMUNOENSAIO POR ELETROQUIMIOLUMINESCENCIA ECLIA IGG"] <- "5-ECLISA"
    dados$tipoTeste[dados$tipoTeste == "IMUNOENSAIO POR ELETROQUIMIOLUMINESCENCIA ECLIA"] <- "5-ECLISA"
    dados$tipoTeste[dados$tipoTeste == "QUIMIOLUMINESCENCIA CLIA"] <- "6-CLIA"
    
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
    
    dados$classificacaoFinal[dados$classificacaoFinal == ""] <- NA
    
    dados$classificacaoFinal[dados$classificacaoFinal == "DESCARTADO"] <- "1-Descartado"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMACAO LABORATORIAL"] <- "2-Confirmado Laboratorial"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMADO LABORATORIAL"] <- "2-Confirmado Laboratorial"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMACAO CLINICO EPIDEMIOLOGICO"] <- "3-Confirmado Clinico-Epidemiologico"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMADO CLINICO EPIDEMIOLOGICO"] <- "3-Confirmado Clinico-Epidemiologico"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMADO CLINICO IMAGEM"] <- "4-Confirmado Clinico-Imagem"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMACAO CLINICO IMAGEM"] <- "4-Confirmado Clinico-Imagem"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMACAO CLINICO"] <- "5-Confirmado Clinico"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMADO CLINICO"] <- "5-Confirmado Clinico"
    dados$classificacaoFinal[dados$classificacaoFinal == "CONFIRMADO POR CRITERIO CLINICO"] <- "5-Confirmado Clinico"
    dados$classificacaoFinal[dados$classificacaoFinal == "SINDROME GRIPAL NAO ESPECIFICADA"] <- "6-SG Nao Especificada"
    
  }
  
  
  # EVOLUCAO CASO
  
  if("evolucaoCaso" %in% nomesVars){
    
    dados$evolucaoCaso <- as.character(dados$evolucaoCaso)
    
    dados$evolucaoCaso <- abjutils::rm_accent(dados$evolucaoCaso)
    dados$evolucaoCaso <- stringr::str_replace_all(dados$evolucaoCaso, "[^[:alpha:]]", " ")
    dados$evolucaoCaso <- stringr::str_to_upper(dados$evolucaoCaso)
    dados$evolucaoCaso <- stringr::str_trim(dados$evolucaoCaso, side = "both")
    dados$evolucaoCaso <- stringr::str_squish(dados$evolucaoCaso)
    
    dados$evolucaoCaso[dados$evolucaoCaso == "CURA"] <- "1-Cura"
    dados$evolucaoCaso[dados$evolucaoCaso == "OBITO"] <- "2-Obito"
    dados$evolucaoCaso[dados$evolucaoCaso == "EM TRATAMENTO DOMICILIAR"] <- "3-Em tratamento domiciliar"
    dados$evolucaoCaso[dados$evolucaoCaso == "INTERNADO"] <- "4-Internado"
    dados$evolucaoCaso[dados$evolucaoCaso == "INTERNADO EM UTI"] <- "5-Internado em UTI"
    dados$evolucaoCaso[dados$evolucaoCaso == "IGNORADO"] <- "6-Ignorado"
    dados$evolucaoCaso[dados$evolucaoCaso == "CANCELADO"] <- "7-Cancelado"
    
    dados$evolucaoCaso[dados$evolucaoCaso == ""] <- NA
    
  }
  
  
  ## PARTE 2
  
  
  # RESULTADO DO TESTE (2)
  
  if("resultadoTeste" %in% nomesVars){
    
    dados = dados %>% dplyr::mutate(
      resultadoTeste = dplyr::case_when(classificacaoFinal == "2-Confirmado Laboratorial" ~ "POSITIVO",
                                 TRUE ~ resultadoTeste))
    
  }
  
  
  # ESTADO DO TESTE (2)
  
  if("estadoTeste" %in% nomesVars){
    
    dados = dados %>% dplyr::mutate(
      estadoTeste = dplyr::case_when(!is.na(resultadoTeste) ~ "2-Concluido",
                              is.na(resultadoTeste) & !is.na(dataTeste) ~ "3-Coletado",
                              TRUE ~ estadoTeste))
    
  }
  
  
  # CLASSIFICACAO FINAL (2)
  
  if("classificacaoFinal" %in% nomesVars){
    
    dados = dados %>% dplyr::mutate(
      classificacaoFinal = dplyr::case_when(
        resultadoTeste == "POSITIVO" ~ "2-Confirmado Laboratorial",
        resultadoTeste != "POSITIVO" & classificacaoFinal == "2-Confirmado Laboratorial" ~ "3-Confirmado Clinico-Epidemiologico",
        resultadoTeste == "NEGATIVO" & is.na(classificacaoFinal) ~ "1-Descartado",
        TRUE ~ classificacaoFinal))
    
  }
  
  
  # PURGE LEVELS
  
  dados <- droplevels(dados)
  
  
  # RETURN
  
  return(dados)
  
  
}
