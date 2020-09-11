#' Curadoria dos Dados do e-SUS Notifica 
#'
#' \code{clean_esus} é uma função genérica para o pré-processamento dos dados do e-SUS Notifica.
#'
#' @param dados Um data frame contendo os dados do e-SUS Notifica.
#'
#' @return A função retorna um data frame com dados tratados do e-SUS Notifica.
#'
#' @examples
#' esusve-uf <- clean_esus(esusve-uf)
#'
#' @export


#library(tidyverse)
#library(lubridate)
#library(abjutils)
#library(data.table)
#library(devtools)
#library(roxygen2)



clean_esus <- function(dados){
  
  
  nomesVars <- names(dados)
  
  
  # NOME COMPLETO
  
  if("nomeCompleto" %in% nomesVars){
    
    dados$nomeCompleto <- as.character(dados$nomeCompleto)
    
    dados$nomeCompleto[dados$nomeCompleto == ""] <- NA
    dados$nomeCompleto <- rm_accent(dados$nomeCompleto)
    dados$nomeCompleto <- str_replace_all(dados$nomeCompleto, "[^[:alpha:]]", " ")
    dados$nomeCompleto <- str_to_upper(dados$nomeCompleto)
    dados$nomeCompleto <- str_trim(dados$nomeCompleto, side = "both")
    dados$nomeCompleto <- str_squish(dados$nomeCompleto)
    
  }
  
  
  # NOME DA MAE
  
  if("nomeMae" %in% nomesVars){
    
    dados$nomeMae <- as.character(dados$nomeMae)
    
    dados$nomeMae[dados$nomeMae == ""] <- NA
    dados$nomeMae <- rm_accent(dados$nomeMae)
    dados$nomeMae <- str_replace_all(dados$nomeMae, "[^[:alpha:]]", " ")
    dados$nomeMae <- str_to_upper(dados$nomeMae)
    dados$nomeMae <- str_trim(dados$nomeMae, side = "both")
    dados$nomeMae <- str_squish(dados$nomeMae)
    
  }
  
  
  # NOME DO LOGRADOURO DE RESIDENCIA
  
  if("logradouro" %in% nomesVars){
    
    dados$logradouro <- as.character(dados$logradouro)
    
    dados$logradouro[dados$logradouro == ""] <- NA
    dados$logradouro <- rm_accent(dados$logradouro)
    dados$logradouro <- str_replace_all(dados$logradouro, "[^[:alpha:]]", " ")
    dados$logradouro <- str_to_upper(dados$logradouro)
    dados$logradouro <- str_trim(dados$logradouro, side = "both")
    dados$logradouro <- str_squish(dados$logradouro)
    
  }
  
  
  # DATA DE NASCIMENTO
  
  if("dataNascimento" %in% nomesVars){
    
    dados$dataNascimento <- as_date(dados$dataNascimento)
    
    dados$dataNascimento[dados$dataNascimento < as_date("1901-01-01")] <- NA
    dados$dataNascimento[dados$dataNascimento > Sys.Date()] <- NA
    
  }
  
  
  # DATA DE INICIO DOS SINTOMAS
  
  if("dataInicioSintomas" %in% nomesVars){
    
    dados$dataInicioSintomas <- as_date(dados$dataInicioSintomas)
    
    dados$dataInicioSintomas[dados$dataInicioSintomas < as_date("2020-01-01")] <- NA
    dados$dataInicioSintomas[dados$dataInicioSintomas > Sys.Date()] <- NA
    
  }
  
  
  # DATA DA NOTIFICACAO
  
  if("dataNotificacao" %in% nomesVars){
    
    dados$dataNotificacao <- as_date(dados$dataNotificacao)
    
    dados$dataNotificacao[dados$dataNotificacao < as_date("2020-01-01")] <- NA
    dados$dataNotificacao[dados$dataNotificacao > Sys.Date()] <- NA
    
  }
  
  
  # DATA DO TESTE
  
  if("dataTeste" %in% nomesVars){
    
    dados$dataTeste <- as_date(dados$dataTeste)
    
    dados$dataTeste[dados$dataTeste < as_date("2020-01-01")] <- NA
    dados$dataTeste[dados$dataTeste > Sys.Date()] <- NA
    
  }
  
  
  # DATA DE ENCERRAMENTO
  
  if("dataEncerramento" %in% nomesVars){
    
    dados$dataEncerramento <- as_date(dados$dataEncerramento)
    
    dados$dataEncerramento[dados$dataEncerramento < as_date("2020-01-01")] <- NA
    dados$dataEncerramento[dados$dataEncerramento > Sys.Date()] <- NA
    
  }
  
  
  # DATA DA CRIACAO DO REGISTRO
  
  if("_created_at" %in% nomesVars){
    
    names(dados)[names(dados) == "_created_at"] <- "dataCriacao"
    
    dados$dataCriacao <- as_date(dados$dataCriacao)
    
    dados$dataCriacao[dados$dataCriacao < as_date("2020-01-01")] <- NA
    dados$dataCriacao[dados$dataCriacao > Sys.Date()] <- NA
    
  }
  
  
  # DATA DA ATUALIZACAO DO REGISTRO
  
  if("_updated_at" %in% nomesVars){
    
    names(dados)[names(dados) == "_updated_at"] <- "dataAtualizacao"
    
    dados$dataAtualizacao <- as_date(dados$dataAtualizacao)
    
    dados$dataAtualizacao[dados$dataAtualizacao < as_date("2020-01-01")] <- NA
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
    
    dados$profissionalSaude <- rm_accent(dados$profissionalSaude)
    dados$profissionalSaude <- str_replace_all(dados$profissionalSaude, "[^[:alpha:]]", " ")
    dados$profissionalSaude <- str_to_upper(dados$profissionalSaude)
    dados$profissionalSaude <- str_trim(dados$profissionalSaude, side = "both")
    dados$profissionalSaude <- str_squish(dados$profissionalSaude)
    
    dados$profissionalSaude[dados$profissionalSaude == ""] <- NA
    
  }
  
  
  # PROFISSIONAL DA SEGURANCA
  
  if("profissionalSeguranca" %in% nomesVars){
    
    dados$profissionalSeguranca <- rm_accent(dados$profissionalSeguranca)
    dados$profissionalSeguranca <- str_replace_all(dados$profissionalSeguranca, "[^[:alpha:]]", " ")
    dados$profissionalSeguranca <- str_to_upper(dados$profissionalSeguranca)
    dados$profissionalSeguranca <- str_trim(dados$profissionalSeguranca, side = "both")
    dados$profissionalSeguranca <- str_squish(dados$profissionalSeguranca)
    
    dados$profissionalSeguranca[dados$profissionalSeguranca == ""] <- NA
    
  }
  
  
  # ESTRANGEIRO
  
  if("estrangeiro" %in% nomesVars){
    
    dados$estrangeiro <- rm_accent(dados$estrangeiro)
    dados$estrangeiro <- str_replace_all(dados$estrangeiro, "[^[:alpha:]]", " ")
    dados$estrangeiro <- str_to_upper(dados$estrangeiro)
    dados$estrangeiro <- str_trim(dados$estrangeiro, side = "both")
    dados$estrangeiro <- str_squish(dados$estrangeiro)
    
    dados$estrangeiro[dados$estrangeiro == ""] <- NA
    
  }
  
  
  # ESTADO DE RESIDENCIA
  
  if("estado" %in% nomesVars){
    
    dados$estado <- as.character(dados$estado)
    
    dados$estado[dados$estado == ""] <- NA
    dados$estado[dados$estado == "ACRE"] <- "AC"
    dados$estado[dados$estado == "ALAGOAS"] <- "AL"
    dados$estado[dados$estado == "AMAP?"] <- "AP"
    dados$estado[dados$estado == "AMAZONAS"] <- "AM"
    dados$estado[dados$estado == "BAHIA"] <- "BA"
    dados$estado[dados$estado == "CEAR?"] <- "CE"
    dados$estado[dados$estado == "DISTRITO FEDERAL"] <- "DF"
    dados$estado[dados$estado == "ESP?RITO SANTO"] <- "ES"
    dados$estado[dados$estado == "GOI?S"] <- "GO"
    dados$estado[dados$estado == "MARANH?O"] <- "MA"
    dados$estado[dados$estado == "MATO GROSSO"] <- "MT"
    dados$estado[dados$estado == "MATO GROSSO DO SUL"] <- "MS"
    dados$estado[dados$estado == "MINAS GERAIS"] <- "MG"
    dados$estado[dados$estado == "PAR?"] <- "PA"
    dados$estado[dados$estado == "PARA?BA"] <- "PB"
    dados$estado[dados$estado == "PARAN?"] <- "PR"
    dados$estado[dados$estado == "PERNAMBUCO"] <- "PE"
    dados$estado[dados$estado == "PIAU?"] <- "PI"
    dados$estado[dados$estado == "RIO DE JANEIRO"] <- "RJ"
    dados$estado[dados$estado == "RIO GRANDE DO NORTE"] <- "RN"
    dados$estado[dados$estado == "RIO GRANDE DO SUL"] <- "RS"
    dados$estado[dados$estado == "ROND?NIA"] <- "RO"
    dados$estado[dados$estado == "RORAIMA"] <- "RR"
    dados$estado[dados$estado == "SANTA CATARINA"] <- "SC"
    dados$estado[dados$estado == "S?O PAULO"] <- "SP"
    dados$estado[dados$estado == "SERGIPE"] <- "SE"
    dados$estado[dados$estado == "TOCANTINS"] <- "TO"
  }
  
  
  # ESTADO DE NOTIFICACAO
  
  if("estadoNotificacao" %in% nomesVars){
    
    dados$estadoNotificacao <- as.character(dados$estadoNotificacao)
    
    dados$estadoNotificacao[dados$estadoNotificacao == ""] <- NA
    dados$estadoNotificacao[dados$estadoNotificacao == "ACRE"] <- "AC"
    dados$estadoNotificacao[dados$estadoNotificacao == "ALAGOAS"] <- "AL"
    dados$estadoNotificacao[dados$estadoNotificacao == "AMAP?"] <- "AP"
    dados$estadoNotificacao[dados$estadoNotificacao == "AMAZONAS"] <- "AM"
    dados$estadoNotificacao[dados$estadoNotificacao == "BAHIA"] <- "BA"
    dados$estadoNotificacao[dados$estadoNotificacao == "CEAR?"] <- "CE"
    dados$estadoNotificacao[dados$estadoNotificacao == "DISTRITO FEDERAL"] <- "DF"
    dados$estadoNotificacao[dados$estadoNotificacao == "ESP?RITO SANTO"] <- "ES"
    dados$estadoNotificacao[dados$estadoNotificacao == "GOI?S"] <- "GO"
    dados$estadoNotificacao[dados$estadoNotificacao == "MARANH?O"] <- "MA"
    dados$estadoNotificacao[dados$estadoNotificacao == "MATO GROSSO"] <- "MT"
    dados$estadoNotificacao[dados$estadoNotificacao == "MATO GROSSO DO SUL"] <- "MS"
    dados$estadoNotificacao[dados$estadoNotificacao == "MINAS GERAIS"] <- "MG"
    dados$estadoNotificacao[dados$estadoNotificacao == "PAR?"] <- "PA"
    dados$estadoNotificacao[dados$estadoNotificacao == "PARA?BA"] <- "PB"
    dados$estadoNotificacao[dados$estadoNotificacao == "PARAN?"] <- "PR"
    dados$estadoNotificacao[dados$estadoNotificacao == "PERNAMBUCO"] <- "PE"
    dados$estadoNotificacao[dados$estadoNotificacao == "PIAU?"] <- "PI"
    dados$estadoNotificacao[dados$estadoNotificacao == "RIO DE JANEIRO"] <- "RJ"
    dados$estadoNotificacao[dados$estadoNotificacao == "RIO GRANDE DO NORTE"] <- "RN"
    dados$estadoNotificacao[dados$estadoNotificacao == "RIO GRANDE DO SUL"] <- "RS"
    dados$estadoNotificacao[dados$estadoNotificacao == "ROND?NIA"] <- "RO"
    dados$estadoNotificacao[dados$estadoNotificacao == "RORAIMA"] <- "RR"
    dados$estadoNotificacao[dados$estadoNotificacao == "SANTA CATARINA"] <- "SC"
    dados$estadoNotificacao[dados$estadoNotificacao == "S?O PAULO"] <- "SP"
    dados$estadoNotificacao[dados$estadoNotificacao == "SERGIPE"] <- "SE"
    dados$estadoNotificacao[dados$estadoNotificacao == "TOCANTINS"] <- "TO"
    
  }
  
  
  # RESULTADO DO TESTE
  
  if("resultadoTeste" %in% nomesVars){
    
    dados$resultadoTeste <- as.character(dados$resultadoTeste)
    
    dados$resultadoTeste <- rm_accent(dados$resultadoTeste)
    dados$resultadoTeste <- str_replace_all(dados$resultadoTeste, "[^[:alpha:]]", " ")
    dados$resultadoTeste <- str_to_upper(dados$resultadoTeste)
    dados$resultadoTeste <- str_trim(dados$resultadoTeste, side = "both")
    dados$resultadoTeste <- str_squish(dados$resultadoTeste)
    
    dados$resultadoTeste[dados$resultadoTeste == ""] <- NA
    
  }
  
  
  # ESTADO DO TESTE
  
  if("estadoTeste" %in% nomesVars){
    
    dados$estadoTeste <- as.character(dados$estadoTeste)
    
    dados$estadoTeste <- rm_accent(dados$estadoTeste)
    dados$estadoTeste <- str_replace_all(dados$estadoTeste, "[^[:alpha:]]", " ")
    dados$estadoTeste <- str_to_upper(dados$estadoTeste)
    dados$estadoTeste <- str_trim(dados$estadoTeste, side = "both")
    dados$estadoTeste <- str_squish(dados$estadoTeste)
    
    dados$estadoTeste[dados$estadoTeste == "SOLICITADO"] <- "1-Solicitado"
    dados$estadoTeste[dados$estadoTeste == "CONCLUIDO"] <- "2-Concluido"
    dados$estadoTeste[dados$estadoTeste == "COLETADO"] <- "3-Coletado"
    dados$estadoTeste[dados$estadoTeste == "EXAME NAO SOLICITADO"] <- "4-Nao Solicitado"
    
    dados$estadoTeste[dados$estadoTeste == ""] <- NA
    
  }
  
  
  # TIPO DE TESTE
  
  if("tipoTeste" %in% nomesVars){
    
    dados$tipoTeste <- as.character(dados$tipoTeste)
    
    dados$tipoTeste <- rm_accent(dados$tipoTeste)
    dados$tipoTeste <- str_replace_all(dados$tipoTeste, "[^[:alpha:]]", " ")
    dados$tipoTeste <- str_to_upper(dados$tipoTeste)
    dados$tipoTeste <- str_trim(dados$tipoTeste, side = "both")
    dados$tipoTeste <- str_squish(dados$tipoTeste)
    
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
    
    dados$classificacaoFinal <- rm_accent(dados$classificacaoFinal)
    dados$classificacaoFinal <- str_replace_all(dados$classificacaoFinal, "[^[:alpha:]]", " ")
    dados$classificacaoFinal <- str_to_upper(dados$classificacaoFinal)
    dados$classificacaoFinal <- str_trim(dados$classificacaoFinal, side = "both")
    dados$classificacaoFinal <- str_squish(dados$classificacaoFinal)
    
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
    
    dados$evolucaoCaso <- rm_accent(dados$evolucaoCaso)
    dados$evolucaoCaso <- str_replace_all(dados$evolucaoCaso, "[^[:alpha:]]", " ")
    dados$evolucaoCaso <- str_to_upper(dados$evolucaoCaso)
    dados$evolucaoCaso <- str_trim(dados$evolucaoCaso, side = "both")
    dados$evolucaoCaso <- str_squish(dados$evolucaoCaso)
    
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
    
    dados = dados %>% mutate(
      resultadoTeste = case_when(classificacaoFinal == "2-Confirmado Laboratorial" ~ "POSITIVO",
                                 TRUE ~ resultadoTeste))
    
  }
  
  
  # ESTADO DO TESTE (2)
  
  if("estadoTeste" %in% nomesVars){
    
    dados = dados %>% mutate(
      estadoTeste = case_when(!is.na(resultadoTeste) ~ "2-Concluido",
                              is.na(resultadoTeste) & !is.na(dataTeste) ~ "3-Coletado",
                              TRUE ~ estadoTeste))
    
  }
  
  
  # CLASSIFICACAO FINAL (2)
  
  if("classificacaoFinal" %in% nomesVars){
    
    dados = dados %>% mutate(
      classificacaoFinal = case_when(
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
