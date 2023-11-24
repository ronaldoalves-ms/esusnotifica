# esusnotifica

O pacote R **esusnotifica** inclui a função genérica **clean_esus** que executa uma série de instruções de curadoria dos dados do e-SUS Notifica. 

O público-alvo do pacote **esusnotifica** abarca gestores e técnicos das Secretarias Estaduais e Municipais de Saúde e do Ministério da Saúde que utilizam os dados do sistema e-sus Notifica.

Espera-se que este repositório configure um ambiente colaborativo de desenvolvimento de soluções computacionais para o trabalho com dados do e-SUS Notifica.

O tratamento dos dados tais como conversão de conteúdos descritivos em códigos correspondem aos códigos apresentados no **dicionário de dados da API**

Dicionário de dados da API e informações adicionais estão disponíveis em https://datasus.saude.gov.br/notifica/.

## Como instalar?

Este pacote **em desenvolvimento** pode ser instalado no R executando os códigos abaixo:

```{r}
install.packages("devtools")
devtools::install_github("ronaldoalves-ms/esusnotifica")
```

## Como usar?

A função **clean_esus** é aplicada aos dados do e-SUS Notifica, conforme exemplo a seguir.

```r
library(esusnotifica)
dados_esus <- clean_esus(dados_esus)
```

## Dúvidas e sugestões

Crie uma [issue](https://github.com/ronaldoalves-ms/esusnotifica/issues) no projeto ou envie um e-mail para ronaldo.alves@saude.gov.br ou cid.santos@saude.gov.br
