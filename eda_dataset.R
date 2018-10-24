library(tidyverse)
library(readxl)
library(data.table)
library(purrr)

summary(eppggs_df_final)

eppggs_df_final %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
  

# SUGESTÕES DE ANÁLISE:
#   1. Verificar distribuição salarial da carreira por ano (tendência anual)
#       a. Descontar cálculo anual da inflação usando 2013 como período-base
#       b. Aumento percentual ano a ano da carreira (aumento real do salário)
#       c. Verificar distribuição do aumento ao longos dos meses do ano (cada linha = ano; eixo x = mes)
#   
#   2. Quebrar análise por sexo:
#       a. Inserir manualmente o sexo de cada pessoa
#       b. Verificar avanço número da proporção homens x mulheres
#       c. Verificar relação salarial entre os sexos (como fazer isso levando em conta a desprorporção)
#       d. Qual foi o tempo médio gasto por um csapiano para assumir um cargo em até 02 anos ()
#       
#   3. Quebrar a análise por CSAP:
#       a. Proporcionalmente, quanl CSAP tem hoje o melhor desempenho em:
#           i. Permanência na carreira (prop. de quantos entraram e quantos saíram)
#           ii. Desempenho salarial global
#           iii. Rising stars (maior quantidade de cargos ou prop de csapianos com cargos)
        
    