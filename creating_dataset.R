library(tidyverse)
library(readxl)
library(data.table)

# FIRST TRY ---------------------------------------------------------------


real_cols_names <- c("MASP", "NOME","SITUACAO_SERVIDOR", "CARGO_EFETIVO", "TEM_APOST", "CARGO_COMISSAO",
                "ORGAO_ENTIDADE", "DESCUNID", "CARGA_HORARIA", "REM_BASICA_BRUTA",
                "TETO", "JUDIC", "FERIAS","DECTER", "PREMIO", "FERIASPREM", "JETONS",
                 "EVENTUAL", "IRRF", "CONT.PREVIDENCIRIA", "REM_POS_DEDUCOES", "ANO")

empty_df <- data.frame()
all_files <- list.files(path="C:/Users/m7531338/OneDrive/Trabalho SEE/Datasets/pay_servidores", pattern = '.csv')


for (i in seq_along(all_files)) {
  
  x <- fread(all_files[i], header = FALSE, skip = 0, select = c(1:21), colClasses = "character") %>% 
          filter(V4 == "ESPEC.EM POLITICAS PUBLICAS E GESTAO GOVERNAMENTAL") %>% 
          mutate(year = all_files[i])
  
  empty_df <- bind_rows(empty_df, x)
  
  rm(x)
  
}

colnames(empty_df) <- real_cols_names




ggplot(dados_bia, aes(x = year, y = V10)) +
  geom_point()
  

dados_bia <- empty_df %>% 
filter(V2 == "BEATRICE CORREA DE OLIVEIRA")

