library(tidyverse)
library(readxl)
library(data.table)

# Creating the dataset by subsetting only the rows needed ------------------------------

eppggs_df <- data.frame()
all_files <- list.files(path="C:/Users/m7531338/OneDrive/Trabalho SEE/Datasets/pay_servidores", pattern = '.csv')

for (i in seq_along(all_files)) {
  
  x <- fread(all_files[i], header = FALSE, skip = 0, select = c(1:21), colClasses = "character") %>% 
          filter(V4 == "ESPEC.EM POLITICAS PUBLICAS E GESTAO GOVERNAMENTAL") %>% 
          mutate(year = all_files[i])
  
  eppggs_df <- bind_rows(eppggs_df, x)
  
  rm(x)
  
}

# Inserting the column names and coercing the data type for numerical --------

real_cols_names <- c("MASP", "NOME","SITUACAO_SERVIDOR", "CARGO_EFETIVO", "TEM_APOST", "CARGO_COMISSAO",
                     "ORGAO_ENTIDADE", "DESCUNID", "CARGA_HORARIA", "REM_BASICA_BRUTA",
                     "TETO", "JUDIC", "FERIAS","DECTER", "PREMIO", "FERIASPREM", "JETONS",
                     "EVENTUAL", "IRRF", "CONT.PREVIDENCIRIA", "REM_POS_DEDUCOES", "ANO")
colnames(empty_df) <- real_cols_names

empty_df[c(1, 9:21)] <- map_df(empty_df[c(1, 9:21)], str_replace, "\\,", "\\.")
empty_df[c(1, 9:21)] <- map_df(empty_df[c(1, 9:21)], as.numeric)
