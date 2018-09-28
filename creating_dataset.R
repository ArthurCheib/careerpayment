library(tidyverse)
library(readxl)
library(data.table)

# Creating the dataset by subsetting only the rows needed ------------------------------

eppggs_df <- data.frame()
all_files <- list.files(pattern = '.csv')

for (i in seq_along(all_files)) {
  
  x <- fread(all_files[i], header = FALSE, skip = 0, select = c(1:21), colClasses = "character") %>% 
          filter(V4 == "ESPEC.EM POLITICAS PUBLICAS E GESTAO GOVERNAMENTAL") %>% 
          mutate(year = all_files[i])
  
  eppggs_df <- bind_rows(eppggs_df, x)
  rm(x)
}

real_cols_names <- c("MASP", "NOME","SITUACAO_SERVIDOR", "CARGO_EFETIVO", "TEM_APOST", "CARGO_COMISSAO",
                     "ORGAO_ENTIDADE", "DESCUNID", "CARGA_HORARIA", "REM_BASICA_BRUTA",
                     "TETO", "JUDIC", "FERIAS","DECTER", "PREMIO", "FERIASPREM", "JETONS",
                     "EVENTUAL", "IRRF", "CONT.PREVIDENCIRIA", "REM_POS_DEDUCOES", "ANO")

colnames(eppggs_df) <- real_cols_names

write.csv(file = eppggs_df, sep = "|", col_names = TRUE)
