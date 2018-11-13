library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)

# CREATING THE MAIN DATASET ------------------------------

setwd("C:/Users/arthu/OneDrive/Trabalho SEE/Datasets/pay_servidores")
eppggs_df <- data.frame()
#2setwd("C:\\Users\\m7531338\\OneDrive\\Trabalho SEE\\Datasets\\pay_servidores")
all_files <- list.files(path = getwd(), pattern = '.csv')
all_files <- all_files[-c(61,62)]

for (i in seq_along(all_files)) {
  
  x <- fread(all_files[i], header = FALSE, skip = 0, select = c(1:21), colClasses = "character") %>% 
          filter(V4 == "ESPEC.EM POLITICAS PUBLICAS E GESTAO GOVERNAMENTAL") %>% 
          mutate(year = all_files[i])
  
  eppggs_df <- bind_rows(eppggs_df, x)
  rm(x)
}

real_cols_names <- c("CODIGO", "NOME","SITUACAO_SERVIDOR", "CARGO_EFETIVO", "TEM_APOST", "CARGO_COMISSAO",
                     "ORGAO_ENTIDADE", "DESCUNID", "CARGA_HORARIA", "REM_BASICA_BRUTA",
                     "TETO", "JUDIC", "FERIAS","DECTER", "PREMIO", "FERIASPREM", "JETONS",
                     "EVENTUAL", "IRRF", "CONT.PREVIDENCIRIA", "REM_POS_DEDUCOES", "ANO")

colnames(eppggs_df) <- real_cols_names

eppggs_df[c(1, 9:21)] <- map_df(eppggs_df[c(1, 9:21)], str_replace, "\\,", "\\.")
eppggs_df[c(1, 9:21)] <- map_df(eppggs_df[c(1, 9:21)], as.numeric)

# Removing errors within the name columns (when loading the data it arose)
temp_01 <- which(str_detect(eppggs_df$NOME, "\\d"))
eppggs_df <- eppggs_df[-temp_01, ]

#Removing "acentos" for preparing the data to be joined (let it equal to the lookup_df)
eppggs_df$NOME <- map_chr(eppggs_df$NOME, iconv, from="UTF-8",to="ASCII//TRANSLIT")


# LAST WRANGLING FOR THE FINAL ARRANGE

# Separating last column into three and removing two other columns (this line should have come earlier). --
eppggs_df_wrangled <- eppggs_df %>% 
  separate(ANO, c("ANO", "MES"), "_") %>% 
  separate(MES, c("MES", "DIA"), "\\.") %>%
  select(-c(JUDIC, TEM_APOST))

eppggs_df_wrangled$DIA <- 5

# Coercing cols into rigth data types and creating new column date format:
eppggs_df_wrangled$ANO <- as.numeric(eppggs_df_wrangled$ANO)
eppggs_df_wrangled$MES <- as.numeric(eppggs_df_wrangled$MES)

eppggs_df_wrangled <- eppggs_df_wrangled %>% 
  unite(DATA_SALARIO, c("ANO", "MES", "DIA"), sep = "-")

eppggs_df_wrangled$DATA_SALARIO <- ymd(eppggs_df_wrangled$DATA_SALARIO)



# CREATING THE SECONDARY DATASET (TO BE USED AS LOOKUP TABLE) -----------

lookup_df <- data.frame()
csap_file <- list.files(pattern = 'csap')
sheets <- excel_sheets("csap.xlsx")

for (i in seq_along(sheets)) {
  
  x <- read_excel(csap_file, col_names = TRUE, skip = 1, col_types = "text", sheet = i) %>%
    mutate(csap = sheets[i])
  
  lookup_df <- bind_rows(lookup_df, x)
  rm(x)
}

lookup_df <- lookup_df[-1]

lookup_df <- lookup_df %>% 
  mutate(CODIGO_ALUNO = c(1:1161))

real_names <- c("NOME", "INICIO_GRAD", "CONCLUSAO_GRAD", "SEXO", "CSAP_RANK", "CODIGO_ALUNO")
colnames(lookup_df) <- real_names

# Removing "acentos" and uppering case for "NOME" column
lookup_df$NOME <- toupper(lookup_df$NOME)
lookup_df$NOME <- map_chr(lookup_df$NOME, iconv, from="UTF-8",to="ASCII//TRANSLIT")
lookup_df$CSAP_RANK <- as.numeric(lookup_df$CSAP_RANK)

# Choosing rows to be filtered (not needed)
to_be_removed <- c("DESLIGADO", "DESLIGADO EM 04/02/2013", "DESLIGADO EM 13/05/2013",
                   "DISCPLINAS PENDENTES EM CURSO", NA, "DESLIGADA",
                   "1º/04/2008", "DESSITENTE", "DESISTENTE")

# Creating function to be the "not in" verb:
`%!in%` = Negate(`%in%`)

lookup_df <- lookup_df %>% 
  filter(CONCLUSAO_GRAD %!in% to_be_removed) %>% 
  select(NOME, CODIGO_ALUNO, SEXO, CSAP_RANK, INICIO_GRAD, CONCLUSAO_GRAD) %>% 
  arrange(CSAP_RANK, NOME)

# Coercing for numeric and transforming excel date format in R date format:
lookup_df$INICIO_GRAD <- as.numeric(lookup_df$INICIO_GRAD)
lookup_df$CONCLUSAO_GRAD <- as.numeric(lookup_df$CONCLUSAO_GRAD)
lookup_df$INICIO_GRAD <- as.Date(lookup_df$INICIO_GRAD, origin = "1899-12-30")
lookup_df$CONCLUSAO_GRAD <- as.Date(lookup_df$CONCLUSAO_GRAD, origin = "1899-12-30")


#Remoção de alunos com conclusão de curso em duas datas diferentes (uma para sua turma oriinal e outra na turma com a qual concluiu)
duplicados_nome <- which(duplicated(lookup_df$NOME))

grad_duplicada <- lookup_df %>% filter(NOME %in% lookup_df[c(910, 920), 1]) %>% select(CODIGO_ALUNO) %>% slice(c(3,4))

lookup_df <- lookup_df %>% 
  filter(CODIGO_ALUNO %!in% grad_duplicada$CODIGO_ALUNO)

# Joining lookup df with the main dataset for forging the Final Dataset for analysis
eppggs_df_final <- left_join(eppggs_df_wrangled, lookup_df, by = "NOME")

#Final adjustments:
eppggs_df_final <- eppggs_df_final[-c(3,4)]

eppggs_df_final <- eppggs_df_final %>% 
  mutate(CSAP = paste0("CSAP ", CSAP_RANK))

rm(eppggs_df, lookup_df, eppggs_df_wrangled, all_files, csap_file, real_cols_names,
   real_names, sheets, temp_01, to_be_removed, i)


# Deduplicação dos locais de trabalho ----------------------------------------------
all_places <- eppggs_df_final %>% count(ORGAO_ENTIDADE)

eppggs_dataset <- eppggs_df_final %>% 
  mutate(CARGO = case_when(CARGO_COMISSAO == "" ~ "SEM CARGO",
                          TRUE ~ "COM CARGO")) %>%
  mutate(ORGAO_ENTIDADE = case_when(ORGAO_ENTIDADE == all_places[[1]][9] ~ all_places[[1]][8],
                                    ORGAO_ENTIDADE == all_places[[1]][12] ~ all_places[[1]][11],
                                    ORGAO_ENTIDADE == all_places[[1]][25] ~ all_places[[1]][24],
                                    ORGAO_ENTIDADE == all_places[[1]][28] ~ all_places[[1]][27],
                                    ORGAO_ENTIDADE == all_places[[1]][30] ~ all_places[[1]][29],
                                    ORGAO_ENTIDADE == all_places[[1]][32] ~ all_places[[1]][31],
                                    ORGAO_ENTIDADE == all_places[[1]][34] ~ all_places[[1]][33],
                                    ORGAO_ENTIDADE == all_places[[1]][41] ~ all_places[[1]][40],
                                    ORGAO_ENTIDADE == all_places[[1]][46] ~ all_places[[1]][49],
                                    ORGAO_ENTIDADE == all_places[[1]][65] ~ all_places[[1]][66],
                                    ORGAO_ENTIDADE == all_places[[1]][78] ~ all_places[[1]][77],
                                    ORGAO_ENTIDADE == all_places[[1]][80] ~ all_places[[1]][79],
                                    TRUE ~ ORGAO_ENTIDADE)) %>%
  mutate(ANO_SALARIO = year(DATA_SALARIO)) %>% 
  filter(!(is.na(CSAP))) %>%
  select("CODIGO", "NOME", "CODIGO_ALUNO", "SEXO", "CSAP", "CSAP_RANK", "CARGO_COMISSAO", "CARGO", "ORGAO_ENTIDADE", "DESCUNID",
         "REM_BASICA_BRUTA", "FERIAS", "IRRF", "CONT.PREVIDENCIRIA", "EVENTUAL", "DECTER",
         "REM_POS_DEDUCOES", "DATA_SALARIO", "INICIO_GRAD", "CONCLUSAO_GRAD", "ANO_SALARIO")
  

save(... = eppggs_dataset, file = "eppggs_dataset.RData")