library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)

# CREATING THE MAIN DATASET ------------------------------

## For the purpose of creating the dataset that is going to be used all along the project we did three things:

# 1) Collected all the payment data avaiable on the "Portal da Transparência MG".
# 2) Collected all the public data about the "Fundação João Pinheiro" alumni that could be shared by this institution.
# 3) Cleaned both dataset in the sequence presented bellow and then joined then:

setwd("C:/Users/arthu/OneDrive/Trabalho_SEE/Datasets/pay_servidores")
eppggs_df <- data.frame()
all_files <- list.files(path = getwd(), pattern = '.csv')

### This dataset contains the payment for all the Minas Gerais state careers, but we select only the interest group,
### which is the EPPGG career.

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

# Removing errors within the name columns (when loading the data it arose) - as a name columns it should contain only letters.
temp_01 <- which(str_detect(eppggs_df$NOME, "\\d"))
eppggs_df <- eppggs_df[-temp_01, ]

#Removing "acentos" for preparing the data to be joined (making it equal to the lookup_df)
eppggs_df$NOME <- map_chr(eppggs_df$NOME, iconv, from="UTF-8",to="ASCII//TRANSLIT")

# LAST WRANGLING FOR THE FINAL ARRANGE

# Separating last column into three cols and removing two other ones.
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

## This dataset consists in the public variables requested to the Fundação João Pinheiro.

lookup_df <- data.frame()
csap_file <- list.files(pattern = 'csap.xlsx')
sheets <- excel_sheets("csap.xlsx")

for (i in seq_along(sheets)) {
  
  x <- read_excel(csap_file, col_names = TRUE, skip = 1, col_types = "text", sheet = i) %>%
    mutate(csap = sheets[i])
  
  lookup_df <- bind_rows(lookup_df, x)
  rm(x)
}

lookup_df <- lookup_df[-1]

lookup_df <- lookup_df %>% 
  mutate(CODIGO_ALUNO = c(1:nrow(lookup_df)))

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

lookup_df <- lookup_df %>% 
  filter(!CONCLUSAO_GRAD %in% to_be_removed) %>% 
  select(NOME, CODIGO_ALUNO, SEXO, CSAP_RANK, INICIO_GRAD, CONCLUSAO_GRAD) %>% 
  arrange(CSAP_RANK, NOME)

# Coercing for numeric and transforming look date format in R date format:
lookup_df$INICIO_GRAD <- as.numeric(lookup_df$INICIO_GRAD)
lookup_df$CONCLUSAO_GRAD <- as.numeric(lookup_df$CONCLUSAO_GRAD)
lookup_df$INICIO_GRAD <- as.Date(lookup_df$INICIO_GRAD, origin = "1899-12-30")
lookup_df$CONCLUSAO_GRAD <- as.Date(lookup_df$CONCLUSAO_GRAD, origin = "1899-12-30")


# Removing names that appears for more then one class (picked the entry class)
duplicados_nome <- which(duplicated(lookup_df$NOME))

grad_duplicada <- lookup_df %>%
  filter(NOME %in% lookup_df[duplicados_nome, 1]) %>%
  select(CODIGO_ALUNO) %>%
  slice(c(3,4))

lookup_df <- lookup_df %>% 
  filter(!CODIGO_ALUNO %in% grad_duplicada$CODIGO_ALUNO)

# Joining lookup df with the main dataset for creating the Final Dataset for analysis
eppggs_df_final <- left_join(eppggs_df_wrangled, lookup_df, by = "NOME")

#Final adjustments -removing unnecessary columns:
eppggs_df_final <- eppggs_df_final[-c(3,4)]

eppggs_df_final <- eppggs_df_final %>% 
  mutate(CSAP = paste0("CSAP ", CSAP_RANK))

rm(eppggs_df, lookup_df, eppggs_df_wrangled, all_files, csap_file, real_cols_names,
   real_names, sheets, temp_01, to_be_removed, i)


# Removing duplicated workplaces and merging names into one big area of work
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
  

eppggs_dataset <- eppggs_dataset %>% 
  mutate(GRUPO_ORGAO = case_when(ORGAO_ENTIDADE == "DEFESA SOCIAL" ~ "SEGURANCA PUBLICA",
                                 ORGAO_ENTIDADE == "ADMINISTRACAO PRISIONAL" ~ "SEGURANCA PUBLICA",
                                 ORGAO_ENTIDADE == "POLICIA CIVIL"  ~ "SEGURANCA PUBLICA",
                                 ORGAO_ENTIDADE == "SEGURANCA PUBLICA" ~ "SEGURANCA PUBLICA",
                                 ORGAO_ENTIDADE == "SECRETARIA DE ESTADO DE CIENCIA TECNOLOGIA" ~ "EDUCACAO",
                                 ORGAO_ENTIDADE == "UEMG-UNIV.EST. DE MINAS GERAIS" ~ "EDUCACAO",
                                 ORGAO_ENTIDADE == "SECRETARIA DE EDUCACAO" ~ "EDUCACAO",
                                 ORGAO_ENTIDADE == "SECRETARIA DE SAUDE" ~ "SAUDE",
                                 ORGAO_ENTIDADE == "ESCOLA DE SAUDE" ~ "SAUDE",
                                 ORGAO_ENTIDADE == "FHEMIG FUND HOSPITALAR EST MG" ~ "SAUDE",
                                 ORGAO_ENTIDADE == "FUNDACAO HEMOMINAS" ~ "SAUDE",
                                 ORGAO_ENTIDADE == "SEC. DE PLANEJAMENTO E GESTAO" ~ "PLANEJAMENTO E GESTAO",
                                 ORGAO_ENTIDADE == "SEC. DESENVOLVIMENTO ECONOMICO" ~ "DESENVOLVIMENTO ECONOMICO",
                                 ORGAO_ENTIDADE == "SECRETARIA DA FAZENDA" ~ "DESENVOLVIMENTO ECONOMICO",
                                 ORGAO_ENTIDADE == "SEC ESTADO DESENVOLVIMENTO ECONOMICO CIENCIA TECNOLOGIA" ~ "DESENVOLVIMENTO ECONOMICO",
                                 ORGAO_ENTIDADE == "SEC DE ESTADO DE DIREITOS HUMANOS PARTICIPACAO SOCIAL E CIDADANIA" ~ "DESENVOLVIMENTO SOCIAL",
                                 ORGAO_ENTIDADE == "SEC. DESENV.SOCIAL" ~ "DESENVOLVIMENTO SOCIAL",
                                 ORGAO_ENTIDADE == "SECRETARIA DE ESTADO DE TRABALHO E EMPREGO" ~ "DESENVOLVIMENTO SOCIAL",
                                 ORGAO_ENTIDADE == "SECRETARIA DE ESTADO DE DESENVOLVIMENTO AGRARIO" ~ "DESENVOLVIMENTO SOCIAL",
                                 ORGAO_ENTIDADE == "FUNDACAO JOAO PINHEIRO" ~ "FUNDACAO JOAO PINHEIRO",
                                 TRUE ~ "OUTROS"))

save(... = eppggs_dataset, file = "eppggs_dataset.RData")
