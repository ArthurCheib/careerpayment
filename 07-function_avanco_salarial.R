## Função que cálcula o crescimento percentual dos salários dos novos entrantes durante os 2 anos iniciais na carreira

calc_evol_salarial <- function(semestre, ano) {

  ### Criando os limites dos salários
  if (semestre == 1) {
    
    mes_limite <- 1
    
  } else if (semestre == 2) {
    
    mes_limite <- 7
    
  }
  
  ano_limite <- ano
  
  limite_inferior <- str_c(ano_limite, "-0", mes_limite, "-", "05")
  limite_superior <- str_c(ano_limite+1, "-0", mes_limite, "-", "05")
  limite_superior_x2 <- str_c(ano_limite+2, "-0", mes_limite, "-", "05")
  
  ### Filtrando os eppggs com salários antes do limite inferior
  eppggs_antigos <- eppggs_dataset %>% 
    filter(DATA_SALARIO < ymd(limite_inferior)) %>% 
    pull(CODIGO) %>% 
    unique()
  
  ### Filtrandos os eppggs com salário somente após o limite inferior
  eppgg_entrantes_pos_limite_inferior <- todos_eppggs[which(!todos_eppggs %in% eppggs_antigos)]
  
  ### Filtrando os eppggs que entraram no segundo semestre de 2014:
  #### Tem o primeiro pagamento depois do limite desejado
  #### 
  #### 
  #### 
  
  entrantes <- eppggs_dataset %>%
    filter(DATA_SALARIO >= ymd(limite_inferior) & DATA_SALARIO < ymd(limite_superior)) %>% 
    filter(CODIGO %in% eppgg_entrantes_pos_limite_inferior) %>%
    filter(REM_BASICA_BRUTA < 8000 & REM_BASICA_BRUTA > 1800) %>%
    group_by(CODIGO, CSAP) %>% 
    summarize(TOTAL_SALARIOS_ANO = n()) %>%
    ungroup() %>%
    filter(TOTAL_SALARIOS_ANO > 6) %>% 
    pull(CODIGO)
  
  ### Filtrando os novos entrantes no segundo semestre de 2014 que tiveram consistência mínima de 16 salários em 24.
  entrantes_consistentes <- eppggs_dataset %>%
    filter(CODIGO %in% entrantes) %>% 
    filter(DATA_SALARIO >= ymd(limite_inferior) & DATA_SALARIO < ymd(limite_superior_x2)) %>% 
    group_by(CODIGO) %>% 
    summarize(TOTAL_SALARIOS_ANO = n()) %>% 
    filter(TOTAL_SALARIOS_ANO > 16) %>% 
    pull(CODIGO)
  
  ### Data frame que será utilizada para análise do entrante de 2014-02:
  df_entrantes <- eppggs_dataset %>% 
    filter(CODIGO %in% entrantes_consistentes & (DATA_SALARIO > ymd(limite_inferior) & DATA_SALARIO < ymd(limite_superior_x2))) %>% 
    select(CODIGO, CARGO_COMISSAO, CARGO, ORGAO_ENTIDADE, REM_BASICA_BRUTA, DATA_SALARIO, ANO_SALARIO) %>% 
    arrange(CODIGO, DATA_SALARIO) %>%
    ungroup() %>% 
    group_by(CODIGO) %>% 
    mutate(MES_SALARIO = order(CODIGO))
 
  ### Analisando o tempo médio dos que entraram e conseguiram cargo - em quanto tempo foi?
  salario_limite <- df_entrantes %>% 
    ungroup() %>%
    arrange(CODIGO, MES_SALARIO) %>% 
    group_by(CODIGO) %>% 
    slice(min(MES_SALARIO), max(MES_SALARIO)) %>% 
    arrange(CODIGO, MES_SALARIO) %>% 
    select(CODIGO, REM_BASICA_BRUTA, MES_SALARIO) %>%
    mutate(MES_SALARIO_DESC = case_when(MES_SALARIO < 12 ~ "PRIMEIRO_SALARIO",
                                        TRUE ~ "ULTIMO_SALARIO")) %>% 
    select(CODIGO, MES_SALARIO_DESC, REM_BASICA_BRUTA) %>% 
    spread(key = MES_SALARIO_DESC, value = REM_BASICA_BRUTA) %>% 
    pull(PRIMEIRO_SALARIO) %>% 
    median()*0.9
  
  x <- df_entrantes %>% 
    ungroup() %>%
    arrange(CODIGO, REM_BASICA_BRUTA, MES_SALARIO) %>% 
    filter(REM_BASICA_BRUTA > salario_limite) %>% 
    group_by(CODIGO, REM_BASICA_BRUTA) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(CODIGO, REM_BASICA_BRUTA, MES_SALARIO) %>%
    group_by(CODIGO) %>% 
    mutate(PERC_AVANCO_MENSAL = round((REM_BASICA_BRUTA/min(REM_BASICA_BRUTA)-1)*100, digits = 2)) %>% 
    filter(PERC_AVANCO_MENSAL != 0) %>% 
    ungroup() %>% 
    group_by(CODIGO) %>% 
    filter(MES_SALARIO == max(MES_SALARIO) | MES_SALARIO == min(MES_SALARIO)) %>% 
    ungroup()
  
}
  