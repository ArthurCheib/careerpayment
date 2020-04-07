## Função que cálcula a média de uma turma com cargo nos primeiros 2 anos de trabalho
calc_media_turma_com_cargo <- function(semestre, ano) {
  

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
  
  ### Média dos entrantes que possui cargo ao fim de 2anos de trbaalho
  x <<- eppggs_dataset %>% 
    filter(CODIGO %in% entrantes_consistentes &
             (DATA_SALARIO > ymd(limite_inferior) & DATA_SALARIO < ymd(limite_superior_x2))) %>% 
    ungroup() %>%
    count(CODIGO, CARGO) %>% 
    filter(CARGO == "COM CARGO") %>% nrow(.)/length(entrantes_consistentes)*100 %>%
    tibble::enframe(name = NULL) %>%
    setNames(c("TEMP_MEDIO_CARGO")) 
  
  x <<- x %>%
    mutate(NM_ENTRANTE = str_c("Entrante ", semestre, "/", ano)) %>%
    select(NM_ENTRANTE, TEMP_MEDIO_CARGO)
  
  print(x)
  
}



  