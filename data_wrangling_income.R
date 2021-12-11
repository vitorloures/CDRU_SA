### Limpeza dos dados ###

library(readxl)
library(writexl)
library(tidyverse)
library(magrittr, include.only = '%>%')
library(tibble)

uf_para_regiao <- function(uf) {
  norte <- c("AM", "AC", "PA", "TO", "RO", "RR", "AP")
  nordeste <- c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA")
  centro_oeste <- c("MT", "MS", "GO", "DF")
  sul <- c("RS", "SC", "PR")
  sudeste <- c("MG", "SP", "RJ", "ES")
  if (uf %in% norte) {
    regiao <- "NORTE"
  }
  else if(uf %in% nordeste){
    regiao <- "NORDESTE" 
  }
  else if(uf %in% sudeste){
    regiao <- "SUDESTE"
  }
  else if(uf %in% sul){
    regiao <- "SUL"
  }
  else if(uf %in% centro_oeste){
    regiao <- "CENTRO OESTE"
  }
  return (regiao)
}
  
agrega_por_cnae_e_var_brasil <- function(input_df, var) {
  # Iremos calcular o desvio da média como a variação percentual entre a média do segmento analisado 
  # e a média dos trabalhadores de todas as CNAEs analisadas. Note que a média das CNAEs de emprego
  # direto, e as CNAEs de emprego indireto serao diferentes
  
    media_geral <- mean(input_df$valor_remuneracao_media)
      
    output_df <- input_df %>%
      group_by(cnae_2_subclasse,{{var}}) %>% 
      summarise(n_trabalhadores_segmento = n(), 
                media_salario = mean(valor_remuneracao_media), 
                desvio_media = round(100*(media_salario / media_geral - 1),2))
    
    output_df <- output_df %>%
      group_by(cnae_2_subclasse) %>% 
      mutate(n_cnae = sum(n_trabalhadores_segmento)) %>% 
      group_by({{var}}) %>% 
      mutate(percentual_da_cnae=round(100*n_trabalhadores_segmento/n_cnae,2)) %>%
      select(-n_cnae)
    return (output_df)
}

agrega_por_cnae_e_var_uf <- function(input_df, var) {
  # Iremos calcular o desvio da média como a variação percentual entre a média do segmento analisado 
  # e a média dos trabalhadores de todas as CNAEs analisadas. Note que a média das CNAEs de emprego
  # direto, e as CNAEs de emprego indireto serao diferentes
  
  media_geral <- mean(input_df$valor_remuneracao_media)
  
  output_df <- input_df %>%
    group_by(sigla_uf, cnae_2_subclasse,{{var}}) %>% 
    summarise(n_trabalhadores_segmento = n(), 
              media_salario = mean(valor_remuneracao_media), 
              desvio_media = round(100*(media_salario / media_geral - 1),2))

  return (output_df)
}

agrega_por_cnae_e_var_regiao <- function(input_df, var) {
  # Iremos calcular o desvio da média como a variação percentual entre a média do segmento analisado 
  # e a média dos trabalhadores de todas as CNAEs analisadas. Note que a média das CNAEs de emprego
  # direto, e as CNAEs de emprego indireto serao diferentes
  
  media_geral <- mean(input_df$valor_remuneracao_media)
  
  output_df <- input_df %>%
    group_by(regiao, cnae_2_subclasse,{{var}}) %>% 
    summarise(n_trabalhadores_segmento = n(), 
              media_salario = mean(valor_remuneracao_media), 
              desvio_media = round(100*(media_salario / media_geral - 1),2))
  
  return (output_df)
}
  
agregacao_dupla_por_cnae_brasil <- function(input_df, var1, var2) {
  # Iremos calcular o desvio da média como a variação percentual entre a média do segmento analisado 
  # e a média dos trabalhadores de todas as CNAEs analisadas. Note que a média das CNAEs de emprego
  # direto, e as CNAEs de emprego indireto serao diferentes
  
  media_geral <- mean(input_df$valor_remuneracao_media)
  
  output_df <- input_df %>%
    group_by(cnae_2_subclasse,{{var1}}, {{var2}}) %>% 
    summarise(n_trabalhadores_segmento = n(), 
              media_salario = mean(valor_remuneracao_media), 
              desvio_media = round(100*(media_salario / media_geral - 1),2))
  
  output_df <- output_df %>%
    group_by(cnae_2_subclasse) %>% 
    mutate(n_cnae = sum(n_trabalhadores_segmento)) %>% 
    group_by({{var1}}, {{var2}}) %>% 
    mutate(percentual_da_cnae=round(100*n_trabalhadores_segmento/n_cnae,2)) %>%
    select(-n_cnae)
  return (output_df)
}

agregacao_dupla_por_cnae_regiao <- function(input_df, var1, var2) {
  # Iremos calcular o desvio da média como a variação percentual entre a média do segmento analisado 
  # e a média dos trabalhadores de todas as CNAEs analisadas. Note que a média das CNAEs de emprego
  # direto, e as CNAEs de emprego indireto serao diferentes
  
  media_geral <- mean(input_df$valor_remuneracao_media)
  
  output_df <- input_df %>%
    group_by(regiao, cnae_2_subclasse,{{var1}}, {{var2}}) %>% 
    summarise(n_trabalhadores_segmento = n(), 
              media_salario = mean(valor_remuneracao_media), 
              desvio_media = round(100*(media_salario / media_geral - 1),2))
  
  output_df <- output_df %>%
    group_by(regiao, cnae_2_subclasse) %>% 
    mutate(n_cnae = sum(n_trabalhadores_segmento)) %>% 
    group_by({{var1}}, {{var2}}) %>% 
    mutate(percentual_da_cnae=round(100*n_trabalhadores_segmento/n_cnae,2)) %>%
    select(-n_cnae)
  return (output_df)
}

  
  rais_direto_vinculos <- read_csv(file = file.path(getwd(), "2019_rais_microdados_vinculos_direto.csv"),
                                   show_col_types = FALSE)
  
  rais_direto_estabele <- read_csv(file = file.path(getwd(), "2019_rais_microdados_estabelecimentos_direto.csv"),
                                   show_col_types = FALSE)
  
  rais_indireto_vinculos <- read_csv(file = file.path(getwd(), "2019_rais_microdados_vinculos_indireto.csv"),
                                   show_col_types = FALSE)
  
  rais_indireto_estabele <- read_csv(file = file.path(getwd(), "2019_rais_microdados_estabelecimentos_indireto.csv"),
                                   show_col_types = FALSE)

# TODO: Analisar dados valor_remuneracao_media_nominal == 0
# Filtra apenas linhas e colunas necessárias para geração das tabelas a seguir
direto_vinculos_tratado <- rais_direto_vinculos %>% 
    select(ano, sigla_uf, id_municipio, valor_remuneracao_media, cbo_2002, 
           idade, faixa_etaria, sexo, raca_cor, nacionalidade, indicador_portador_deficiencia, tipo_deficiencia,
           cnae_2_subclasse, grau_instrucao_apos_2005) %>%
    dplyr::filter(ano == 2019 & valor_remuneracao_media > 0)  

direto_vinculos_tratado$regiao <- lapply(direto_vinculos_tratado$sigla_uf, 
                                         FUN=uf_para_regiao)

indireto_vinculos_tratado <- rais_indireto_vinculos %>% 
  select(ano, sigla_uf, id_municipio, valor_remuneracao_media_nominal, cbo_2002, 
         idade, faixa_etaria, sexo, raca_cor, nacionalidade, indicador_portador_deficiencia, tipo_deficiencia,
         cnae_2_subclasse, grau_instrucao_apos_2005) %>%
  dplyr::filter(ano == 2019 & valor_remuneracao_media_nominal > 0) %>%
  rename(valor_remuneracao_media=valor_remuneracao_media_nominal)

indireto_vinculos_tratado$regiao <- lapply(indireto_vinculos_tratado$sigla_uf, 
                                         FUN=uf_para_regiao)

# TABELA 1: Total de trabalhadores e média salarial por CNAE
brasil_direto_geral <- direto_vinculos_tratado %>%
      group_by(cnae_2_subclasse) %>% 
      summarise(numero_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 

brasil_indireto_geral <- indireto_vinculos_tratado %>%
  group_by(cnae_2_subclasse) %>% 
  summarise(numero_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 

write_xlsx(brasil_direto_geral,path =file.path(getwd(), "brasil_direto_geral.xlsx"))
write_xlsx(brasil_indireto_geral,path =file.path(getwd(), "brasil_indireto_geral.xlsx"))


# TABELA 2: Visualização por região geográfica 

brasil_dct_por_regiao <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, regiao)
brasil_idct_por_regiao <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, regiao)

write_xlsx(brasil_dct_por_regiao,path =file.path(getwd(), "brasil_direto_regiao.xlsx"))
write_xlsx(brasil_idct_por_regiao,path =file.path(getwd(), "brasil_indireto_regiao.xlsx"))
  
# DEMAIS TABELAS
# Raça agregado a nível Brasil

brasil_dct_por_raca <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, raca_cor)
brasil_idct_por_raca <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, raca_cor)

write_xlsx(brasil_dct_por_raca,path =file.path(getwd(), "brasil_direto_raca.xlsx"))
write_xlsx(brasil_idct_por_raca,path =file.path(getwd(), "brasil_indireto_raca.xlsx"))

# Raça agregado a nível UF

uf_dct_por_raca <- agrega_por_cnae_e_var_uf(direto_vinculos_tratado, raca_cor)
uf_idct_por_raca <- agrega_por_cnae_e_var_uf(indireto_vinculos_tratado, raca_cor)

write_xlsx(uf_dct_por_raca,path =file.path(getwd(), "uf_direto_raca.xlsx"))
write_xlsx(uf_idct_por_raca,path =file.path(getwd(), "uf_indireto_raca.xlsx"))


# Escolaridade: Brasil

brasil_dct_por_escolaridade <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, grau_instrucao_apos_2005)
brasil_idct_por_escolaridade <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, grau_instrucao_apos_2005)

write_xlsx(brasil_dct_por_escolaridade,path =file.path(getwd(), "brasil_direto_escolaridade.xlsx"))
write_xlsx(brasil_idct_por_escolaridade,path =file.path(getwd(), "brasil_indireto_escolaridade.xlsx"))

# Escolaridade: UF

uf_dct_por_escolaridade <- agrega_por_cnae_e_var_uf(direto_vinculos_tratado, grau_instrucao_apos_2005)
uf_idct_por_escolaridade <- agrega_por_cnae_e_var_uf(indireto_vinculos_tratado, grau_instrucao_apos_2005)

write_xlsx(uf_dct_por_escolaridade,path =file.path(getwd(), "uf_direto_escolaridade.xlsx"))
write_xlsx(uf_idct_por_escolaridade,path =file.path(getwd(), "uf_indireto_escolaridade.xlsx"))

# Sexo: Brasil

brasil_dct_por_sexo <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, sexo)
brasil_idct_por_sexo <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, sexo)

write_xlsx(brasil_dct_por_sexo,path =file.path(getwd(), "brasil_direto_sexo.xlsx"))
write_xlsx(brasil_idct_por_sexo,path =file.path(getwd(), "brasil_indireto_sexo.xlsx"))

# Sexo: UF

uf_dct_por_sexo <- agrega_por_cnae_e_var_uf(direto_vinculos_tratado, sexo)
uf_idct_por_sexo <- agrega_por_cnae_e_var_uf(indireto_vinculos_tratado, sexo)

write_xlsx(uf_dct_por_sexo,path =file.path(getwd(), "uf_direto_sexo.xlsx"))
write_xlsx(uf_idct_por_sexo,path =file.path(getwd(), "uf_indireto_sexo.xlsx"))

# Faixa etária: Brasil

brasil_dct_por_faixa_etaria <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, faixa_etaria)
brasil_idct_por_faixa_etaria <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, faixa_etaria)

write_xlsx(brasil_dct_por_faixa_etaria,path =file.path(getwd(), "brasil_direto_faixa_etaria.xlsx"))
write_xlsx(brasil_idct_por_faixa_etaria,path =file.path(getwd(), "brasil_indireto_faixa_etaria.xlsx"))

# Faixa etária: UF

uf_dct_por_faixa_etaria <- agrega_por_cnae_e_var_uf(direto_vinculos_tratado, faixa_etaria)
uf_idct_por_faixa_etaria <- agrega_por_cnae_e_var_uf(indireto_vinculos_tratado, faixa_etaria)

write_xlsx(uf_dct_por_faixa_etaria,path =file.path(getwd(), "uf_direto_faixa_etaria.xlsx"))
write_xlsx(uf_idct_por_faixa_etaria,path =file.path(getwd(), "uf_indireto_faixa_etaria.xlsx"))

# Portador deficiência: Brasil

brasil_dct_por_deficiencia <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, indicador_portador_deficiencia)
brasil_idct_por_deficiencia <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, indicador_portador_deficiencia)

write_xlsx(brasil_dct_por_deficiencia,path =file.path(getwd(), "brasil_direto_deficiencia.xlsx"))
write_xlsx(brasil_idct_por_deficiencia,path =file.path(getwd(), "brasil_indireto_deficiencia.xlsx"))

# Portador deficiência: Região

regiao_dct_por_deficiencia  <- agrega_por_cnae_e_var_regiao(direto_vinculos_tratado, indicador_portador_deficiencia)
regiao_idct_por_deficiencia  <- agrega_por_cnae_e_var_regiao(indireto_vinculos_tratado, indicador_portador_deficiencia)

write_xlsx(regiao_dct_por_deficiencia,path =file.path(getwd(), "regiao_direto_deficiencia.xlsx"))
write_xlsx(regiao_idct_por_deficiencia,path =file.path(getwd(), "regiao_indireto_deficiencia.xlsx"))

# Portador deficiência: UF

uf_dct_por_deficiencia  <- agrega_por_cnae_e_var_uf(direto_vinculos_tratado, indicador_portador_deficiencia)
uf_idct_por_deficiencia  <- agrega_por_cnae_e_var_uf(indireto_vinculos_tratado, indicador_portador_deficiencia)

write_xlsx(uf_dct_por_deficiencia,path =file.path(getwd(), "uf_direto_deficiencia.xlsx"))
write_xlsx(uf_idct_por_deficiencia,path =file.path(getwd(), "uf_indireto_deficiencia.xlsx"))

# Tipo de deficiência: Brasil

brasil_dct_por_tipo_deficiencia <- agrega_por_cnae_e_var_brasil(
  dplyr::filter(direto_vinculos_tratado, tipo_deficiencia>0), tipo_deficiencia)
brasil_idct_por_tipo_deficiencia <- agrega_por_cnae_e_var_brasil(
  dplyr::filter(indireto_vinculos_tratado, tipo_deficiencia>0), tipo_deficiencia)

write_xlsx(brasil_dct_por_tipo_deficiencia,path =file.path(getwd(), "brasil_direto_tipo_deficiencia.xlsx"))
write_xlsx(brasil_idct_por_tipo_deficiencia,path =file.path(getwd(), "brasil_indireto_tipo_deficiencia.xlsx"))

# Tipo de deficiência: Região

regiao_dct_por_tipo_deficiencia <- agrega_por_cnae_e_var_regiao(
  dplyr::filter(direto_vinculos_tratado, tipo_deficiencia>0), tipo_deficiencia)
regiao_idct_por_tipo_deficiencia <- agrega_por_cnae_e_var_regiao(
  dplyr::filter(indireto_vinculos_tratado, tipo_deficiencia>0), tipo_deficiencia)

write_xlsx(regiao_dct_por_tipo_deficiencia,path =file.path(getwd(), "regiao_direto_tipo_deficiencia.xlsx"))
write_xlsx(regiao_idct_por_tipo_deficiencia,path =file.path(getwd(), "regiao_indireto_tipo_deficiencia.xlsx"))

# Sexo e Raça: Brasil

brasil_dct_sexo_raca <- agregacao_dupla_por_cnae_brasil(direto_vinculos_tratado, sexo, raca_cor)
brasil_idct_sexo_raca <- agregacao_dupla_por_cnae_brasil(indireto_vinculos_tratado, sexo, raca_cor)

write_xlsx(brasil_dct_sexo_raca, path =file.path(getwd(), "brasil_direto_sexo_raca.xlsx"))
write_xlsx(brasil_idct_sexo_raca, path =file.path(getwd(), "brasil_indireto_sexo_raca.xlsx"))

# Sexo e Raça: Região

regiao_dct_sexo_raca <- agregacao_dupla_por_cnae_regiao(direto_vinculos_tratado, sexo, raca_cor)
regiao_idct_sexo_raca <- agregacao_dupla_por_cnae_regiao(indireto_vinculos_tratado, sexo, raca_cor)

write_xlsx(regiao_dct_sexo_raca, path =file.path(getwd(), "regiao_direto_sexo_raca.xlsx"))
write_xlsx(regiao_idct_sexo_raca, path =file.path(getwd(), "regiao_indireto_sexo_raca.xlsx"))

# Sexo e Escolaridade: Brasil

brasil_dct_sexo_escolaridade <- agregacao_dupla_por_cnae_brasil(direto_vinculos_tratado, sexo, grau_instrucao_apos_2005)
brasil_idct_sexo_escolaridade <- agregacao_dupla_por_cnae_brasil(indireto_vinculos_tratado, sexo, grau_instrucao_apos_2005)

write_xlsx(brasil_dct_sexo_escolaridade, path =file.path(getwd(), "brasil_direto_sexo_escolaridade.xlsx"))
write_xlsx(brasil_idct_sexo_escolaridade, path =file.path(getwd(), "brasil_indireto_sexo_escolaridade.xlsx"))

# Sexo e Escolaridade: Brasil

regiao_dct_sexo_escolaridade <- agregacao_dupla_por_cnae_regiao(direto_vinculos_tratado, sexo, grau_instrucao_apos_2005)
regiao_idct_sexo_escolaridade <- agregacao_dupla_por_cnae_regiao(indireto_vinculos_tratado, sexo, grau_instrucao_apos_2005)

write_xlsx(regiao_dct_sexo_escolaridade, path =file.path(getwd(), "regiao_direto_sexo_escolaridade.xlsx"))
write_xlsx(regiao_idct_sexo_escolaridade, path =file.path(getwd(), "regiao_indireto_sexo_escolaridade.xlsx"))


# Isso é tudo pessoal:
beepr::beep(sound=3)