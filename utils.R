# Funções em comum

library(readxl)
library(openxlsx)
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
  # , e as CNAEs de emprego in serao diferentes
  
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
  # , e as CNAEs de emprego in serao diferentes
  
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
  # , e as CNAEs de emprego in serao diferentes
  
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
  # , e as CNAEs de emprego in serao diferentes
  
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
  # , e as CNAEs de emprego in serao diferentes
  
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

extract_vinculos <- function(csv_vinculos) {
  rais_vinculos <- read_csv(file = file.path(getwd(), csv_vinculos),
                            show_col_types = FALSE)

  
  # Filtra apenas linhas e colunas necessárias para geração das tabelas a seguir
  rais_vinculos_tratado <- rais_vinculos %>% 
    select(ano, sigla_uf, id_municipio, valor_remuneracao_media, cbo_2002, 
           idade, faixa_etaria, sexo, raca_cor, nacionalidade, indicador_portador_deficiencia, tipo_deficiencia,
           cnae_2_subclasse, grau_instrucao_apos_2005) %>%
    dplyr::filter(ano == 2019 & valor_remuneracao_media > 0)  
  
  rais_vinculos_tratado$regiao <- lapply(rais_vinculos_tratado$sigla_uf, 
                                           FUN=uf_para_regiao)
  
  return(rais_vinculos_tratado)
}

extract_estabelecimentos <- function(csv_estabelecimentos){
  rais_estabele <- read_csv(file = file.path(getwd(), csv_estabelecimentos),
                            show_col_types = FALSE)
  
  rais_estabele_tratado <- rais_estabele %>% 
    dplyr::filter(ano == 2019 & indicador_atividade_ano == 1) %>%
    select(ano, sigla_uf, id_municipio,
           quantidade_vinculos_ativos, 
           natureza_juridica, tamanho, cnae_2_subclasse
    ) 
  
  rais_estabele_tratado$regiao <- lapply(rais_estabele_tratado$sigla_uf, 
                                         FUN=uf_para_regiao)
  
  
  codigo_municipio_ibge <- read_excel(path = file.path(getwd(), "RELATORIO_DTB_BRASIL_MUNICIPIO.xls")) %>%
    select("Código Município Completo", Nome_Município) %>%
    rename(id_municipio = "Código Município Completo", municipio = Nome_Município)
  
  rais_estabele_tratado <- merge(rais_estabele_tratado, codigo_municipio_ibge, by="id_municipio")
  
  dicionario_cnaes <- read_excel(path = file.path(getwd(), "CNAE20_Subclasses_EstruturaDetalhada.xls")) %>% 
    select(5,6) %>%
    rename("cnae_2_subclasse" = 1, "descricao_cnae" = 2) %>%
    na.omit()
  
  dicionario_cnaes$cnae_2_subclasse <- gsub("-", "", dicionario_cnaes$cnae_2_subclasse) 
  dicionario_cnaes$cnae_2_subclasse <- gsub("/", "", dicionario_cnaes$cnae_2_subclasse)
  
  rais_estabele_tratado <- merge(rais_estabele_tratado, dicionario_cnaes, by="cnae_2_subclasse")
  
  dicionario_tamanho <- read_csv('dicionario_tamanho.csv') %>% rename("tamanho" = "chave", "tamanho_desc" = "valor") %>% 
    select(tamanho, tamanho_desc)
  
  rais_estabele_tratado <- merge(rais_estabele_tratado, dicionario_tamanho, by="tamanho")
  
  return(rais_estabele_tratado)
}

replace_categorical_variable_to_label <- function(df){
  # Código de município  
  codigo_municipio_ibge <- read_excel(path = file.path(getwd(), "RELATORIO_DTB_BRASIL_MUNICIPIO.xls")) %>%
    select("Código Município Completo", Nome_Município) %>%
    rename(id_municipio = "Código Município Completo", municipio = Nome_Município)
  
  df <- merge(df, codigo_municipio_ibge, by="id_municipio")
  
  # Nome da classe CNAE
  dicionario_cnaes <- read_excel(path = file.path(getwd(), "CNAE20_Subclasses_EstruturaDetalhada.xls")) %>% 
    select(5,6) %>%
    rename("cnae_2_subclasse" = 1, "descricao_cnae" = 2) %>%
    na.omit()
  dicionario_cnaes$cnae_2_subclasse <- gsub("-", "", dicionario_cnaes$cnae_2_subclasse) 
  dicionario_cnaes$cnae_2_subclasse <- gsub("/", "", dicionario_cnaes$cnae_2_subclasse)
  df <- merge(df, dicionario_cnaes, by="cnae_2_subclasse")
  
  # Nome da classe pertencente a coluna da RAIS
  dicionario_rais <- read_csv('dicionario_rais.csv') 
  # df_estabelecimentos <- merge(df_estabelecimentos, dicionario_tamanho, by="tamanho")
  
  
  # dicionario_tamanho <- read_csv('dicionario_tamanho.csv') %>% select(tamanho, tamanho_desc)
  # df_estabelecimentos <- merge(df_estabelecimentos, dicionario_tamanho, by="tamanho")
}


generate_estabelecimentos_multiple_aggregation_levels <- function(df_estabelecimentos){
  out <- list()
  
  brasil_n_estabelecimentos <- df_estabelecimentos %>%
    group_by(cnae_2_subclasse, descricao_cnae) %>% 
    summarise(n_estabelecimentos = n()) 
  
  out$brasil_n_estabelecimentos <- brasil_n_estabelecimentos
  
  brasil_n_vinculos <- df_estabelecimentos %>%
    group_by(cnae_2_subclasse, descricao_cnae) %>% 
    summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 
  out$brasil_n_vinculos <- brasil_n_vinculos
  
  brasil_n_estabelecimentos_tamanho <- df_estabelecimentos %>%
    group_by(cnae_2_subclasse, descricao_cnae, tamanho, tamanho_desc) %>% 
    summarise(n_estabelecimentos = n()) 
  out$brasil_n_estabelecimentos_tamanho  <- brasil_n_estabelecimentos_tamanho
  
  regiao_n_estabelecimentos <- df_estabelecimentos %>%
    group_by(regiao, cnae_2_subclasse, descricao_cnae) %>% 
    summarise(n_estabelecimentos = n()) 
  out$regiao_n_estabelecimentos <- regiao_n_estabelecimentos
  
  regiao_n_vinculos <- df_estabelecimentos %>%
    group_by(regiao, cnae_2_subclasse, descricao_cnae) %>% 
    summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 
  out$regiao_n_vinculos <- regiao_n_vinculos
  
  regiao_n_estabelecimentos_tamanho <- df_estabelecimentos %>%
    group_by(regiao, cnae_2_subclasse, descricao_cnae, tamanho, tamanho_desc) %>% 
    summarise(n_estabelecimentos = n()) 
  out$regiao_n_estabelecimentos_tamanho <- regiao_n_estabelecimentos_tamanho
  
  uf_n_estabelecimentos <- df_estabelecimentos %>%
    group_by(sigla_uf, cnae_2_subclasse, descricao_cnae) %>% 
    summarise(n_estabelecimentos = n()) 
  out$uf_n_estabelecimentos <- uf_n_estabelecimentos
  
  uf_n_vinculos <- df_estabelecimentos %>%
    group_by(sigla_uf, cnae_2_subclasse, descricao_cnae) %>% 
    summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 
  out$uf_n_vinculos <- uf_n_vinculos
  
  uf_n_estabelecimentos_tamanho <- df_estabelecimentos %>%
    group_by(sigla_uf, cnae_2_subclasse, descricao_cnae, tamanho, tamanho_desc) %>% 
    summarise(n_estabelecimentos = n()) 
  out$uf_n_estabelecimentos_tamanho <- uf_n_estabelecimentos_tamanho
  
  
  return(out)
}


generate_vinculos_multiple_aggregation_levels <- function(df_vinculos){
  out <- list()
  
  brasil_geral <- df_vinculos %>%
    group_by(cnae_2_subclasse) %>% 
    summarise(ntrabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 
  
  out$brasil_geral <- brasil_geral
  
  regiao_geral <- df_vinculos %>%
    group_by(regiao) %>% 
    summarise(n_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 
  out$regiao_geral <- regiao_geral
  
  uf_geral <- df_vinculos %>%
    group_by(sigla_uf) %>% 
    summarise(n_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 
  out$uf_geral <- uf_geral
  
  brasil_por_regiao <- agrega_por_cnae_e_var_brasil(df_vinculos, regiao)
  out$brasil_por_regiao
  
  brasil_por_raca <- agrega_por_cnae_e_var_brasil(df_vinculos, raca_cor)
  out$brasil_por_raca <- brasil_por_raca
  
  uf_por_raca <- agrega_por_cnae_e_var_uf(df_vinculos, raca_cor)
  out$uf_por_raca <- uf_por_raca
  
  brasil_por_escolaridade <- agrega_por_cnae_e_var_brasil(df_vinculos, grau_instrucao_apos_2005)
  out$brasil_por_escolaridade <- brasil_por_escolaridade
  
  uf_por_escolaridade <- agrega_por_cnae_e_var_uf(df_vinculos, grau_instrucao_apos_2005)
  out$uf_por_escolaridade <- uf_por_escolaridade
  
  brasil_por_sexo <- agrega_por_cnae_e_var_brasil(df_vinculos, sexo)
  out$brasil_por_sexo <- brasil_por_sexo
  
  uf_por_sexo <- agrega_por_cnae_e_var_uf(df_vinculos, sexo)
  out$uf_por_sexo <- uf_por_sexo
  
  brasil_por_faixa_etaria <- agrega_por_cnae_e_var_brasil(df_vinculos, faixa_etaria)
  out$brasil_por_faixa_etaria <- brasil_por_faixa_etaria
  
  uf_por_faixa_etaria <- agrega_por_cnae_e_var_uf(df_vinculos, faixa_etaria)
  out$uf_por_faixa_etaria <- uf_por_faixa_etaria
  
  brasil_por_deficiencia <- agrega_por_cnae_e_var_brasil(df_vinculos, indicador_portador_deficiencia)
  out$brasil_por_deficiencia <- brasil_por_deficiencia
    
  regiao_por_deficiencia  <- agrega_por_cnae_e_var_regiao(df_vinculos, indicador_portador_deficiencia)
  out$regiao_por_deficiencia <- regiao_por_deficiencia
  
  uf_por_deficiencia  <- agrega_por_cnae_e_var_uf(df_vinculos, indicador_portador_deficiencia)
  out$uf_por_deficiencia <- uf_por_deficiencia
    
  brasil_por_tipo_deficiencia <- agrega_por_cnae_e_var_brasil(
    dplyr::filter(df_vinculos, tipo_deficiencia>0), tipo_deficiencia)
  out$brasil_por_tipo_deficiencia <- brasil_por_tipo_deficiencia
  
  regiao_por_tipo_deficiencia <- agrega_por_cnae_e_var_regiao(
    dplyr::filter(df_vinculos, tipo_deficiencia>0), tipo_deficiencia)
  out$regiao_por_tipo_deficiencia <- regiao_por_tipo_deficiencia
  
  brasil_sexo_raca <- agregacao_dupla_por_cnae_brasil(df_vinculos, sexo, raca_cor)
  out$brasil_sexo_raca <- brasil_sexo_raca
  
  regiao_sexo_raca <- agregacao_dupla_por_cnae_regiao(df_vinculos, sexo, raca_cor)
  out$regiao_sexo_raca <- regiao_sexo_raca
  
  brasil_sexo_escolaridade <- agregacao_dupla_por_cnae_brasil(df_vinculos, sexo, grau_instrucao_apos_2005)
  out$brasil_sexo_escolaridade <- brasil_sexo_escolaridade
  
  regiao_sexo_escolaridade <- agregacao_dupla_por_cnae_regiao(df_vinculos, sexo, grau_instrucao_apos_2005)
  out$regiao_sexo_escolaridade <- regiao_sexo_escolaridade
  
  return(out)
}

add_colunas_descricao_dicionario <- function(dfs_vinculos){
  # Nome da classe CNAE
  dicionario_cnaes <- read_excel(path = file.path(getwd(), "CNAE20_Subclasses_EstruturaDetalhada.xls")) %>% 
    select(5,6) %>%
    rename("cnae_2_subclasse" = 1, "descricao_cnae" = 2) %>%
    na.omit()
  dicionario_cnaes$cnae_2_subclasse <- gsub("-", "", dicionario_cnaes$cnae_2_subclasse) 
  dicionario_cnaes$cnae_2_subclasse <- gsub("/", "", dicionario_cnaes$cnae_2_subclasse)
  
  # Dicionário RAIS
  colunas_alvo <- c("raca_cor",
                    "grau_instrucao_apos_2005",
                    "sexo",
                    "faixa_etaria",
                    "indicador_portador_deficiencia",
                    "tipo_deficiencia")
  
  dicionario_rais <- read_csv(file = file.path(getwd(), "dicionario_rais.csv"), show_col_types = FALSE) %>% 
    dplyr::filter(id_tabela == "microdados_vinculos" & nome_coluna %in% colunas_alvo) %>%
    select(nome_coluna, chave, valor)
  
  for (name in names(dfs_vinculos)){
    # região geral e uf_geral não faz nada? 
    if (name %in% c("regiao_geral", "uf_geral")){
          next
    }
    dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], dicionario_cnaes, by="cnae_2_subclasse")

    # Adiciona descrição de raça
    if(name %in% c("brasil_por_raca", "uf_por_raca")){
      dicionario <- gera_dicionario_com_chave_valor(dicionario_rais, "raca_cor") %>%
        rename("raca_cor" = "chave", "descricao_raca_cor" = "valor") 
      
      dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], 
                                 dicionario, by="raca_cor")
    }
    # Adiciona descrição de escolaridade
    if(name %in% c("brasil_por_escolaridade", "uf_por_escolaridade")){
      dicionario <-gera_dicionario_com_chave_valor(dicionario_rais, "grau_instrucao_apos_2005") %>%
        rename("grau_instrucao_apos_2005" = "chave", "descricao_grau_instrucao_apos_2005" = "valor") 
      dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], 
                                 dicionario, by="grau_instrucao_apos_2005")
      
    }
    if(name %in% c("brasil_por_sexo", "uf_por_sexo")){
      dicionario <- gera_dicionario_com_chave_valor(dicionario_rais, "sexo") %>%
        rename("sexo" = "chave", "descrica_sexo" = "valor") 
      dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], 
                                 dicionario, by="sexo")
    }
    if(name %in% c("brasil_por_faixa_etaria", "uf_por_faixa_etaria")){
      
      dicionario <- gera_dicionario_com_chave_valor(dicionario_rais, "faixa_etaria") %>%
        rename("faixa_etaria" = "chave", "descricao_faixa_etaria" = "valor") 
      dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], 
                                 dicionario, by="faixa_etaria")
    }
    if(name %in% c("brasil_por_deficiencia", "regiao_por_deficiencia", "uf_por_deficiencia")){
      dicionario <- gera_dicionario_com_chave_valor(dicionario_rais, "indicador_portador_deficiencia") %>%
        rename("indicador_portador_deficiencia" = "chave", "descricao_indicador_portador_deficiencia" = "valor") 
      dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], 
                                 dicionario, by="indicador_portador_deficiencia")
    }
    
    if(name %in% c("brasil_por_tipo_deficiencia", "regiao_por_tipo_deficiencia")){
      dicionario <- gera_dicionario_com_chave_valor(dicionario_rais, "tipo_deficiencia") %>%
        rename("tipo_deficiencia" = "chave", "descricao_tipo_deficiencia" = "valor") 
      dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], 
                                 dicionario, by="tipo_deficiencia")
    }
    
    if(name %in% c("brasil_sexo_raca", "regiao_sexo_raca")){
      dicionario <- gera_dicionario_com_chave_valor(dicionario_rais, "sexo") %>%
        rename("sexo" = "chave", "descricao_sexo" = "valor") 
      dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], 
                                 dicionario, by="sexo")

      dicionario <- gera_dicionario_com_chave_valor(dicionario_rais, "raca_cor") %>%
        rename("raca_cor" = "chave", "descricao_raca_cor" = "valor") 
      
      dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], 
                                 dicionario, by="raca_cor")
    }
    
    if(name %in% c("brasil_sexo_escolaridade", "regiao_sexo_escolaridade")){
      dicionario <- gera_dicionario_com_chave_valor(dicionario_rais, "sexo") %>%
        rename("sexo" = "chave", "descricao_sexo" = "valor") 
      dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], 
                                 dicionario, by="sexo")
      
      dicionario <- gera_dicionario_com_chave_valor(dicionario_rais, "grau_instrucao_apos_2005") %>%
        rename("grau_instrucao_apos_2005" = "chave", "descricao_grau_instrucao_apos_2005" = "valor") 
      dfs_vinculos[[name]] <- merge(dfs_vinculos[[name]], 
                                 dicionario, by="grau_instrucao_apos_2005")
    }
    
  }
  return(dfs_vinculos)
}


gera_dicionario_com_chave_valor <- function(dicionario_rais, field_to_filter){
  nome_coluna <- paste("descricao", field_to_filter, sep="_")
  
  dicionario <- dicionario_rais %>% dplyr::filter(nome_coluna == field_to_filter) %>% select(chave, valor)
  return(dicionario)
}


df <- merge(df, dicionario_cnaes, by="cnae_2_subclasse")



# Dicionário RAIS
read_csv(file = file.path(getwd(), csv_estabelecimentos),
         show_col_types = FALSE)

 colunas_alvo <- c("raca_cor",
                   "grau_instrucao_apos_2005",
                   "sexo",
                   "faixa_etaria",
                   "indicador_portador_deficiencia",
                   "tipo_deficiencia")

dicionario_rais <- read_csv(file = file.path(getwd(), "dicionario_rais.csv"), show_col_types = FALSE) %>% 
  dplyr::filter(id_tabela == "microdados_vinculos" & nome_coluna %in% colunas_alvo) %>%
  select(nome_coluna, chave, valor)
  
  
  
  select(5,6) %>%
  rename("cnae_2_subclasse" = 1, "descricao_cnae" = 2) %>%
  na.omit()

df <- merge(df, dicionario_cnaes, by="cnae_2_subclasse")

dplyr::filter(ano == 2019 & indicador_atividade_ano == 1) %>%
  select(ano, sigla_uf, id_municipio,
         quantidade_vinculos_ativos, 
         natureza_juridica, tamanho, cnae_2_subclasse
  ) 

