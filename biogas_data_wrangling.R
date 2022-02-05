### Limpeza dos dados ###

library(readxl)
library(openxlsx)
library(tidyverse)
library(magrittr, include.only = '%>%')
library(tibble)
source("data_wrangling_income.R")

get_grupo <- function(cnae) {
  grupo_A <- c('0151202', '0154700', '0155505', '1011201', '1012101', '1012103', '1051100', '1052000', 
               '1071600', '1931400', '3811400', '3821100', '3839499')
  grupo_B <- c('2832100')
  grupo_C <- c('1354500', '2221800', '2731700', '2833000', '3314710', '3321000', '4313400', '4322301', 
               '4399103')
  grupo_D <- c('2710401', '3313901', '3511501', '3520401')

  if (cnae %in% grupo_A) {
    nome_grupo <- "Resíduos"
  }
  else if (cnae %in% grupo_B) {
    nome_grupo <- "Separação"
  }
  if (cnae %in% grupo_C) {
    nome_grupo <- "Biodigestão"
  }
  if (cnae %in% grupo_D) {
    nome_grupo <- "Energia e Biometano"
  }
  
  return (nome_grupo)
}


uf_para_regiao(uf) -> regiao
agrega_por_cnae_e_var_brasi(input_df, var) -> output_df
agrega_por_cnae_e_var_uf(input_df, var) -> output_df
agrega_por_cnae_e_var_regiao(input_df, var) -> output_df
agregacao_dupla_por_cnae_brasil(input_df, var1, var2) -> output_df
agregacao_dupla_por_cnae_regiao(input_df, var1, var2) -> output_df


biogas_rais_vinculos <- read_csv(file = file.path(getwd(), "biogas_dados_vinculos_2019.csv"),
                                 show_col_types = FALSE) %>% 
  select(ano, sigla_uf, id_municipio, valor_remuneracao_media, cbo_2002, 
         idade, faixa_etaria, sexo, raca_cor, nacionalidade, indicador_portador_deficiencia, tipo_deficiencia,
         cnae_2_subclasse, grau_instrucao_apos_2005) %>%
  dplyr::filter(ano == 2019 & valor_remuneracao_media > 0)  
biogas_rais_vinculos$regiao <- lapply(biogas_rais_vinculos$sigla_uf, 
                                         FUN=uf_para_regiao)
biogas_rais_vinculos$nome_grupo <- lapply(biogas_rais_vinculos$cnae_2_subclasse, 
                                      FUN=get_grupo)

biogas_rais_estabele <- read_csv(file = file.path(getwd(), "biogas_estabelecimentos_2019.csv"),
                                 show_col_types = FALSE)  %>% 
  dplyr::filter(ano == 2019 & indicador_atividade_ano == 1) %>%
  select(ano, sigla_uf, id_municipio,
         quantidade_vinculos_ativos, 
         natureza_juridica, tamanho, cnae_2_subclasse
  ) 
biogas_rais_estabele$regiao <- lapply(biogas_rais_estabele$sigla_uf, 
                                                 FUN=uf_para_regiao)
biogas_rais_estabele$nome_grupo <- lapply(biogas_rais_estabele$cnae_2_subclasse, 
                                          FUN=get_grupo)


# TABELA 1: Total de trabalhadores e média salarial por CNAE
brasil_geral <- biogas_rais_vinculos %>%
  group_by(cnae_2_subclasse) %>% 
  summarise(numero_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 

# Agrupamento por região
regiao_geral <- biogas_rais_vinculos %>%
  group_by(regiao) %>% 
  summarise(numero_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 

# Agrupamento por UF
uf_geral <- biogas_rais_vinculos %>%
  group_by(sigla_uf) %>% 
  summarise(numero_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 

# TABELA 2: Visualização por região geográfica 

brasil_dct_por_regiao <- agrega_por_cnae_e_var_brasil(biogas_rais_vinculos, regiao)

# DEMAIS TABELAS
# Raça agregado a nível Brasil

brasil_dct_por_raca <- agrega_por_cnae_e_var_brasil(biogas_rais_vinculos, raca_cor)
# Raça agregado a nível UF

uf_dct_por_raca <- agrega_por_cnae_e_var_uf(biogas_rais_vinculos, raca_cor)

# Escolaridade: Brasil

brasil_dct_por_escolaridade <- agrega_por_cnae_e_var_brasil(biogas_rais_vinculos, grau_instrucao_apos_2005)

# Escolaridade: UF

uf_dct_por_escolaridade <- agrega_por_cnae_e_var_uf(biogas_rais_vinculos, grau_instrucao_apos_2005)

# Sexo: Brasil

brasil_dct_por_sexo <- agrega_por_cnae_e_var_brasil(biogas_rais_vinculos, sexo)

# Sexo: UF

uf_dct_por_sexo <- agrega_por_cnae_e_var_uf(biogas_rais_vinculos, sexo)

# Faixa etária: Brasil

brasil_dct_por_faixa_etaria <- agrega_por_cnae_e_var_brasil(biogas_rais_vinculos, faixa_etaria)

# Faixa etária: UF

uf_dct_por_faixa_etaria <- agrega_por_cnae_e_var_uf(biogas_rais_vinculos, faixa_etaria)

# Portador deficiência: Brasil

brasil_dct_por_deficiencia <- agrega_por_cnae_e_var_brasil(biogas_rais_vinculos, indicador_portador_deficiencia)

# Portador deficiência: Região

regiao_dct_por_deficiencia  <- agrega_por_cnae_e_var_regiao(biogas_rais_vinculos, indicador_portador_deficiencia)

# Portador deficiência: UF

uf_dct_por_deficiencia  <- agrega_por_cnae_e_var_uf(biogas_rais_vinculos, indicador_portador_deficiencia)

# Tipo de deficiência: Brasil

brasil_dct_por_tipo_deficiencia <- agrega_por_cnae_e_var_brasil(
  dplyr::filter(biogas_rais_vinculos, tipo_deficiencia>0), tipo_deficiencia)

# Tipo de deficiência: Região

regiao_dct_por_tipo_deficiencia <- agrega_por_cnae_e_var_regiao(
  dplyr::filter(biogas_rais_vinculos, tipo_deficiencia>0), tipo_deficiencia)

# Sexo e Raça: Brasil

brasil_dct_sexo_raca <- agregacao_dupla_por_cnae_brasil(biogas_rais_vinculos, sexo, raca_cor)

# Sexo e Raça: Região

regiao_dct_sexo_raca <- agregacao_dupla_por_cnae_regiao(biogas_rais_vinculos, sexo, raca_cor)

# Sexo e Escolaridade: Brasil

brasil_dct_sexo_escolaridade <- agregacao_dupla_por_cnae_brasil(biogas_rais_vinculos, sexo, grau_instrucao_apos_2005)

# Sexo e Escolaridade: Brasil

regiao_dct_sexo_escolaridade <- agregacao_dupla_por_cnae_regiao(biogas_rais_vinculos, sexo, grau_instrucao_apos_2005)

### Gera arquivos xlsx


write.xlsx(list(geral = brasil_geral, 
                regiao = brasil_dct_por_regiao, 
                raca = brasil_dct_por_raca, 
                tipo_deficiencia = brasil_dct_por_tipo_deficiencia,
                sexo_raca = brasil_dct_sexo_raca,
                sexo_escolaridade = brasil_dct_sexo_escolaridade,
                escolaridade = brasil_dct_por_escolaridade,
                sexo = brasil_dct_por_sexo,
                faixa_etaria = brasil_dct_por_faixa_etaria,
                deficiencia = brasil_dct_por_deficiencia),
           file.path(getwd(), "brasil_biogas_biometano.xlsx"), 
           overwrite = TRUE)


write.xlsx(list(
  geral = uf_geral,
  raca = uf_dct_por_raca,
  escolaridade = uf_dct_por_escolaridade,
  sexo = uf_dct_por_sexo,
  faixa_etaria = uf_dct_por_faixa_etaria,
  deficiencia = uf_dct_por_deficiencia),
  file.path(getwd(), "uf_biogas_biometano.xlsx"),
  overwrite = TRUE)


write.xlsx(list(
  geral = regiao_geral,
  sexo_escolaridade = regiao_dct_sexo_escolaridade,
  sexo_raca = regiao_dct_sexo_raca,
  tipo_deficiencia = regiao_dct_por_tipo_deficiencia, 
  deficiencia = regiao_dct_por_deficiencia), 
  file.path(getwd(), "regiao_biogas_biometano.xlsx"), 
  overwrite = TRUE)





###### Processa dados de Estabelecimentos ####

##### Concatena no DF nome da cidade, a partir do código IBGE

codigo_municipio_ibge <- read_excel(path = file.path(getwd(), "RELATORIO_DTB_BRASIL_MUNICIPIO.xls")) %>%
  select("Código Município Completo", Nome_Município) %>%
  rename(id_municipio = "Código Município Completo", municipio = Nome_Município)

biogas_rais_estabele <- merge(biogas_rais_estabele, codigo_municipio_ibge, by="id_municipio")

dicionario_cnaes <- read_excel(path = file.path(getwd(), "CNAE20_Subclasses_EstruturaDetalhada.xls")) %>% 
  select(5,6) %>%
  rename("cnae_2_subclasse" = 1, "descricao" = 2) %>%
  na.omit()

dicionario_cnaes$cnae_2_subclasse <- gsub("-", "", dicionario_cnaes$cnae_2_subclasse) 
dicionario_cnaes$cnae_2_subclasse <- gsub("/", "", dicionario_cnaes$cnae_2_subclasse)

biogas_rais_estabele <- merge(biogas_rais_estabele, dicionario_cnaes, by="cnae_2_subclasse")

dicionario_tamanho <- read_csv('dicionario_tamanho.csv') %>% rename("tamanho" = "chave", "tamanho_desc" = "valor") %>% select(tamanho, tamanho_desc)
biogas_rais_estabele <- merge(biogas_rais_estabele, dicionario_tamanho, by="tamanho")

############

brasil_numero_estabelecimentos_dct <- biogas_rais_estabele %>%
  group_by(cnae_2_subclasse, descricao) %>% 
  summarise(n_estabelecimentos = n()) 

brasil_numero_vinculos_dct <- biogas_rais_estabele %>%
  group_by(cnae_2_subclasse, descricao) %>% 
  summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 

brasil_numero_estabelecimentos_tamanho_dct <- biogas_rais_estabele %>%
  group_by(cnae_2_subclasse, descricao, tamanho, tamanho_desc) %>% 
  summarise(n_estabelecimentos = n()) 

regiao_numero_estabelecimentos_dct <- biogas_rais_estabele %>%
  group_by(regiao, cnae_2_subclasse, descricao) %>% 
  summarise(n_estabelecimentos = n()) 

regiao_numero_vinculos_dct <- biogas_rais_estabele %>%
  group_by(regiao, cnae_2_subclasse, descricao) %>% 
  summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 

regiao_numero_estabelecimentos_tamanho_dct <- biogas_rais_estabele %>%
  group_by(regiao, cnae_2_subclasse, descricao, tamanho, tamanho_desc) %>% 
  summarise(n_estabelecimentos = n()) 

uf_numero_estabelecimentos_dct <- biogas_rais_estabele %>%
  group_by(sigla_uf, cnae_2_subclasse, descricao) %>% 
  summarise(n_estabelecimentos = n()) 

uf_numero_vinculos_dct <- biogas_rais_estabele %>%
  group_by(sigla_uf, cnae_2_subclasse, descricao) %>% 
  summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 

uf_numero_estabelecimentos_tamanho_dct <- biogas_rais_estabele %>%
  group_by(sigla_uf, cnae_2_subclasse, descricao, tamanho, tamanho_desc) %>% 
  summarise(n_estabelecimentos = n()) 


write.xlsx(list(brasil_estab = brasil_numero_estabelecimentos_dct, 
                brasil_vinc = brasil_numero_vinculos_dct, 
                brasil_estab_tamanho = brasil_numero_estabelecimentos_tamanho_dct,
                
                regiao_estab = regiao_numero_estabelecimentos_dct, 
                regiao_vinc = regiao_numero_vinculos_dct, 
                regiao_estab_tamanho = regiao_numero_estabelecimentos_tamanho_dct,
                
                uf_estab = uf_numero_estabelecimentos_dct, 
                uf_vinc = uf_numero_vinculos_dct, 
                uf_estab_tamanho = uf_numero_estabelecimentos_tamanho_dct
),
file.path(getwd(), "estabelecimento_biogas_biometano.xlsx"), 
overwrite = TRUE)


# Isso é tudo pessoal:

beepr::beep(sound=3)