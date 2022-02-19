### Limpeza dos dados ###

library(readxl)
library(openxlsx)
library(tidyverse)
library(magrittr, include.only = '%>%')
library(tibble)
  
rais_direto_vinculos <- read_csv(file = file.path(getwd(), "2019_rais_microdados_vinculos_direto.csv"),
                                   show_col_types = FALSE)
  
rais_direto_estabele <- read_csv(file = file.path(getwd(), "2019_rais_microdados_estabelecimentos_direto.csv"),
                                   show_col_types = FALSE)
  
rais_indireto_vinculos <- read_csv(file = file.path(getwd(), "2019_rais_microdados_vinculos_indireto.csv"),
                                   show_col_types = FALSE)
  
rais_indireto_estabele <- read_csv(file = file.path(getwd(), "2019_rais_microdados_estabelecimentos_indireto.csv"),
                                   show_col_types = FALSE) %>%
                            rename(tamanho = tamanho_estabelecimento)

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


# Agrupamento por região
regiao_direto_geral <- direto_vinculos_tratado %>%
  group_by(regiao) %>% 
  summarise(numero_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 

regiao_indireto_geral <- indireto_vinculos_tratado %>%
  group_by(regiao) %>% 
  summarise(numero_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 

# Agrupamento por UF
uf_direto_geral <- direto_vinculos_tratado %>%
  group_by(sigla_uf) %>% 
  summarise(numero_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 

uf_indireto_geral <- indireto_vinculos_tratado %>%
  group_by(sigla_uf) %>% 
  summarise(numero_trabalhadores = n(), media_salarial = mean(valor_remuneracao_media)) 


# TABELA 2: Visualização por região geográfica 

brasil_dct_por_regiao <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, regiao)
brasil_idct_por_regiao <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, regiao)

# DEMAIS TABELAS
# Raça agregado a nível Brasil

brasil_dct_por_raca <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, raca_cor)
brasil_idct_por_raca <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, raca_cor)

# Raça agregado a nível UF

uf_dct_por_raca <- agrega_por_cnae_e_var_uf(direto_vinculos_tratado, raca_cor)
uf_idct_por_raca <- agrega_por_cnae_e_var_uf(indireto_vinculos_tratado, raca_cor)

# Escolaridade: Brasil

brasil_dct_por_escolaridade <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, grau_instrucao_apos_2005)
brasil_idct_por_escolaridade <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, grau_instrucao_apos_2005)

# Escolaridade: UF

uf_dct_por_escolaridade <- agrega_por_cnae_e_var_uf(direto_vinculos_tratado, grau_instrucao_apos_2005)
uf_idct_por_escolaridade <- agrega_por_cnae_e_var_uf(indireto_vinculos_tratado, grau_instrucao_apos_2005)

# Sexo: Brasil

brasil_dct_por_sexo <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, sexo)
brasil_idct_por_sexo <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, sexo)

# Sexo: UF

uf_dct_por_sexo <- agrega_por_cnae_e_var_uf(direto_vinculos_tratado, sexo)
uf_idct_por_sexo <- agrega_por_cnae_e_var_uf(indireto_vinculos_tratado, sexo)

# Faixa etária: Brasil

brasil_dct_por_faixa_etaria <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, faixa_etaria)
brasil_idct_por_faixa_etaria <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, faixa_etaria)

# Faixa etária: UF

uf_dct_por_faixa_etaria <- agrega_por_cnae_e_var_uf(direto_vinculos_tratado, faixa_etaria)
uf_idct_por_faixa_etaria <- agrega_por_cnae_e_var_uf(indireto_vinculos_tratado, faixa_etaria)

# Portador deficiência: Brasil

brasil_dct_por_deficiencia <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, indicador_portador_deficiencia)
brasil_idct_por_deficiencia <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, indicador_portador_deficiencia)

# Portador deficiência: Região

regiao_dct_por_deficiencia  <- agrega_por_cnae_e_var_regiao(direto_vinculos_tratado, indicador_portador_deficiencia)
regiao_idct_por_deficiencia  <- agrega_por_cnae_e_var_regiao(indireto_vinculos_tratado, indicador_portador_deficiencia)

# Portador deficiência: UF

uf_dct_por_deficiencia  <- agrega_por_cnae_e_var_uf(direto_vinculos_tratado, indicador_portador_deficiencia)
uf_idct_por_deficiencia  <- agrega_por_cnae_e_var_uf(indireto_vinculos_tratado, indicador_portador_deficiencia)

# Tipo de deficiência: Brasil

brasil_dct_por_tipo_deficiencia <- agrega_por_cnae_e_var_brasil(
  dplyr::filter(direto_vinculos_tratado, tipo_deficiencia>0), tipo_deficiencia)
brasil_idct_por_tipo_deficiencia <- agrega_por_cnae_e_var_brasil(
  dplyr::filter(indireto_vinculos_tratado, tipo_deficiencia>0), tipo_deficiencia)

# Tipo de deficiência: Região

regiao_dct_por_tipo_deficiencia <- agrega_por_cnae_e_var_regiao(
  dplyr::filter(direto_vinculos_tratado, tipo_deficiencia>0), tipo_deficiencia)
regiao_idct_por_tipo_deficiencia <- agrega_por_cnae_e_var_regiao(
  dplyr::filter(indireto_vinculos_tratado, tipo_deficiencia>0), tipo_deficiencia)

# Sexo e Raça: Brasil

brasil_dct_sexo_raca <- agregacao_dupla_por_cnae_brasil(direto_vinculos_tratado, sexo, raca_cor)
brasil_idct_sexo_raca <- agregacao_dupla_por_cnae_brasil(indireto_vinculos_tratado, sexo, raca_cor)

# Sexo e Raça: Região

regiao_dct_sexo_raca <- agregacao_dupla_por_cnae_regiao(direto_vinculos_tratado, sexo, raca_cor)
regiao_idct_sexo_raca <- agregacao_dupla_por_cnae_regiao(indireto_vinculos_tratado, sexo, raca_cor)

# Sexo e Escolaridade: Brasil

brasil_dct_sexo_escolaridade <- agregacao_dupla_por_cnae_brasil(direto_vinculos_tratado, sexo, grau_instrucao_apos_2005)
brasil_idct_sexo_escolaridade <- agregacao_dupla_por_cnae_brasil(indireto_vinculos_tratado, sexo, grau_instrucao_apos_2005)

# Sexo e Escolaridade: Brasil

regiao_dct_sexo_escolaridade <- agregacao_dupla_por_cnae_regiao(direto_vinculos_tratado, sexo, grau_instrucao_apos_2005)
regiao_idct_sexo_escolaridade <- agregacao_dupla_por_cnae_regiao(indireto_vinculos_tratado, sexo, grau_instrucao_apos_2005)


### Gera arquivos xlsx


write.xlsx(list(geral = brasil_direto_geral, 
                regiao = brasil_dct_por_regiao, 
                raca = brasil_dct_por_raca, 
                tipo_deficiencia = brasil_dct_por_tipo_deficiencia,
                sexo_raca = brasil_dct_sexo_raca,
                sexo_escolaridade = brasil_dct_sexo_escolaridade,
                escolaridade = brasil_dct_por_escolaridade,
                sexo = brasil_dct_por_sexo,
                faixa_etaria = brasil_dct_por_faixa_etaria,
                deficiencia = brasil_dct_por_deficiencia),
           file.path(getwd(), "brasil_direto.xlsx"), 
           overwrite = TRUE)

write.xlsx(list(geral = brasil_indireto_geral, 
                regiao = brasil_idct_por_regiao, 
                raca = brasil_idct_por_raca, 
                tipo_deficiencia = brasil_idct_por_tipo_deficiencia,
                sexo_raca = brasil_idct_sexo_raca,
                sexo_escolaridade = brasil_idct_sexo_escolaridade,
                escolaridade = brasil_idct_por_escolaridade,
                sexo = brasil_idct_por_sexo,
                faixa_etaria = brasil_idct_por_faixa_etaria,
                deficiencia = brasil_idct_por_deficiencia),
          file.path(getwd(), "brasil_indireto.xlsx"),
          overwrite = TRUE)

write.xlsx(list(
  geral = uf_direto_geral,
  raca = uf_dct_por_raca,
  escolaridade = uf_dct_por_escolaridade,
  sexo = uf_dct_por_sexo,
  faixa_etaria = uf_dct_por_faixa_etaria,
  deficiencia = uf_dct_por_deficiencia),
  file.path(getwd(), "uf_direto.xlsx"),
  overwrite = TRUE)

write.xlsx(list(
               geral = uf_indireto_geral,
               raca = uf_idct_por_raca,
               escolaridade = uf_idct_por_escolaridade,
               sexo = uf_idct_por_sexo,
               faixa_etaria = uf_idct_por_faixa_etaria,
               deficiencia = uf_idct_por_deficiencia), 
           file.path(getwd(), "uf_indireto.xlsx"), 
           overwrite = TRUE)

write.xlsx(list(
  geral = regiao_direto_geral,
  sexo_escolaridade = regiao_dct_sexo_escolaridade,
  sexo_raca = regiao_dct_sexo_raca,
  tipo_deficiencia = regiao_dct_por_tipo_deficiencia, 
  deficiencia = regiao_dct_por_deficiencia), 
  file.path(getwd(), "regiao_direto.xlsx"), 
  overwrite = TRUE)

write.xlsx(list(
  geral = regiao_indireto_geral,
  sexo_escolaridade = regiao_idct_sexo_escolaridade,
  sexo_raca = regiao_idct_sexo_raca,
  tipo_deficiencia = regiao_idct_por_tipo_deficiencia, 
  deficiencia = regiao_idct_por_deficiencia), 
  file.path(getwd(), "regiao_indireto.xlsx"),
  overwrite = TRUE)


###### Processa dados de Estabelecimentos ####

# Esta é a base que será enviada para a patroa
direto_estabelecimentos_tratado <- rais_direto_estabele %>% 
  dplyr::filter(ano == 2019 & indicador_atividade_ano == 1) %>%
  select(ano, sigla_uf, id_municipio,
         quantidade_vinculos_ativos, 
         natureza_juridica, tamanho, cnae_2_subclasse
         ) 

direto_estabelecimentos_tratado$regiao <- lapply(direto_estabelecimentos_tratado$sigla_uf, 
                                         FUN=uf_para_regiao)


##### Concatena no DF nome da cidade, a partir do código IBGE

codigo_municipio_ibge <- read_excel(path = file.path(getwd(), "RELATORIO_DTB_BRASIL_MUNICIPIO.xls")) %>%
  select("Código Município Completo", Nome_Município) %>%
  rename(id_municipio = "Código Município Completo", municipio = Nome_Município)

direto_estabelecimentos_tratado <- merge(direto_estabelecimentos_tratado, codigo_municipio_ibge, by="id_municipio")

dicionario_cnaes <- read_excel(path = file.path(getwd(), "CNAE20_Subclasses_EstruturaDetalhada.xls")) %>% 
            select(5,6) %>%
            rename("cnae_2_subclasse" = 1, "descricao" = 2) %>%
            na.omit()

dicionario_cnaes$cnae_2_subclasse <- gsub("-", "", dicionario_cnaes$cnae_2_subclasse) 
dicionario_cnaes$cnae_2_subclasse <- gsub("/", "", dicionario_cnaes$cnae_2_subclasse)

direto_estabelecimentos_tratado <- merge(direto_estabelecimentos_tratado, dicionario_cnaes, by="cnae_2_subclasse")

dicionario_tamanho <- read_csv('dicionario_tamanho.csv') %>% rename("tamanho" = "chave", "tamanho_desc" = "valor") %>% select(tamanho, tamanho_desc)
direto_estabelecimentos_tratado <- merge(direto_estabelecimentos_tratado, dicionario_tamanho, by="tamanho")

############

brasil_numero_estabelecimentos_dct <- direto_estabelecimentos_tratado %>%
  group_by(cnae_2_subclasse, descricao) %>% 
  summarise(n_estabelecimentos = n()) 

brasil_numero_vinculos_dct <- direto_estabelecimentos_tratado %>%
  group_by(cnae_2_subclasse, descricao) %>% 
  summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 

brasil_numero_estabelecimentos_tamanho_dct <- direto_estabelecimentos_tratado %>%
  group_by(cnae_2_subclasse, descricao, tamanho, tamanho_desc) %>% 
  summarise(n_estabelecimentos = n()) 

regiao_numero_estabelecimentos_dct <- direto_estabelecimentos_tratado %>%
  group_by(regiao, cnae_2_subclasse, descricao) %>% 
  summarise(n_estabelecimentos = n()) 

regiao_numero_vinculos_dct <- direto_estabelecimentos_tratado %>%
  group_by(regiao, cnae_2_subclasse, descricao) %>% 
  summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 

regiao_numero_estabelecimentos_tamanho_dct <- direto_estabelecimentos_tratado %>%
  group_by(regiao, cnae_2_subclasse, descricao, tamanho, tamanho_desc) %>% 
  summarise(n_estabelecimentos = n()) 

uf_numero_estabelecimentos_dct <- direto_estabelecimentos_tratado %>%
  group_by(sigla_uf, cnae_2_subclasse, descricao) %>% 
  summarise(n_estabelecimentos = n()) 

uf_numero_vinculos_dct <- direto_estabelecimentos_tratado %>%
  group_by(sigla_uf, cnae_2_subclasse, descricao) %>% 
  summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 

uf_numero_estabelecimentos_tamanho_dct <- direto_estabelecimentos_tratado %>%
  group_by(sigla_uf, cnae_2_subclasse, descricao, tamanho, tamanho_desc) %>% 
  summarise(n_estabelecimentos = n()) 

############

indireto_estabelecimentos_tratado <- rais_indireto_estabele %>% 
  dplyr::filter(ano == 2019 & indicador_atividade_ano == 1) %>%
  select(ano, sigla_uf, id_municipio,
         quantidade_vinculos_ativos, 
         natureza_juridica, tamanho, cnae_2_subclasse
  ) 

indireto_estabelecimentos_tratado$regiao <- lapply(indireto_estabelecimentos_tratado$sigla_uf, 
                                                 FUN=uf_para_regiao)

indireto_estabelecimentos_tratado <- merge(indireto_estabelecimentos_tratado, codigo_municipio_ibge, by="id_municipio")

indireto_estabelecimentos_tratado <- merge(indireto_estabelecimentos_tratado, dicionario_cnaes, by="cnae_2_subclasse")

indireto_estabelecimentos_tratado <- merge(indireto_estabelecimentos_tratado, dicionario_tamanho, by="tamanho")


brasil_numero_estabelecimentos_idct <- indireto_estabelecimentos_tratado %>%
  group_by(cnae_2_subclasse, descricao) %>% 
  summarise(n_estabelecimentos = n()) 

brasil_numero_vinculos_idct <- indireto_estabelecimentos_tratado %>%
  group_by(cnae_2_subclasse, descricao) %>% 
  summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 

brasil_numero_estabelecimentos_tamanho_idct <- indireto_estabelecimentos_tratado %>%
  group_by(cnae_2_subclasse, descricao, tamanho, tamanho_desc) %>% 
  summarise(n_estabelecimentos = n()) 

regiao_numero_estabelecimentos_idct <- indireto_estabelecimentos_tratado %>%
  group_by(regiao, cnae_2_subclasse, descricao) %>% 
  summarise(n_estabelecimentos = n()) 

regiao_numero_vinculos_idct <- indireto_estabelecimentos_tratado %>%
  group_by(regiao, cnae_2_subclasse, descricao) %>% 
  summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 

regiao_numero_estabelecimentos_tamanho_idct <- indireto_estabelecimentos_tratado %>%
  group_by(regiao, cnae_2_subclasse, descricao, tamanho, tamanho_desc) %>% 
  summarise(n_estabelecimentos = n()) 

uf_numero_estabelecimentos_idct <- indireto_estabelecimentos_tratado %>%
  group_by(sigla_uf, cnae_2_subclasse, descricao) %>% 
  summarise(n_estabelecimentos = n()) 

uf_numero_vinculos_idct <- indireto_estabelecimentos_tratado %>%
  group_by(sigla_uf, cnae_2_subclasse, descricao) %>% 
  summarise(quantidade_vinculos_ativos = sum(quantidade_vinculos_ativos)) 

uf_numero_estabelecimentos_tamanho_idct <- indireto_estabelecimentos_tratado %>%
  group_by(sigla_uf, cnae_2_subclasse, descricao, tamanho, tamanho_desc) %>% 
  summarise(n_estabelecimentos = n()) 

######

write.xlsx(list(rais = direto_estabelecimentos_tratado
),
file.path(getwd(), "estabelecimento_direto_dado_bruto.xlsx"), 
overwrite = TRUE)

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
           file.path(getwd(), "estabelecimento_direto.xlsx"), 
           overwrite = TRUE)

write.xlsx(list(brasil_estab = brasil_numero_estabelecimentos_idct, 
                brasil_vinc = brasil_numero_vinculos_idct, 
                brasil_estab_tamanho = brasil_numero_estabelecimentos_tamanho_idct,
                
                regiao_estab = regiao_numero_estabelecimentos_idct, 
                regiao_vinc = regiao_numero_vinculos_idct, 
                regiao_estab_tamanho = regiao_numero_estabelecimentos_tamanho_idct,
                
                uf_estab = uf_numero_estabelecimentos_idct, 
                uf_vinc = uf_numero_vinculos_idct, 
                uf_estab_tamanho = uf_numero_estabelecimentos_tamanho_idct
          ),
        file.path(getwd(), "estabelecimento_indireto.xlsx"), 
        overwrite = TRUE)


write.xlsx(list(rais = indireto_estabelecimentos_tratado),
        file.path(getwd(), "estabelecimento_indireto_dado_bruto.xlsx"), 
        overwrite = TRUE)

# Isso é tudo pessoal:

beepr::beep(sound=3)