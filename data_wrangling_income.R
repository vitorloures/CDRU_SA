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
      output_df <- input_df %>%
      group_by(cnae_2_subclasse, {{var}}) %>%
      summarise(n_trabalhadores = n(), 
                mean = mean(valor_remuneracao_media), 
                std = sd(valor_remuneracao_media) / mean(valor_remuneracao_media))  %>% 
      group_by(cnae_2_subclasse) %>% 
      mutate(count_cnae = sum(count)) %>% 
      group_by({{var}}) %>% 
      mutate(count_per=round(100*count/count_cnae,2))
  
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
  select(ano, sigla_uf, id_municipio, id_municipio_6_trabalho, valor_remuneracao_media_nominal, cbo_2002, 
         idade, sexo, raca_cor, nacionalidade, indicador_portador_deficiencia, cnae_2_subclasse,
         grau_instrucao_1985_2005, grau_instrucao_apos_2005) %>%
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


# TABELA 2: Visualização por região geográfica 

df_dir_por_regiao <- direto_vinculos_tratado %>%
  group_by(cnae_2_subclasse, regiao) %>%
  summarise(count = n(), 
            mean = mean(valor_remuneracao_media), 
            std = sd(valor_remuneracao_media) / mean(valor_remuneracao_media)) 

df_indir_por_regiao <- indireto_vinculos_tratado %>%
  group_by(cnae_2_subclasse, regiao) %>%
  summarise(count = n(), 
            mean = mean(valor_remuneracao_media), 
            std = sd(valor_remuneracao_media) / mean(valor_remuneracao_media)) 

# Add percentage column
df_dir_regiao_percentual <- df_dir_por_regiao %>% 
  group_by(cnae_2_subclasse) %>% 
  mutate(count_cnae = sum(count)) %>% 
  group_by(regiao) %>% 
  mutate(count_per=paste0(round(100*count/count_cnae,2), '%'))

df_indir_regiao_percentual <- df_indir_por_regiao %>% 
  group_by(cnae_2_subclasse) %>% 
  mutate(count_cnae = sum(count)) %>% 
  group_by(regiao) %>% 
  mutate(count_per=paste0(round(100*count/count_cnae,2), '%'))

# Raça agregado a nível Brasil

df_dir_por_raca_brasil <- direto_vinculos_tratado %>%
  group_by(cnae_2_subclasse, raca_cor) %>%
  summarise(count = n(), 
            mean = mean(valor_remuneracao_media), 
            std = sd(valor_remuneracao_media) / mean(valor_remuneracao_media))  %>% 
            group_by(cnae_2_subclasse) %>% 
            mutate(count_cnae = sum(count)) %>% 
            group_by(raca_cor) %>% 
            mutate(count_per=paste0(round(100*count/count_cnae,2), '%'))

df_indir_por_raca_brasil <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, raca_cor)

# Raça agregado a nível UF

# Escolaridade: Brasil

df_dir_escolar_br <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, grau_instrucao_apos_2005)
df_indir_escolar_br <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, grau_instrucao_apos_2005)

# Escolaridade: UF

# Sexo: Brasil

df_dir_sexo_br <- agrega_por_cnae_e_var_brasil(direto_vinculos_tratado, sexo)
df_indir_sexo_br <- agrega_por_cnae_e_var_brasil(indireto_vinculos_tratado, sexo)

# Sexo: UF

### Download CSV files

file.path(getwd(), "2019_rais_microdados_vinculos_direto.csv")


write_xlsx(brasil_direto_geral,path =file.path(getwd(), "geral_vinculos_direto_brasil.xlsx"))
write_xlsx(brasil_indireto_geral,path =file.path(getwd(), "geral_vinculos_indireto_brasil.xlsx"))

write_xlsx(df_dir_regiao_percentual,file.path(getwd(),path = "vinculos_direto_regiao.xlsx"))
write_xlsx(df_indir_regiao_percentual,file.path(getwd(),path = "vinculos_indireto_regiao.xlsx"))

write_xlsx(df_dir_por_raca_brasil,file.path(getwd(), path ="vinculos_direto_raca_brasil.xlsx"))
write_xlsx(df_indir_por_raca_brasil,file.path(getwd(), path ="vinculos_indireto_raca_brasil.xlsx"))

write_xlsx(df_dir_escolar_br,file.path(getwd(), path ="vinculos_direto_escolar_brasil.xlsx"))
write_xlsx(df_indir_escolar_br,file.path(getwd(), path ="vinculos_indireto_escolar_brasil.xlsx"))

write_xlsx(df_dir_sexo_br,file.path(getwd(), path ="vinculos_direto_sexo_brasil.xlsx"))
write_xlsx(df_indir_sexo_br,file.path(getwd(), path = "vinculos_indireto_sexo_brasil.xlsx"))



###### Trash below this point


rais_uf_2019 <- rais_direto_vinculos %>% 
  select(-ano, -id_municipio,-vinculo_ativo_3112,-tipo_admissao,
         -mes_desligamento,-mes_admissao,-motivo_desligamento,
         -causa_desligamento_1,
         -causa_desligamento_2,-causa_desligamento_3,
         -id_municipio_6_trabalho,-quantidade_dias_afastamento,
         -quantidade_horas_contratadas,
         -indicador_cei_vinculado,-indicador_trabalho_parcial,
         -indicador_trabalho_intermitente,
         -faixa_remuneracao_dezembro_sm,-valor_remuneracao_dezembro_sm,
         -valor_remuneracao_janeiro_nominal,
         -valor_remuneracao_fevereiro_nominal,
         -valor_remuneracao_marco_nominal,
         -valor_remuneracao_abril_nominal,
         -valor_remuneracao_maio_nominal,
         -valor_remuneracao_junho_nominal,
         -valor_remuneracao_julho_nominal,
         -valor_remuneracao_agosto_nominal,
         -valor_remuneracao_setembro_nominal,
         -valor_remuneracao_outubro_nominal,
         -valor_remuneracao_novembro_nominal,
         -valor_remuneracao_dezembro_nominal,
         -tipo_salario, -valor_salario_contratual, -subatividade_ibge,
         -cbo_1994,-cbo_2002,-subsetor_ibge,-cnae_1,-cnae_2,
         -grau_instrucao_1985_2005,
         -bairros_sp,-bairros_fortaleza,-bairros_rj,-distritos_sp,
         -regioes_administrativas_df,-indicador_simples) %>% 
  tibble::view()


beepr::beep(sound=2)



# Isso é tudo pessoal:
beepr::beep(sound=3)