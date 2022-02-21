library(readxl)
library(openxlsx)
library(tidyverse)
library(magrittr, include.only = '%>%')
library(tibble)


rais_vinculos <- extract_vinculos("biogas_dados_vinculos_2019.csv")

rais_estabelecimentos <- extract_estabelecimentos("biogas_estabelecimentos_2019.csv")

dfs_estabelecimentos <- generate_estabelecimentos_multiple_aggregation_levels(rais_estabelecimentos)

dfs_vinculos <- generate_vinculos_multiple_aggregation_levels(rais_vinculos)

dfs_vinculos_descricao_completa <- add_colunas_descricao_dicionario(dfs_vinculos)

write.xlsx(list(brasil_estab = dfs_estabelecimentos$brasil_n_estabelecimentos, 
                brasil_vinc = dfs_estabelecimentos$brasil_n_vinculos, 
                brasil_estab_tamanho = dfs_estabelecimentos$brasil_n_estabelecimentos_tamanho,
                
                regiao_estab = dfs_estabelecimentos$regiao_n_estabelecimentos, 
                regiao_vinc = dfs_estabelecimentos$regiao_n_vinculos, 
                regiao_estab_tamanho = dfs_estabelecimentos$regiao_n_estabelecimentos_tamanho,
                
                uf_estab = dfs_estabelecimentos$uf_n_estabelecimentos, 
                uf_vinc = dfs_estabelecimentos$uf_n_vinculos, 
                uf_estab_tamanho = dfs_estabelecimentos$uf_n_estabelecimentos_tamanho
                ),
                file.path(getwd(), "dados_biogas_estabelecimentos_final.xlsx"), 
                overwrite = TRUE)


write.xlsx(list(geral = dfs_vinculos_descricao_completa$brasil_geral, 
                # regiao = brasil_dct_por_regiao, 
                raca = dfs_vinculos_descricao_completa$brasil_por_raca, 
                tipo_deficiencia = dfs_vinculos_descricao_completa$brasil_por_tipo_deficiencia,
                
                sexo_raca = dfs_vinculos_descricao_completa$brasil_sexo_raca,
                sexo_raca2 = dfs_vinculos_descricao_completa$brasil_sexo_raca2,
                sexo_escolaridade = dfs_vinculos_descricao_completa$brasil_sexo_escolaridade,
                sexo_escolaridade2 = dfs_vinculos_descricao_completa$brasil_por_escolaridade2,
                escolaridade = dfs_vinculos_descricao_completa$brasil_por_escolaridade,
                escolaridade2 = dfs_vinculos_descricao_completa$brasil_por_escolaridade2,
                
                sexo = dfs_vinculos_descricao_completa$brasil_por_sexo,
                faixa_etaria = dfs_vinculos_descricao_completa$brasil_por_faixa_etaria,
                deficiencia = dfs_vinculos_descricao_completa$brasil_por_deficiencia),
           file.path(getwd(), "brasil_biogas_vinculos.xlsx"), 
           overwrite = TRUE)

write.xlsx(list(
  geral = dfs_vinculos_descricao_completa$uf_geral,
  raca = dfs_vinculos_descricao_completa$uf_por_raca,
  escolaridade = dfs_vinculos_descricao_completa$uf_por_escolaridade,
  escolaridade2 = dfs_vinculos_descricao_completa$uf_por_escolaridade2,
  sexo = dfs_vinculos_descricao_completa$uf_por_sexo,
  faixa_etaria = dfs_vinculos_descricao_completa$uf_por_faixa_etaria,
  deficiencia = dfs_vinculos_descricao_completa$uf_por_deficiencia),
  file.path(getwd(), "uf_biogas_vinculos.xlsx"),
  overwrite = TRUE)

write.xlsx(list(
  geral = dfs_vinculos_descricao_completa$regiao_geral,
  sexo_escolaridade = dfs_vinculos_descricao_completa$regiao_sexo_escolaridade,
  sexo_raca = dfs_vinculos_descricao_completa$regiao_sexo_raca,
  sexo_escolaridade2 = dfs_vinculos_descricao_completa$regiao_sexo_escolaridade2,
  sexo_raca2 = dfs_vinculos_descricao_completa$regiao_sexo_raca2,
  tipo_deficiencia = dfs_vinculos_descricao_completa$regiao_por_tipo_deficiencia, 
  deficiencia = dfs_vinculos_descricao_completa$regiao_por_deficiencia), 
  file.path(getwd(), "regiao_biogas_vinculos.xlsx"), 
  overwrite = TRUE)





