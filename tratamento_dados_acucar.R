# Tratamento de dados da cadeia sucroalcoleira

# Agregação de dados a nível nacional correlacionado renda com:
# sexo, faixa etária, raça, e escolaridade

library(readxl)
library(writexl)


df_acucar_raw <- read_csv(file = file.path(getwd(), "2019_dados_acucar_rais.csv"),
                      show_col_types = FALSE)

#Seleciona variáveis desejadas e limpa dados de baixa qualidade
df_acucar <- df_acucar_raw %>% 
  select(ano, valor_remuneracao_media, cbo_2002, 
         faixa_etaria, sexo, raca_cor, cnae_2_subclasse, grau_instrucao_apos_2005) %>%
  dplyr::filter(ano == 2019 & valor_remuneracao_media > 0)  

# Agrega nas variáveis desejadas

acucar_sexo <- agrega_por_cnae_e_var_brasil(df_acucar, sexo)
acucar_faixa_etaria <- agrega_por_cnae_e_var_brasil(df_acucar, faixa_etaria)
acucar_etnia <- agrega_por_cnae_e_var_brasil(df_acucar, raca_cor)
acucar_escolaridade <- agrega_por_cnae_e_var_brasil(df_acucar, grau_instrucao_apos_2005)

# Exporta dados para xlsx

sheets <- list(
               "raca" = acucar_etnia,
               "escolaridade" = acucar_escolaridade,
               "sexo" = acucar_sexo,
               "faixa_etaria" = acucar_faixa_etaria
               )
write_xlsx(sheets, file.path(getwd(), "dados_acucar.xlsx"))
