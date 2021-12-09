library("basedosdados")
library("tibble")
library(dplyr)
library(magrittr, include.only = "%>%")


basedosdados::set_billing_id('bep-consultoria')
# bigrquery::bq_auth(email = "vitorf.loures@gmail.com", cache = NULL, use_oob = NULL)
# bigrquery::bq_auth('vitor/home/Downloads/bep-consultoria-df9d04e696eb.json')


# Query Base dos Dados em SQL
query <- 'SELECT * FROM `basedosdados.br_bd_diretorios_brasil.municipio` LIMIT 10'
# dados_rais <- basedosdados::bdplyr('br_me_rais.microdados_vinculos')
read_sql(query = query)


### ANÁLISE RAIS ###

dados_rais <- basedosdados::bdplyr('br_me_rais.microdados_vinculos')


tibble::glimpse(dados_rais)

tictoc::tic()
tibble::view(dados_rais)
# tibble::glimpse(dados_rais_por_ano_e_cnae)
beepr::beep(sound=5)
tictoc::toc()




dados_rais_2019 <- basedosdados::bd_collect(dados_rais_por_ano_header)

filter(ano==2019)


cnaes_formal <- c("3822100", "3821100", "3811400", "3812200", "3839499", "2825900")



  
dados_rais_por_ano_e_cnae <- dados_rais_por_ano %>% filter(cnae_2_subclasse %in% cnaes_formal)

tibble::view(dados_rais_por_ano_e_cnae)
dados_rais_por_ano <- dados_rais %>%
  dplyr::filter(ano==2019)
view(dados_rais_por_ano)




# # Script to Costelinha


# dados_rais <- basedosdados::bdplyr('br_me_rais.microdados_vinculos')
# 
# tictoc::tic()
# tibble::view(dados_rais)
# beepr::beep(sound=5)
# tictoc::toc()
# 
# ### arrumando os dados
# 
# list_variaveis <- list('tipo_vinculo', 'valor_remuneracao_media_nominal',
#                   'valor_salario_contratual', 'valor_salario_contratual', 
#                   'faixa_etaria', 'idade,','grau_instrucao_apos_2005',
#                   'nacionalidade', 'sexo', 'raca_cor', 
#                   'indicador_portador_deficiencia', 'tipo_deficiencia', 
#                   'ano_chegada_brasil', 'tamanho_estabelecimento', 
#                   'tipo_estabelecimento', 'tamanho_estabelecimento', 
#                   'tipo_estabelecimento', 'natureza_juridica') 
#                   
#            
# dados_rais_arrumados <- dados_rais %>% 
#                        dplyr::filter(ano==2019)
# 
# tictoc::tic()
# tibble::view(dados_rais_arrumados)
# #tibble::glimpse(dados_rais_arrumados)
# beepr::beep(sound=5)
# tictoc::toc()
# 
# ##Importação dos dados
# 
# tictoc::tic()
# dados_rais_2019 <- basedosdados::bd_collect(dados_rais_arrumados)
# readxl::write_csv(dados_rais_arrumados, file = "dados_rais_2019.csv")
# beepr::beep(sound=7)
# tictoc::toc()
# 
# 
# ##CNAE emprego formal = '3822100', '3821100','3811400',
# ##                  '3812200', '3839499', '2825900'

