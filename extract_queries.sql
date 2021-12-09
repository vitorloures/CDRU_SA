--------------------------------------------------------------------------------
------------------------ Queries para extração de dados (RAIS) -----------------
--------------------------------------------------------------------------------

-- Extração de dados de emprego do açúcar
SELECT 
*
FROM `basedosdados.br_me_rais.microdados_vinculos` 
WHERE ano IN (2019)
and cnae_2_subclasse IN ('0113000', '0161003', '1071600',
'1072401', '1931400', '1099603', '2013401', '2013402')

-- Extração PNADC que não funciona: 

SELECT v4013, -- Atividade da empresa
v40132, v40132a, -- Seção da atividade
-- CONCAT(v4013, v40132,v40132a) as cnae_2_subclasse,
v4029, -- Carteira assinada, ou informal
v4033, -- Rendimento bruto mensal
v4039, -- Total de horas
v2007, -- sexo
v2010 -- cor ou raça 
v3001, -- Saber ler ou escrever? 
capital, 
sigla_uf, ano, id_uf, capital 
FROM `basedosdados.br_ibge_pnadc.microdados` 
WHERE ano = 2019 
AND v4013 IN ('01130', '01610', '10716',
'10724', '19314', '10996', '20134') 