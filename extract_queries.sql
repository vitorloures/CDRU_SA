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

---- Extração de dados de emprego da cadeia de resíduos

SELECT * 
FROM `basedosdados.br_me_rais.microdados_vinculos` 
WHERE ano  = 2019
and cnae_2_subclasse IN (
-- Direto
'3822000', '3821100', '3811400', '3812200', '3839499', '2825900'
);


SELECT * 
FROM `basedosdados.br_me_rais.microdados_vinculos` 
WHERE ano  = 2019
and cnae_2_subclasse IN (
-- Indireto
'4687701', '4687703', '4687702', '3839401', '3900500', '3831999', '3832700', '3831901'
);


-- Extração dados de biogás (BEP - Parte 2) 
SELECT * 
FROM `basedosdados.br_me_rais.microdados_vinculos` 
WHERE ano  = 2019
and cnae_2_subclasse IN (
'0151202', '0154700', '0155505', '1011201', '1012101', '1012103', '1051100', '1052000', 
'1071600', '1931400', '3811400', '3821100', '3839499', '2832100', '1354500', '2221800', 
'2731700', '2833000', '3314710', '3321000', '4313400', '4322301', '4399103', '2710401', 
'3313901', '3511501', '3520401'
);

SELECT * 
FROM `basedosdados.br_me_rais.microdados_estabelecimentos`
WHERE ano  = 2019
and cnae_2_subclasse IN (
'0151202', '0154700', '0155505', '1011201', '1012101', '1012103', '1051100', '1052000', 
'1071600', '1931400', '3811400', '3821100', '3839499', '2832100', '1354500', '2221800', 
'2731700', '2833000', '3314710', '3321000', '4313400', '4322301', '4399103', '2710401', 
'3313901', '3511501', '3520401'
);

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
'10724', '19314', '10996', '20134');

