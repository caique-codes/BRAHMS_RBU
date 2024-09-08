
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###                 PREPARANDO O AMBIENTE  DE TRABALHO                       ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###



###
### Limpando arquivos armazenados na memoria (1)
###

rm(list = ls())


###
### Definindo opcao de codificacao dos carateres e linguagem (2)
###

aviso = getOption('warn')
options(warn = -1)
options(encoding = 'latin1')
options(warn = aviso)
rm(aviso)


###
### Definindo opcao de exibicao de numeros sem exponencial (3)
###

aviso = getOption('warn')
options(warn = -1)
options(scipen = 999)
options(warn = aviso)
rm(aviso)


###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###             INSTALANDO E CARREGANDO PACOTES NECESSARIOS                  ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###

###
### Instalando (5)
###

install.packages('PNADcIBGE')
install.packages('survey')
install.packages('convey')
install.packages("flextable")
install.packages("officer")
install.packages("patchwork")
install.packages("tidyverse")
install.packages("ggtext")
install.packages("extrafont")
install.packages('devtools')
install.packages("wesanderson")
install.packages("viridis")
install.packages("ggrepel")
install.packages("ggspatial")

if ('PNADcIBGE' %in% rownames(installed.packages()) == F) {
  install.packages('PNADcIBGE', dependencies = T)
}

if ('survey' %in% rownames(installed.packages()) == F) {
  install.packages('survey', dependencies = T)
}

if ('convey' %in% rownames(installed.packages()) == F) {
  install.packages('convey', dependencies = T)
}

if ('flextable' %in% rownames(installed.packages()) == F) {
  install.packages('flextable', dependencies = T)
}

if ('officer' %in% rownames(installed.packages()) == F) {
  install.packages('officer', dependencies = T)
}

if ('patchwork' %in% rownames(installed.packages()) == F) {
  install.packages('patchwork', dependencies = T)
}

if ('tidyverse' %in% rownames(installed.packages()) == F) {
  install.packages('tidyverse', dependencies = T)
}

if ('ggtext' %in% rownames(installed.packages()) == F) {
  install.packages('ggtext', dependencies = T)
}

if ('extrafont' %in% rownames(installed.packages()) == F) {
  install.packages('extrafont', dependencies = T)
}

if ('devtools' %in% rownames(installed.packages()) == F) {
  install.packages('devtools', dependencies = T)
}

if ('wesanderson' %in% rownames(installed.packages()) == F) {
  install.packages('wesanderson', dependencies = T)
}

if ('viridis' %in% rownames(installed.packages()) == F) {
  install.packages('viridis', dependencies = T)
}

if ('ggspatial' %in% rownames(installed.packages()) == F) {
  install.packages('ggspatial', dependencies = T)
}

if ('ggrepel' %in% rownames(installed.packages()) == F) {
  install.packages('ggrepel', dependencies = T)
}


###
### Carregando (6)
###


library(PNADcIBGE)
library(survey)
library(convey)
library(flextable)
library(officer)
library(patchwork)
library(tidyverse)
library(ggtext)
library(extrafont)
library(devtools)
devtools::install_github('ipeaGIT/geobr', subdir = 'r-package')
library(sf)
library(geobr)
library(wesanderson)
library(viridis)
library(ggspatial)
library(ggrepel)




###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###               ACESSANDO MICRODADOS DA PNAD CONTINUA                      ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###



###
### Metodo manual (7)
###


vars.select = c('UF', 'UPA', 'V1008', 'V1014', 'V1022', 'V1032', 'V2003', 
                'V2005', 'V2007', 'V2009', 'V3002', 'V3002A', 'V3003A', 
                'V3009A', 'V3014', 'V4009', 'V4010', 'V4012', 'V4013', 'V4014', 
                'V4028', 'V4029', 'V4032', 'V403312', 'V403322', 'V403412', 
                'V403422', 'V40401', 'V40402', 'V40403', 'V4041', 'V4043', 
                'V4045', 'V4047', 'V4048', 'V4049', 'V405012', 'V405022', 
                'V405112', 'V405122', 'V4057', 'V405812', 'V405822', 'V405912', 
                'V405922', 'V4076', 'V5001A2', 'V5002A2', 'V5003A2', 'V5004A2', 
                'V5006A2', 'V5007A2', 'VD4002', 'VD4009', 'VD4020', 'VD4022', 
                'VD4047', 'VD4048')

## Carregando BD da Visita 5 do diretorio (7.1)

Visita_5_MM = read_pnadc(microdata = 'PNADC_2019_visita5.txt', 
                         input_txt = 'input_PNADC_2019_visita5_20210104.txt', 
                         vars = vars.select)

## Rotulando vars categoricas (7.2)

#Visita_5_MM = pnadc_labeller(Visita_5_MM, 
#                            dictionary.file = 'dicionario_PNADC_microdados_2019_visita5_20210617.xls')

## Incluindo vars de deflatores (7.3)

#Visita_5_MM = pnadc_deflator(Visita_5_MM, deflator.file = 'deflator_PNADC_2019.xls')

## Expandindo amostra de base de acordo com os pesos amostrais (7.4)

#Amostra_Complexa = pnadc_design(Visita_5_MM)



###
### Metodo automatico (8)
###


#Visita_5_MA = get_pnadc(2019, interview = 5, labels = F, deflator = F,
 #                       design = T)



###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###               MANIPULANDO A AMOSTRA DA BASE DE COLETA                    ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###



###
### Chave de identificacao para domicilio (9)
###


#Visita_5_MM = Visita_5_MM %>% select(-ID_DOMICILIO)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Chave_Dom' = paste(UPA, V1008, V1014, sep = ''))


###
### Chave de identificacao de individuo (10)
###


Visita_5_MM = Visita_5_MM %>% 
  mutate('Chave_Ind' = paste(UPA, V1008, V1014, V2003, sep = ''))


###
### Ordenacao do BD por chaves de identificacao (11)
###


Visita_5_MM = Visita_5_MM %>% 
  arrange(ID_DOMICILIO, Chave_Ind)


###
### Ordenacao das colunas (12)
###


#Visita_5_MM = Visita_5_MM[, order(names(Visita_5_MM))]


###
### Ajustando rendas conforme salario-minimo - 2019 = R$ 998,00 (13)
###


## Rendimento de aposentadoria ou pensao publica (13.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate(V5004A2 = case_when(V5004A2 < 998 ~ 998,
                             TRUE ~ V5004A2))

## Rendimento bruto normal monetario - trabalho principal (13.2)

Visita_5_MM$VD4009 = as.numeric(Visita_5_MM$VD4009)

Visita_5_MM = Visita_5_MM %>% 
  mutate(V403312 = case_when(V403312 < 998 & VD4009 == 1 ~ 998, 
                             V403312 < 998 & VD4009 == 3 ~ 998, 
                             V403312 < 998 & VD4009 == 5 ~ 998, 
                             V403312 < 998 & VD4009 == 7 ~ 998, 
                             TRUE ~ V403312))

## Rendimento bruto normal monetario - trabalho secundario (13.3)

Visita_5_MM$V4043 = as.numeric(Visita_5_MM$V4043)

Visita_5_MM$V4047 = as.numeric(Visita_5_MM$V4047)

Visita_5_MM$V4048 = as.numeric(Visita_5_MM$V4048)

Visita_5_MM = Visita_5_MM %>% 
  mutate(V405012 = case_when(V405012 < 998 & V4043 == 1 & V4048 == 1 ~ 998, 
                             V405012 < 998 & V4043 == 2 ~ 998,
                             V405012 < 998 & V4043 == 3 & V4048 == 1 ~ 998, 
                             V405012 < 998 & V4043 == 4 & V4047 == 1 ~ 998,
                             V405012 < 998 & V4043 == 4 & V4048 == 1 ~ 998,
                             TRUE ~ V405012))



###
### Rendas diversas - exclusive renda mensal em produtos e mercadorias (14)
###


Visita_5_MM = Visita_5_MM %>% 
  mutate('Rendas_Diversas' =  tibble(VD4047, V5001A2 * (-1), V5002A2 * (-1), 
                                     V5003A2 * (-1)) %>% rowSums(na.rm = T))



###
### Renda individual - exclusive renda mensal em produtos e mercadorias (15)
###


Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_Individual' = tibble(V403312, V405012, V405812, V5001A2, V5002A2, 
                                     V5003A2, V5004A2, V5006A2, V5007A2, 
                                     VD4047) %>% rowSums(na.rm = T))



###
### Numero de pessoas no domicilio (16)
###


Visita_5_MM$V2005 = as.numeric(Visita_5_MM$V2005)

Visita_5_MM = Visita_5_MM %>% 
  mutate('filtro' = case_when(V2005 %in% c(1:16) ~ 1,
                              TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('Num_Moradores' = sum(filtro))



###
### Renda domiciliar - Exclusive renda mensal em produtos e mercadorias (17)
###


Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_Dom1' = case_when(filtro == 0 ~ 0,
                                  TRUE ~ Renda_Individual)) %>% 
  select(-filtro)

Visita_5_MM = Visita_5_MM %>% 
  group_by(Chave_Dom) %>% 
  mutate('Renda_Dom' = sum(Renda_Dom1)) %>% 
  select(-Renda_Dom1)

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_Dom = case_when(V2005 != 1 ~ NA_real_, 
                               TRUE ~ Renda_Dom))



###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###                   SIMULACAO DE BENEFICIOS SOCIAIS                        ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###



###
### SALARIO FAMILIA (18)
###


# Númeoro de filhos elegíveis - <= 14 anos

Visita_5_MM = Visita_5_MM %>% 
  mutate('filtro' = case_when(V2005 %in% c(4:6) & V2009 <= 14 ~ 1,
                              TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('Filho14' = sum(filtro)) %>% 
  select(-filtro)

## Salario Familia - Trabalho Principal (18.1)

Visita_5_MM$V4012 = as.numeric(Visita_5_MM$V4012)

Visita_5_MM$V4029 = as.numeric(Visita_5_MM$V4029)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_SalFam1' = round((V403312 - (Filho14 * 32.80)), digits = 2))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_SalFam2' = round((V403312 - (Filho14 * 46.54)), digits = 2))

Visita_5_MM = replace_na(Visita_5_MM, list(Renda_SalFam1 = 0))

Visita_5_MM = replace_na(Visita_5_MM, list(Renda_SalFam2 = 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate('SalFam1' = case_when(V4012 == 1 & V4029 == 1 & Renda_SalFam1 >= 907.78 & Renda_SalFam1 <= 1364.43 ~  Filho14 * 32.80,
                               V4012 == 3 & V4029 == 1 & Renda_SalFam1 >= 907.78 & Renda_SalFam1 <= 1364.43 ~  Filho14 * 32.80,
                               TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate(SalFam1 = case_when(V4012 == 1 & V4029 == 1 & Renda_SalFam2 >= 0 & Renda_SalFam2 <= 907.77 ~  Filho14 * 46.54,
                             V4012 == 3 & V4029 == 1 & Renda_SalFam2 >= 0 & Renda_SalFam2 <= 907.77 ~  Filho14 * 46.54,
                             TRUE ~ SalFam1))

Visita_5_MM = Visita_5_MM %>% 
  mutate(SalFam1 = case_when(V2005 %in% c(4:19) ~ 0,
                             TRUE ~ SalFam1))

## Salario Familia - Trabalho Secundario (18.2)

Visita_5_MM$V4043 = as.numeric(Visita_5_MM$V4043)

Visita_5_MM$V4048 = as.numeric(Visita_5_MM$V4048)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_SalFam3' = round((V405012 - (Filho14 * 32.80)), digits = 2))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_SalFam4' = round((V405012 - (Filho14 * 46.54)), digits = 2))

Visita_5_MM = replace_na(Visita_5_MM, list(Renda_SalFam3 = 0))

Visita_5_MM = replace_na(Visita_5_MM, list(Renda_SalFam4 = 0))


Visita_5_MM = Visita_5_MM %>% 
  mutate('SalFam2' = case_when(V4043 == 1 & V4048 == 1 & Renda_SalFam3 >= 907.78 & Renda_SalFam3 <= 1364.43 ~ Filho14 * 32.80,
                               V4043 == 3 & V4048 == 1 & Renda_SalFam3 >= 907.78 & Renda_SalFam3 <= 1364.43 ~ Filho14 * 32.80,
                               TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate(SalFam2 = case_when(V4043 == 1 & V4048 == 1 & Renda_SalFam4 >= 0 & Renda_SalFam4 <= 907.77 ~  Filho14 * 46.54,
                             V4043 == 3 & V4048 == 1 & Renda_SalFam4 >= 0 & Renda_SalFam4 <= 907.77 ~  Filho14 * 46.54,
                             TRUE ~ SalFam2))

Visita_5_MM = Visita_5_MM %>% 
  mutate(SalFam2 = case_when(V2005 %in% c(4:19) ~ 0,
                             TRUE ~ SalFam2))

## Salario Familia - Aposentados (18.3)

Visita_5_MM$V2007 = as.numeric(Visita_5_MM$V2007)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_SalFam5' = round((V5004A2 - (Filho14 * 32.80)), digits = 2))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_SalFam6' = round((V5004A2 - (Filho14 * 46.54)), digits = 2))

Visita_5_MM = replace_na(Visita_5_MM, list(Renda_SalFam5 = 0))

Visita_5_MM = replace_na(Visita_5_MM, list(Renda_SalFam6 = 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate('SalFam3' = case_when(V5004A2 > 0 & V2009 >= 65 & V2007 == 1 & Renda_SalFam5 >= 907.78 & Renda_SalFam5 <= 1364.43 ~ Filho14 * 32.80,
                               TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate(SalFam3 = case_when(V5004A2 > 0 & V2009 >= 65 & V2007 == 1 & Renda_SalFam6 >= 0 & Renda_SalFam6 <= 907.77 ~ Filho14 * 46.54,
                             TRUE ~ SalFam3))
Visita_5_MM = Visita_5_MM %>% 
  mutate(SalFam3 = case_when(V5004A2 > 0 & V2009 >= 60 & V2007 == 2 & Renda_SalFam5 >= 907.78 & Renda_SalFam5 <= 1364.43 ~ Filho14 * 32.80,
                             TRUE ~ SalFam3))

Visita_5_MM = Visita_5_MM %>% 
  mutate(SalFam3 = case_when(V5004A2 > 0 & V2009 >= 60 & V2007 == 2 & Renda_SalFam6 >= 0 & Renda_SalFam6 <= 907.77 ~ Filho14 * 46.54,
                             TRUE ~ SalFam3))

Visita_5_MM = Visita_5_MM %>% 
  mutate(SalFam3 = case_when(V2005 %in% c(4:19) ~ 0,
                             TRUE ~ SalFam3))

## Salario Familia - Trabalho + Secundario + Aposentados (18.4)

Visita_5_MM = Visita_5_MM %>%
  mutate('SalFam' = SalFam1 + SalFam2 + SalFam3)



###
### SEGURO-DESEMPREGO (19)
###


Visita_5_MM$VD4002 = as.numeric(Visita_5_MM$VD4002)

Visita_5_MM = Visita_5_MM %>% 
  mutate("SegDemp1" = case_when(VD4002 == 2 & Rendas_Diversas >= 798.40 & Rendas_Diversas <= 1735.29 ~ ((1267.11 * 4.78) / 12),
                               TRUE ~ NA_real_))

Df.aux = Visita_5_MM %>% 
  select(ID_DOMICILIO, Chave_Ind, VD4002, SegDemp1) %>% 
  filter(VD4002 == 2)

Df.aux = Df.aux[sample(nrow(Df.aux), 12503), ]

Df.aux = Df.aux %>% 
  mutate("SegDemp2" = case_when(is.na(SegDemp1) ~ ((1267.11 * 4.78) / 12),
                             TRUE ~ NA_real_)) %>% 
  select(-SegDemp1)

Visita_5_MM = left_join(Visita_5_MM, Df.aux, by = "Chave_Ind")

Visita_5_MM = Visita_5_MM %>% 
  mutate("SegDemp" = tibble(SegDemp1, SegDemp2) %>% rowSums(na.rm = T)) %>% 
  select(-SegDemp1, -SegDemp2, -ID_DOMICILIO.y, -VD4002.y) %>% 
  rename(VD4002 = VD4002.x, ID_DOMICILIO = ID_DOMICILIO.x)

rm(Df.aux)



###
### ABONO SALARIAL (20)
###



## Abono salarial - Trabalho Principal (20.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Abon_Sal1' = case_when(V2009 >= 23 & VD4009 == 1 & V403312 >= 0 & V403312 <= 1996 ~ (998 / 12),
                                 V2009 >= 23 & VD4009 == 5 & V403312 >= 0 & V403312 <= 1996 ~ (998 / 12),
                                 V2009 >= 23 & VD4009 == 7 & V403312 >= 0 & V403312 <= 1996 ~ (998 / 12),
                                 TRUE ~ 0))

## Abono salarial - Trabalho Secundario (20.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Abon_Sal2' = case_when(V2009 >= 23 & V4043 == 4 & V4047 == 1 & V405012 >= 0 & V405012 <= 1996 ~ (998 / 12),
                                 V2009 >= 23 & V4043 == 4 & V4048 == 1 & V405012 >= 0 & V405012 <= 1996 ~ (998 / 12),
                                 V2009 >= 23 & V4043 == 3 & V4048 == 1 & V405012 >= 0 & V405012 <= 1996 ~ (998 / 12),
                                 V2009 >= 23 & V4043 == 2 & V405012 >= 0 & V405012 <= 1996 ~ (998 / 12),
                                 TRUE ~ 0))


## Abono salarial - Trabalho Principal + Trabalho Secundario (20.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('AbonSal' = Abon_Sal1 + Abon_Sal2)


###
### ABONO ANUAL - DECIMO TERCEIRO SALARIO DO APOSENTADO E DO PENSIONISTA (21)
###


## Aposentaria e pensao (21.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Abono_A' = case_when(V5004A2 > 0 ~ (V5004A2 / 12),
                               TRUE ~ 0))



###
### BENEFICIO DE PRESTACAO CONTINUADA - BPC (22)
###


## Renda Individual - Exclusive Renda Mensal em Produtos e Mercadorias, BPC e Aposentadorias (22.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('RI_BPC' = tibble(V403312, V405012, V405812, V5003A2, V5006A2, V5007A2, VD4047) %>% 
           rowSums(na.rm = T))


## Renda Individual Per Capita - Exclusive Renda Mensal em Produtos e Mercadorias, BPC e Aposentadorias (22.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('RIPC_BPC' = RI_BPC / Num_Moradores)

## Calculo do BPC (22.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('BPC' = V5001A2)

Visita_5_MM = Visita_5_MM %>% 
  mutate(BPC = case_when(V2009 >= 65 & RIPC_BPC < 249.5 & is.na(V5001A2) & is.na(V5004A2) ~ 998,
                         TRUE ~ BPC))

Visita_5_MM = replace_na(Visita_5_MM, list(BPC = 0))


###
### BOLSA FAMILIA (23)
###


## Renda Individual - Exclusive renda mensal em produtos e mercadorias (23.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('RI_BF' = tibble(V403312, V405012, V405812, BPC, V5003A2, V5004A2, V5006A2, V5007A2, VD4047) %>% 
           rowSums(na.rm = T))

## Renda Individual Per Capita - Exclusive renda mensal em produtos e mercadorias (23.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('RIPC_BF' = RI_BF / Num_Moradores)

## Numero de dependentes entre 0 e 15 anos (23.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('f1' = case_when(V2005 %in% c(4:6) & V2009 %in% c(0:15) ~ 1,
                          TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('Ndep0_15' = sum(f1)) %>% 
  select(-f1)

## Numero de dependentes entre 16 e 17 anos (23.4)

Visita_5_MM = Visita_5_MM %>% 
  mutate('f1' = case_when(V2005 %in% c(4:6) & V2009 %in% c(16:17) ~ 1,
                          TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('Ndep16_17' = sum(f1)) %>% 
  select(-f1)

## Calculo do Bolsa Familia (23.5)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Basic' = case_when(V2005 == 1 & RIPC_BF <= 89 ~ 89,
                              TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Variav1' = case_when(V2005 == 1 & RIPC_BF <= 178 & Ndep0_15 <= 5 ~ Ndep0_15 * 41,
                               V2005 == 1 & RIPC_BF <= 178 & Ndep0_15 > 5 ~ 205,
                               TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Variav2' = case_when(V2005 == 1 & RIPC_BF <= 178 & Ndep16_17 <= 2 ~ Ndep16_17 * 48,
                               V2005 == 1 & RIPC_BF <= 178 & Ndep16_17 > 2 ~ 96,
                               TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Bolsa_Fam' = Basic + Variav1 + Variav2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('RDPC' = ((RI_BF + Bolsa_Fam) / Num_Moradores))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Variav3' = case_when(V2005 == 1 & RDPC < 89 ~ (89 - RDPC),
                               TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate('BolsaFam_Mensal' = Basic + Variav1 + Variav2 + Variav3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('BolsaFam' = (BolsaFam_Mensal + (BolsaFam_Mensal / 12)))


###
### DECIMO TERCEIRO SALARIO (24)
###


## Decimo terceiro salario - Trabalho principal (24.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate("Rend_Sal13_1" = V403312 - SalFam1)


Visita_5_MM = Visita_5_MM %>% 
  mutate('Sal13_1' = case_when(VD4009 == 1 ~ (Rend_Sal13_1 / 12),
                               VD4009 == 3 ~ (Rend_Sal13_1 / 12),
                               VD4009 == 5 ~ (Rend_Sal13_1 / 12),
                               VD4009 == 7 ~ (Rend_Sal13_1 / 12),
                               TRUE ~ 0))
Visita_5_MM = Visita_5_MM %>% 
  mutate(Sal13_1 = case_when(Sal13_1 < 0 ~ 0,
                             TRUE ~ Sal13_1))

## Decimo terceiro salario - Trabalho secundario (24.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate("Rend_Sal13_2" = V405012 - SalFam2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Sal13_2' = case_when(V4043 == 4 & V4047 == 1 ~ (Rend_Sal13_2 / 12),
                               V4043 == 4 & V4048 == 1 ~ (Rend_Sal13_2 / 12),
                               V4043 == 3 & V4048 == 1 ~ (Rend_Sal13_2 / 12),
                               V4043 == 1 & V4048 == 1 ~ (Rend_Sal13_2 / 12),
                               V4043 == 2 ~ (Rend_Sal13_2 / 12),
                               TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate(Sal13_2 = case_when(Sal13_2 < 0 ~ 0,
                             TRUE ~ Sal13_2))

## Decimo terceiro salario - Trabalho principal + secundario (24.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Sal13' = Sal13_1 + Sal13_2)


###
### ADICIONAL DE FERIAS (25)
###


## Adicional de ferias - Trabalho principal (25.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate("Rend_A_Ferias1" = (1/3) * Rend_Sal13_1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('A_Ferias1' = case_when(VD4009 == 1 ~ Rend_A_Ferias1 / 12,
                                 VD4009 == 3 ~ Rend_A_Ferias1 / 12,
                                 VD4009 == 5 ~ Rend_A_Ferias1 / 12,
                                 VD4009 == 7 ~ Rend_A_Ferias1 / 12,
                                 TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate(A_Ferias1 = case_when(A_Ferias1 < 0 ~ 0,
                               TRUE ~ A_Ferias1))

## Adicional de ferias - Trabalho secundario (25.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate("Rend_A_Ferias2" = (1/3) * Rend_Sal13_2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('A_Ferias2' = case_when(V4043 == 4 & V4047 == 1 ~ Rend_A_Ferias2 / 12,
                                 V4043 == 4 & V4048 == 1 ~  Rend_A_Ferias2 / 12,
                                 V4043 == 3 & V4048 == 1 ~ Rend_A_Ferias2 / 12,
                                 V4043 == 1 & V4048 == 1 ~ Rend_A_Ferias2 / 12,
                                 V4043 == 2 ~ Rend_A_Ferias2 / 12,
                                 TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate(A_Ferias2 = case_when(A_Ferias2 < 0 ~ 0,
                               TRUE ~ A_Ferias2))

## Adicional de ferias - Trabalho principal + secundario (25.3)

Visita_5_MM = Visita_5_MM %>%
  mutate('A_Ferias' = A_Ferias1 + A_Ferias2)



###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###                   SIMULACAO DAS CONTRIBUICOES SOCIAIS                    ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###



###
### EMPREGADO E TRABALHADOR AVULSO (26)
###


## Empregado e trabalhador avulso - Trabalho Principal (26.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_Conte1' = ((V403312 + A_Ferias1 + Sal13_1) - SalFam1))


Visita_5_MM = Visita_5_MM %>% 
  mutate('Conte_1' = case_when(VD4009 == 1 & Renda_Conte1 <= 1751.81 ~ 0.08 * Renda_Conte1,
                               VD4009 == 1 & Renda_Conte1 > 1751.81 & Renda_Conte1 <= 2919.72 ~ 0.09 * Renda_Conte1,
                               VD4009 == 1 & Renda_Conte1 > 2919.72 & Renda_Conte1 <= 5839.45 ~ 0.11 * Renda_Conte1,
                               VD4009 == 1 & Renda_Conte1 > 5839.45 ~ 0.11 * 5839.45,
                               TRUE ~ 0))

## Empregado e trabalhador avulso - Trabalho Secundario (26.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_Conte2' = ((V405012 + A_Ferias2 + Sal13_2) - SalFam2))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Conte_2' = case_when(V4043 == 3 & V4048 == 1 & Renda_Conte2 <= 1751.81 ~ 0.08 * Renda_Conte2,
                               V4043 == 3 & V4048 == 1 & Renda_Conte2 > 1751.81 & Renda_Conte2 <= 2919.72 ~ 0.09 * Renda_Conte2,
                               V4043 == 3 & V4048 == 1 & Renda_Conte2 > 2919.72 & Renda_Conte2 <= 5839.45 ~ 0.11 * Renda_Conte2,
                               V4043 == 3 & V4048 == 1 & Renda_Conte2 > 5839.45 ~ 0.11 * 5839.45,
                               TRUE ~ 0))

## Empregado e trabalhador avulso - Trabalho Principla + Secundario (26.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Conte' = Conte_1 + Conte_2)

Visita_5_MM = Visita_5_MM %>% 
  mutate(Conte = case_when(Conte < 0 ~ 0,
                           TRUE ~ Conte))



###
### EMPREGADO DOMESTICO (27)
###


## Empregado Domestico - Trabalho Principal (27.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_ConteD1' = V403312 + A_Ferias1 + Sal13_1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Conted1' = case_when(VD4009 == 3 & Renda_ConteD1 <= 1751.81 ~ 0.08 * Renda_ConteD1,
                               VD4009 == 3 & Renda_ConteD1 > 1751.81 & Renda_ConteD1 <= 2919.72 ~ 0.09 * Renda_ConteD1,
                               VD4009 == 3 & Renda_ConteD1 > 2919.72 & Renda_ConteD1 <= 5839.45 ~ 0.11 * Renda_ConteD1,
                               VD4009 == 3 & Renda_ConteD1 > 5839.45 ~ 0.11 * Renda_ConteD1,
                               TRUE ~ 0))

## Empregado Domestico - Trabalho Secundario (27.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_ConteD2' = V405012 + A_Ferias2 + Sal13_2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Conted2' = case_when(V4043 == 1 & V4048 == 1 & Renda_ConteD2 <= 1751.81 ~ 0.08 * Renda_ConteD2,
                               V4043 == 1 & V4048 == 1 & Renda_ConteD2 > 1751.81 & Renda_ConteD2 <= 2919.72 ~ 0.09 * Renda_ConteD2,
                               V4043 == 1 & V4048 == 1 & Renda_ConteD2 > 2919.72 & Renda_ConteD2 <= 5839.45 ~ 0.11 * Renda_ConteD2,
                               V4043 == 1 & V4048 == 1 & Renda_ConteD2 > 5839.45 ~ 0.11 * Renda_ConteD2,
                               TRUE ~ 0))

## Empregado Domestico - Trabalho Principal + Secundario (27.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Conted' = Conted1 + Conted2)

Visita_5_MM = Visita_5_MM %>% 
  mutate(Conted = case_when(Conted < 0 ~ 0,
                            TRUE ~ Conted))



###
### CONTRIBUINTE INDIVIDUAL (28)
###


## Contribuinte Individual - Trabalho Principal (28.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Contind1' = case_when(VD4009 == 8 ~ 0.11 * 998,
                                VD4009 == 9 ~ 0.11 * 998,
                                V4010 == 2631 ~ 0.11 * 998,
                                TRUE ~ 0))

## Contribuinte Individual - Trabalho Secundario (28.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Contind2' = case_when(V4043 == 5 ~ 0.11 * 998,
                                V4043 == 6 ~ 0.11 * 998,
                                TRUE ~ 0))

## Contribuinte Individual - Trabalho Principal + Secundario (28.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Contind' = Contind1 + Contind2)

Visita_5_MM = Visita_5_MM %>% 
  mutate(Contind = case_when(Contind < 0 ~ 0,
                             TRUE ~ Contind))


###
### SERVIDOR PUBLICO ESTATUTARIO FEDERAL (29)
###


Visita_5_MM$V4028 = as.numeric(Visita_5_MM$V4028)
Visita_5_MM$V4014 = as.numeric(Visita_5_MM$V4014)
Visita_5_MM$V4045 = as.numeric(Visita_5_MM$V4045)


## Servidor Publico Estatutario Federal - Trabalho Principal (29.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContFed1' = case_when(V4012 == 4 & V4028 == 1 & V4014 == 1 ~ 0.11 * Renda_ConteD1,
                                TRUE ~ 0))

## Servidor Publico Estatutario Federal - Trabalho Secundario (29.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContFed2' = case_when(V4043 == 4 & V4047 == 1 & V4045 == 1 ~ 0.11 * Renda_ConteD2,
                                TRUE ~ 0))

## Servidor Publico Estatutario Federal - Trabalho Principal + Secundario (29.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContFed' = ContFed1 + ContFed2)

Visita_5_MM = Visita_5_MM %>% 
  mutate(ContFed = case_when(ContFed < 0 ~ 0,
                             TRUE ~ ContFed))



###
### SERVIDOR PUBLICO ESTATUTARIO ESTADUAL (30)
###


## Servidor Publico Estatutario Estadual - Trabalho Principal (30.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContEst1' = case_when(V4012 == 2 & V4014 == 2 & UF == "11"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "12"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "13"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "14"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "15"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "16"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "17"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "21"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "23"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "24"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "25"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "27"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "31"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "32"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "33"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "35"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "41"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "50"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "51"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "53"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "11"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "12"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "13"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "14"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "15"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "16"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "17"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "21"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "23"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "24"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "25"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "27"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "31"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "32"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "33"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "35"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "41"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "50"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "51"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "53"  ~ 0.11 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "22" ~ 0.12 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "22" ~ 0.12 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "26" ~ 0.135 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "26" ~ 0.135 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "28" ~ 0.13 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "28" ~ 0.13 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "29" ~ 0.12 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "29" ~ 0.12 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "42" ~ 0.12 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "42" ~ 0.12 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "43" ~ 0.1325 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "43" ~ 0.1325 * Renda_ConteD1,
                                V4012 == 2 & V4014 == 2 & UF == "52" ~ 0.1325 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 2 & V4028 == 1 & UF == "52" ~ 0.1325 * Renda_ConteD1,
                                TRUE ~ 0))

## Servidor Publico Estatutario Estadual - Trabalho Secundario (30.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContEst2' = case_when(V4043 == 2 & V4045 == 2 & UF == "11" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "12" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "13" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "14" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "15" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "16" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "17" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "21" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "23" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "24" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "25" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "27" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "31" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "32" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "33" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "35" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "41" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "50" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "51" ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "53" ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "11"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "12"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "13"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "14"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "15"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "16"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "17"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "21"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "23"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "24"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "25"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "27"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "31"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "32"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "33"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "35"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "41"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "50"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "51"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "53"  ~ 0.11 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "22" ~ 0.12 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "22" ~ 0.12 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "26" ~ 0.135 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "26" ~ 0.135 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "28" ~ 0.13 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "28" ~ 0.13 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "29" ~ 0.12 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "29" ~ 0.12 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "42" ~ 0.12 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "42" ~ 0.12 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "43" ~ 0.1325 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "43" ~ 0.1325 * Renda_ConteD2,
                                V4043 == 2 & V4045 == 2 & UF == "52" ~ 0.1325 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 2 & V4047 == 1 & UF == "52" ~ 0.1325 * Renda_ConteD2,
                                TRUE ~ 0))

## Servidor Publico Estatutario Estadual - Trabalho Principal + Secundario (30.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContEst' = ContEst1 + ContEst2)
         
Visita_5_MM = Visita_5_MM %>% 
  mutate(ContEst = case_when(ContEst < 0 ~ 0,
                             TRUE ~ ContEst))



###
### SERVIDOR PUBLICO FEDERAL MILITAR (31)
###


## Servidor Publico Federal Militar - Trabalho Principal (31.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContMil1' = case_when(V4012 == 2 & V4014 == 1 ~ 0.075 * Renda_ConteD1,
                                TRUE ~ 0))

## Servidor Publico Federal Militar - Trabalho Secundario (31.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContMil2' = case_when(V4043 == 2 & V4045 == 1 ~ 0.075 * Renda_ConteD2,
                                TRUE ~ 0))

## Servidor Publico Federal Militar - Trabalho Principal + Secundario (31.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContMil' = ContMil1 + ContMil2)

Visita_5_MM = Visita_5_MM %>% 
  mutate(ContMil = case_when(ContMil < 0 ~ 0,
                             TRUE ~ ContMil))



###
### SERVIDOR PUBLICO MUNICIPAL (32)
###


## Servidor Publico Estatutario Municipal - Trabalho Principal (32.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContMun1' = case_when(V4012 == 2 & V4014 == 3 ~ 0.11 * Renda_ConteD1,
                                V4012 == 4 & V4014 == 3 & V4028 == 1 ~ 0.11 * Renda_ConteD1,
                                TRUE ~ 0))

## Servidor Publico Estatutario Municipal - Trabalho Secundario (32.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContMun2' = case_when(V4043 == 2 & V4045 == 3 ~ 0.11 * Renda_ConteD2,
                                V4043 == 4 & V4045 == 3 & V4047 == 1 ~ 0.11 * Renda_ConteD2,
                                TRUE ~ 0))

## Servidor Publico Estatutario Municipal - Trabalho Principal + Secundario (32.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContMun' = ContMun1 + ContMun2)

Visita_5_MM = Visita_5_MM %>% 
  mutate(ContMun = case_when(ContMun < 0 ~ 0,
                             TRUE ~ ContMun))



###
### APOSENTADOS E PENSIONISTAS (33)
###


Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_Aposent' = V5004A2 - 5839.45)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ContAposent' = case_when(Renda_Aposent > 0 ~ 0.11 * Renda_Aposent,
                                    TRUE ~ 0))



###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###     SIMULACAO Do IMPOSTO DE RENDA PESSOA FISICA 2020 (ANO BASE 2019)     ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###



###
### APOSENTADOS E PENSIONISTAS (34)
###


## Parcela Isenta de Aposentadoria - >65 anos (34.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Pisent65' = case_when(V2009 >= 65 & V5004A2 >= 1903.98 ~ 1903.98,
                                V2009 >= 65 & V5004A2 < 1903.98 ~ V5004A2,
                                TRUE ~ 0))

Visita_5_MM = replace_na(Visita_5_MM, list(Pisent65 = 0))

## Parcela Isenta de Abono Anual - >65 anos (34.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('PiAbonAn' = case_when(V2009 >= 65 & Abono_A >= 158.67 ~ 1903.98,
                                V2009 >= 65 & Abono_A < 158.67 ~ Abono_A,
                                TRUE ~ 0))

Visita_5_MM = replace_na(Visita_5_MM, list(PiAbonAn = 0))



###
### CALCULO DOS RENDIMENTOS TRIBUTAVEIS (35)
###


## Rendimentos Tributaveis (35.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('RTB' = tibble(V403312, V405012, V405812, V5004A2, V5006A2, V5007A2, A_Ferias, AbonSal, (-1) * Pisent65) %>% 
           rowSums(na.rm = T))

Visita_5_MM = Visita_5_MM %>%
  mutate(RTB = case_when(RTB < 0 ~ 0,
                         TRUE ~ RTB))

## Numero de Dependentes (35.2)

Visita_5_MM$V3002A = as.numeric(Visita_5_MM$V3002A)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Dep' = case_when(RTB <= 1903.98 & V2005 == 2 ~ 1,
                           RTB <= 1903.98 & V2005 == 3 ~ 1,
                           RTB <= 1903.98 & V2009 <= 21 & V2005 == 4 ~ 1,
                           RTB <= 1903.98 & V2009 <= 21 & V2005 == 5 ~ 1,
                           RTB <= 1903.98 & V2009 <= 21 & V2005 == 6 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 4 & V3003A == 8 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 4 & V3003A == 9 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 4 & V3003A == 10 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 4 & V3003A == 11 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 5 & V3003A == 8 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 5 & V3003A == 9 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 5 & V3003A == 10 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 5 & V3003A == 11 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 6 & V3003A == 8 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 6 & V3003A == 9 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 6 & V3003A == 10 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 6 & V3003A == 11 ~ 1,
                           RTB <= 1903.98 & V2005 == 8 ~ 1,
                           RTB <= 1903.98 & V2005 == 13 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 10 & V3003A == 8 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 10 & V3003A == 9 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 10 & V3003A == 10 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 10 & V3003A == 11 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 11 & V3003A == 8 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 11 & V3003A == 9 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 11 & V3003A == 10 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 11 & V3003A == 11 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 12 & V3003A == 8 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 12 & V3003A == 9 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 12 & V3003A == 10 ~ 1,
                           RTB <= 1903.98 & V2009 <= 24 & V2005 == 12 & V3003A == 11 ~ 1,
                           TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate('DepInst' = case_when(RTB <= 1903.98 & V3002A == 1 & V2005 == 2 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2005 == 3 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 21 & V2005 == 4 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 21 & V2005 == 5 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 21 & V2005 == 6 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 4 & V3003A == 8 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 4 & V3003A == 9 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 4 & V3003A == 10 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 4 & V3003A == 11 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 5 & V3003A == 8 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 5 & V3003A == 9 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 5 & V3003A == 10 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 5 & V3003A == 11 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 6 & V3003A == 8 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 6 & V3003A == 9 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 6 & V3003A == 10 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 6 & V3003A == 11 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 10 & V3003A == 8 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 10 & V3003A == 9 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 10 & V3003A == 10 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 10 & V3003A == 11 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 11 & V3003A == 8 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 11 & V3003A == 9 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 11 & V3003A == 10 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 11 & V3003A == 11 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 12 & V3003A == 8 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 12 & V3003A == 9 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 12 & V3003A == 10 ~ 1,
                               RTB <= 1903.98 & V3002A == 1 & V2009 <= 24 & V2005 == 12 & V3003A == 11 ~ 1,
                               TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Rtbd' = case_when(RTB <= 1903.98 & V2005 == 2 ~ RTB,
                            RTB <= 1903.98 & V2005 == 3 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 21 & V2005 == 4 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 21 & V2005 == 5 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 21 & V2005 == 6 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 4 & V3003A == 8 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 4 & V3003A == 9 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 4 & V3003A == 10 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 4 & V3003A == 11 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 5 & V3003A == 8 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 5 & V3003A == 9 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 5 & V3003A == 10 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 5 & V3003A == 11 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 6 & V3003A == 8 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 6 & V3003A == 9 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 6 & V3003A == 10 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 6 & V3003A == 11 ~ RTB,
                            RTB <= 1903.98 & V2005 == 8 ~ RTB,
                            RTB <= 1903.98 & V2005 == 13 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 10 & V3003A == 8 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 10 & V3003A == 9 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 10 & V3003A == 10 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 10 & V3003A == 11 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 11 & V3003A == 8 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 11 & V3003A == 9 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 11 & V3003A == 10 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 11 & V3003A == 11 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 12 & V3003A == 8 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 12 & V3003A == 9 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 12 & V3003A == 10 ~ RTB,
                            RTB <= 1903.98 & V2009 <= 24 & V2005 == 12 & V3003A == 11 ~ RTB,
                            TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('N_Dep' = sum(Dep), 'N_DepInst' = sum(DepInst), 'RtbdDep' = sum(Rtbd))

## Rendimentos Tributaveis da Pessoa de Referencia - Incluindo o dos Dependentes (35.3)

Visita_5_MM = Visita_5_MM %>%
  mutate(RtbdDep = case_when(V2005 != 1 ~ 0,
                             TRUE ~ RtbdDep))

Visita_5_MM = Visita_5_MM %>%
  mutate(N_Dep = case_when(V2005 != 1 ~ 0,
                           TRUE ~ N_Dep))

Visita_5_MM = Visita_5_MM %>%
  mutate(RTB = case_when(Dep == 1 ~ 0,
                         TRUE ~ RTB))

Visita_5_MM = Visita_5_MM %>% 
  mutate('RendTrib' = round((RTB + RtbdDep), digits = 2))


###
### DEDUCOES (36)
###


## Deducao com dependentes (36.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('DedDep' = case_when(V2005 == 1 ~ 189.59 * N_Dep,
                              TRUE ~ 0))

## Deducao com Instrucao (36.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('DedInst' = case_when(V2005 == 1 & RTB > 1903.98 ~ 296.79 * N_DepInst,
                               TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate(DedInst = case_when(V2005 == 1 & V3002A == 1 & RendTrib > 1903.98 ~ 296.79 + DedInst,
                             TRUE ~ DedInst))

Visita_5_MM = Visita_5_MM %>% 
  mutate(DedInst = case_when(V2005 != 1 & DepInst != 1 & V3002A == 1 & RendTrib > 1903.98 ~ 296.79,
                             TRUE ~ DedInst))

Visita_5_MM = Visita_5_MM %>% 
  mutate(DedInst = case_when(Renda_Dom == 0 ~ 0,
                             TRUE ~ DedInst))

## Deducao com Saude (36.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('FrendTrib' = case_when(RendTrib >= 0 & RendTrib <= 400 ~ 1,
                                 RendTrib > 400 & RendTrib <= 880 ~ 2,
                                 RendTrib > 880 & RendTrib <= 1760 ~ 3,
                                 RendTrib > 1760 & RendTrib <= 2640 ~ 4,
                                 RendTrib > 2640 & RendTrib <= 4400 ~ 5,
                                 RendTrib > 4400 & RendTrib <= 6160 ~ 6,
                                 RendTrib > 6160 & RendTrib <= 8800 ~ 7,
                                 RendTrib > 8800 & RendTrib <= 13200 ~ 8,
                                 RendTrib > 13200 & RendTrib <= 17600 ~ 9,
                                 RendTrib > 17600 & RendTrib <= 26400 ~ 10,
                                 RendTrib > 26400 & RendTrib <= 35200 ~ 11,
                                 RendTrib > 35200 & RendTrib <= 52800 ~ 12,
                                 RendTrib > 52800 & RendTrib <= 70400 ~ 13,
                                 RendTrib > 70400 & RendTrib <= 140800 ~ 14,
                                 RendTrib > 140800 & RendTrib <= 211200 ~ 15,
                                 RendTrib > 211200 & RendTrib <= 281600 ~ 16,
                                 RendTrib > 281600 ~ 17))

Visita_5_MM = Visita_5_MM %>% 
  mutate('DM_IRPF' = case_when(FrendTrib == 1 ~ 59.42,
                               FrendTrib == 2 ~ 38.76,
                               FrendTrib == 3 ~ 50.71,
                               FrendTrib == 4 ~ 57.03,
                               FrendTrib == 5 ~ 166.16,
                               FrendTrib == 6 ~ 285.85,
                               FrendTrib == 7 ~ 401.84,
                               FrendTrib == 8 ~ 604.72,
                               FrendTrib == 9 ~ 828.84,
                               FrendTrib == 10 ~ 1069.89,
                               FrendTrib == 11 ~ 1371.84,
                               FrendTrib == 12 ~ 1357.14,
                               FrendTrib == 13 ~ 1361.84,
                               FrendTrib == 14 ~ 1576.34,
                               FrendTrib == 15 ~ 1833.50,
                               FrendTrib == 16 ~ 1859.10,
                               FrendTrib == 17 ~ 2515.63))

Visita_5_MM = Visita_5_MM %>% 
  mutate('DedSaude' = case_when(RTB > 1903.98 ~ DM_IRPF,
                                TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate(DedSaude = case_when(Renda_Dom == 0 ~ 0,
                              TRUE ~ DedSaude))

## Deducao Padrao (36.4)

Visita_5_MM = Visita_5_MM %>% 
  mutate('RendTrib1' = 0.2 * RendTrib)

Visita_5_MM = Visita_5_MM %>% 
  mutate('DedPad' = case_when(RendTrib1 < 1396.20 ~ RendTrib1,
                              RendTrib1 >= 1396.20 ~ 1396.20))



###
### CALCULO DO IMPOSTO - DECLARACAO COMPLETA (37)
###


## Total de Contribuicoes (37.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Cont' = Conte + Conted + Contind + ContFed + ContEst + ContMil + ContMun + ContAposent)

## Base de Calculo da Declaracao Completa (37.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('BaseCal1' = RendTrib - Cont - DedDep - DedInst - DedSaude)

Visita_5_MM = Visita_5_MM %>% 
  mutate(BaseCal1 = case_when(BaseCal1 < 0 ~ 0,
                              TRUE ~ BaseCal1))

## Calculo do Imposto - Salario (37.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ImpComp' = case_when(BaseCal1 <= 1903.98 ~ 0,
                               BaseCal1 > 1903.98 & BaseCal1 <= 2826.65 ~ (0.075 * (BaseCal1 - 1903.98)),
                               BaseCal1 > 2826.65 & BaseCal1 <= 3751.05 ~ ((0.075 * 922.67) + (0.15 * (BaseCal1 - 2826.65))),
                               BaseCal1 > 3751.05 & BaseCal1 <= 4664.68 ~ ((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * (BaseCal1 - 3751.05))),
                               BaseCal1 > 4664.68 ~ ((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * 913.62) + (0.275 * (BaseCal1 - 4664.68))),
                               TRUE ~ 0))

## Calculo do Imposto - 13Âº Salario (37.4)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_Impc13_1' = 12 * Sal13_1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Impc13_1' = case_when(Renda_Impc13_1 <= 1903.98 ~ 0,
                                Renda_Impc13_1 > 1903.98 & Renda_Impc13_1 <= 2826.65 ~ ((0.075 * (Renda_Impc13_1 - 1903.98)) / 12),
                                Renda_Impc13_1 > 2826.65 & Renda_Impc13_1 <= 3751.05 ~ (((0.075 * 922.67) + (0.15 * (Renda_Impc13_1 - 2826.65))) / 12),
                                Renda_Impc13_1 > 3751.05 & Renda_Impc13_1 <= 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * (Renda_Impc13_1 - 3751.05))) / 12),
                                Renda_Impc13_1 > 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * 913.62) + (0.275 * (Renda_Impc13_1 - 4664.68))) / 12),
                                TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_Impc13_2' = 12 * Sal13_2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Impc13_2' = case_when(Renda_Impc13_2 <= 1903.98 ~ 0,
                                Renda_Impc13_2 > 1903.98 & Renda_Impc13_2 <= 2826.65 ~ ((0.075 * (Renda_Impc13_2 - 1903.98)) / 12),
                                Renda_Impc13_2 > 2826.65 & Renda_Impc13_2 <= 3751.05 ~ (((0.075 * 922.67) + (0.15 * (Renda_Impc13_2 - 2826.65))) / 12),
                                Renda_Impc13_2 > 3751.05 & Renda_Impc13_2 <= 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * (Renda_Impc13_2 - 3751.05))) / 12),
                                Renda_Impc13_2 > 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * 913.62) + (0.275 * (Renda_Impc13_2 - 4664.68))) / 12),
                                TRUE ~ 0))

## Calculo do Imposto - Abono Anual (37.5)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Renda_ImpcAbo' = ((Abono_A - PiAbonAn) * 12))

Visita_5_MM = Visita_5_MM %>% 
  mutate('ImpcAbo' = case_when(Renda_ImpcAbo <= 1903.98 ~ 0,
                               Renda_ImpcAbo > 1903.98 & Renda_ImpcAbo <= 2826.65 ~ ((0.075 * (Renda_ImpcAbo - 1903.98)) / 12),
                               Renda_ImpcAbo > 2826.65 & Renda_ImpcAbo <= 3751.05 ~ (((0.075 * 922.67) + (0.15 * (Renda_ImpcAbo - 2826.65))) / 12),
                               Renda_ImpcAbo > 3751.05 & Renda_ImpcAbo <= 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * (Renda_ImpcAbo - 3751.05))) / 12),
                               Renda_ImpcAbo > 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * 913.62) + (0.275 * (Renda_ImpcAbo - 4664.68))) / 12),
                               TRUE ~ 0))

## Total (37.6)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ImpTotc' = ImpComp + Impc13_1 + Impc13_2 + ImpcAbo)


###
### CALCULO DO IMPOSTO - DECLARACAO SIMPLIFICDA (38)
###


## Base de Calculo da Declaracao Simplificada (38.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('BaseCal2' = case_when(RendTrib1 < 1396.20 ~ RendTrib - RendTrib1,
                                RendTrib1 >= 1396.20 ~ RendTrib - 1396.20))

## Calculo do Imposto - Salario (38.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ImpSimp' = case_when(BaseCal2 <= 1903.98 ~ 0,
                               BaseCal2 > 1903.98 & BaseCal2 <= 2826.65 ~ (0.075 * (BaseCal2 - 1903.98)),
                               BaseCal2 > 2826.65 & BaseCal2 <= 3751.05 ~ ((0.075 * 922.67) + (0.15 * (BaseCal2 - 2826.65))),
                               BaseCal2 > 3751.05 & BaseCal2 <= 4664.68 ~ ((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * (BaseCal2 - 3751.05))),
                               BaseCal2 > 4664.68 ~ ((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * 913.62) + (0.275 * (BaseCal2 - 4664.68))),
                               TRUE ~ 0))

## Calculo do Imposto - 13º Salario (38.3)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Imps13_1' = case_when(Renda_Impc13_1 <= 1903.98 ~ 0,
                                Renda_Impc13_1 > 1903.98 & Renda_Impc13_1 <= 2826.65 ~ ((0.075 * (Renda_Impc13_1 - 1903.98)) / 12),
                                Renda_Impc13_1 > 2826.65 & Renda_Impc13_1 <= 3751.05 ~ (((0.075 * 922.67) + (0.15 * (Renda_Impc13_1 - 2826.65))) / 12),
                                Renda_Impc13_1 > 3751.05 & Renda_Impc13_1 <= 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * (Renda_Impc13_1 - 3751.05))) / 12),
                                Renda_Impc13_1 > 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * 913.62) + (0.275 * (Renda_Impc13_1 - 4664.68))) / 12),
                                TRUE ~ 0))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Imps13_2' = case_when(Renda_Impc13_2 <= 1903.98 ~ 0,
                                Renda_Impc13_2 > 1903.98 & Renda_Impc13_2 <= 2826.65 ~ ((0.075 * (Renda_Impc13_2 - 1903.98)) / 12),
                                Renda_Impc13_2 > 2826.65 & Renda_Impc13_2 <= 3751.05 ~ (((0.075 * 922.67) + (0.15 * (Renda_Impc13_2 - 2826.65))) / 12),
                                Renda_Impc13_2 > 3751.05 & Renda_Impc13_2 <= 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * (Renda_Impc13_2 - 3751.05))) / 12),
                                Renda_Impc13_2 > 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * 913.62) + (0.275 * (Renda_Impc13_2 - 4664.68))) / 12),
                                TRUE ~ 0))

## Calculo do Imposto - Abono Anual (38.4)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ImpsAbo' = case_when(Renda_ImpcAbo <= 1903.98 ~ 0,
                               Renda_ImpcAbo > 1903.98 & Renda_ImpcAbo <= 2826.65 ~ ((0.075 * (Renda_ImpcAbo - 1903.98)) / 12),
                               Renda_ImpcAbo > 2826.65 & Renda_ImpcAbo <= 3751.05 ~ (((0.075 * 922.67) + (0.15 * (Renda_ImpcAbo - 2826.65))) / 12),
                               Renda_ImpcAbo > 3751.05 & Renda_ImpcAbo <= 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * (Renda_ImpcAbo - 3751.05))) / 12),
                               Renda_ImpcAbo > 4664.68 ~ (((0.075 * 922.67) + (0.15 * 924.39) + (0.225 * 913.62) + (0.275 * (Renda_ImpcAbo - 4664.68))) / 12),
                               TRUE ~ 0))

## Total (38.5)

Visita_5_MM = Visita_5_MM %>% 
  mutate('ImpTots' = ImpSimp + Imps13_1 + Imps13_2 + ImpsAbo)



###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###                       RENDAS E QUINTIS DE RENDA                          ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###


###
### Renda Individual Inicial - RII (39)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('RII' = tibble(V403312, V405012, V405812, V5006A2, V5007A2, Rendas_Diversas, Sal13, A_Ferias) %>% 
           rowSums(na.rm = T))


###
### Renda Domiciliar Inicial - RDI (40)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate(Rend = case_when(V2005 %in% c(17:19) ~ 0,
                          TRUE ~ RII)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('RDI' = sum(Rend)) %>% 
  select(-Rend)


###
### Renda Individual Bruta - RIB (41)
###

# RIB = Impacto com Bolsa-Familia
# RIBSBF = Impacto sem Bolsa-Familia

Visita_5_MM = Visita_5_MM %>% 
  mutate('RIB' = tibble(RII, SalFam, SegDemp, AbonSal, BolsaFam, Abono_A, BPC, V5003A2, V5004A2) %>% 
           rowSums(na.rm = T))

Visita_5_MM = Visita_5_MM %>% 
  mutate('RIBSBF' = RIB - BolsaFam)


###
### Renda Domiciliar Bruta - RDB (42)
###

# RDB = Impacto com Bolsa-Familia
# RDBSBF = Impacto sem Bolsa-Familia

Visita_5_MM = Visita_5_MM %>% 
  mutate(Rend = case_when(V2005 %in% c(17:19) ~ 0,
                          TRUE ~ RIB),
         Rend1 = case_when(V2005 %in% c(17:19) ~ 0,
                          TRUE ~ RIBSBF)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('RDB' = sum(Rend),
         'RDBSBF' = sum(Rend1)) %>% 
  select(-Rend, -Rend1)


###
### Renda Individual Disponivel - RID (43)
###

# RID = Impacto com Bolsa-Familia
# RIDSBF = Impacto sem Bolsa-Familia

Visita_5_MM = Visita_5_MM %>% 
  mutate('RID' = RIB - Cont - ImpTots,
         'RIDSBF' = RIBSBF - Cont - ImpTots)


###
### Renda Domiciliar Disponivel - RDD (44)
###

# RDD = Impacto com Bolsa-Familia
# RDDSBF = Impacto sem Bolsa-Familia

Visita_5_MM = Visita_5_MM %>% 
  mutate(Rend = case_when(V2005 %in% c(17:19) ~ 0,
                          TRUE ~ RID),
         Rend1 = case_when(V2005 %in% c(17:19) ~ 0,
                          TRUE ~ RIDSBF)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('RDD' = sum(Rend),
         'RDDSBF' = sum(Rend1)) %>% 
  select(-Rend, -Rend1)


###
### Rendas Domiciliares  - Per Capita (45)
###

# RDIPC = Renda Domiciliar Inicial Per Capita
# RDBPC = Renda Domiciliar Bruta Per Capita
# RDBSBFPC = Renda Domiciliar Bruta Sem Bolsa-Familia Per Capita
# RDDPC = Renda Domiciliar Disponivel Per Capita
# RDDSBFPC = Renda Domiciliar Disponivel Per Capita sem Bolsa-Familia

Visita_5_MM = Visita_5_MM %>% 
  mutate('RDIPC' = round((RDI / Num_Moradores), digits = 2),
         'RDBPC' = round((RDB / Num_Moradores), digits = 2),
         'RDBSBFPC' = round((RDBSBF / Num_Moradores), digits = 2),
         'RDDPC' = round((RDD / Num_Moradores), digits = 2),
         'RDDSBFPC' = round((RDDSBF / Num_Moradores), digits = 2))

## Ajustes (45.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate(RDI = case_when(V2005 != 1 ~ NA_real_,
                              TRUE ~ RDI),
         RDB = case_when(V2005 != 1 ~ NA_real_,
                         TRUE ~ RDB),
         RDD = case_when(V2005 != 1 ~ NA_real_,
                         TRUE ~ RDD),
         RDBSBF = case_when(V2005 != 1 ~ NA_real_,
                            TRUE ~ RDBSBF),
         RDDSBF = case_when(V2005 != 1 ~ NA_real_,
                            TRUE ~ RDDSBF))

## Ajustes remanescentes (45.2)

Visita_5_MM = replace_na(Visita_5_MM, list(RDIPC = 0))
Visita_5_MM = replace_na(Visita_5_MM, list(RDBPC = 0))
Visita_5_MM = replace_na(Visita_5_MM, list(RDDPC = 0))
Visita_5_MM = replace_na(Visita_5_MM, list(RDBSBFPC = 0))
Visita_5_MM = replace_na(Visita_5_MM, list(RDDSBFPC = 0))



###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###                         INDICADORES DE POBREZA                           ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###


###
### INDICADORES DE POBREZA - IPEA (46)
###

## Numero de Extremamente Pobres - Linha de Extrema Pobreza do IPEA, R$ 422 (46.1)
# A linha de extrema Pobreza aqui considerada e o valor medio de uma cesta 
# basica calculado pelo DIEESE para 16 capitais do pais.
# Pob_RDIPC = Pobres sob a Renda Domiciliar inicial Per Capita
# Pob_RDBPC = Pobres sob a Renda Domiciliar Bruta Per Capita
# Pob_RDDPC = Pobres sob a Renda Domiciliar Disponivel Per Capita
# Pob_RDBSBFPC = Pobres sob a Renda Domiciliar Bruta Sem Bolsa Familia Per Capita
# Pob_RDBSBFPC = Pobres sob a Renda Domiciliar Disponivel Sem Bolsa Familia Per Capita

Visita_5_MM = Visita_5_MM %>% 
  mutate('Pob_RDIPC' = case_when(RDIPC <= 422 ~ 1,
                                 TRUE ~ 0),
         'Pob_RDBPC' = case_when(RDBPC <= 422 ~ 1,
                                 TRUE ~ 0),
         'Pob_RDDPC' = case_when(RDDPC <= 422 ~ 1,
                                 TRUE ~ 0),
         'Pob_RDBSBFPC' = case_when(RDBSBFPC <= 422 ~ 1,
                                    TRUE ~ 0),
         'Pob_RDDSBFPC' = case_when(RDDSBFPC <= 422 ~ 1,
                                    TRUE ~ 0))

## Hiato de Extremamente Pobres - Linha de Extrema pobreza do IPEA (46.2)
# Hiato_RDIPC = Hiato sob a Renda Domiciliar inicial Per Capita
# Hiato_RDBPC = Hiato sob a Renda Domiciliar Bruta Per Capita
# Hiato_RDDPC = Hiato sob a Renda Domiciliar Disponivel Per Capita
# Hiato_RDBSBFPC = Hiato sob a Renda Domiciliar Bruta Sem Bolsa Familia Per Capita
# Hiato_RDDSBFPC = Hiato sob a Renda Domiciliar Disponivel Sem Bolsa Familia Per Capita

Visita_5_MM = Visita_5_MM %>% 
  mutate('Hiato_RDIPC' = case_when(RDIPC <= 422 ~ 422 - RDIPC,
                                      TRUE ~ NA_real_),
         'Hiato_RDBPC' = case_when(RDBPC <= 422 ~ 422 - RDBPC,
                                   TRUE ~ NA_real_),
         'Hiato_RDDPC' = case_when(RDDPC <= 422 ~ 422 - RDDPC,
                                   TRUE ~ NA_real_),
         'Hiato_RDBSBFPC' = case_when(RDBSBFPC <= 422 ~ 422 - RDBSBFPC,
                                      TRUE ~ NA_real_),
         'Hiato_RDDSBFPC' = case_when(RDDSBFPC <= 422 ~ 422 - RDDSBFPC,
                                      TRUE ~ NA_real_))



###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###           SIMULACAO DA RBU - MODELO IMP. DE RENDA NEG.                   ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###


###
### RENDA BASICA UNIVERSAL - RBU (47)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('RBU' = 422)


###
### TRANSFERENCIAS (48)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('Transf' = RIB - RII)

## Cálculo das transferencias Domiciliares (48.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_1 = case_when(V2005 %in% c(17:19) ~ 0,
                             TRUE ~ Transf)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('Transf_Dom' = sum(Renda_1)) %>% 
  select(-Renda_1)

## Cálculo das transferencias Domiciliares Per Capita (48.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Transf_PC' = Transf_Dom / Num_Moradores) %>% 
  mutate(Transf_Dom = case_when(V2005 != 1 ~ NA_real_,
                               TRUE ~ Transf_Dom))


###
### OUTRAS TRANSFERÊNCIAS - EXCLUINDO APOSENTADORIA E PENSÃO + ABONO ANUAL + BPC (49)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('Transf_Out' = tibble(SalFam, AbonSal, SegDemp, BolsaFam, V5003A2) %>% 
           rowSums(na.rm = T))


###
### BENEFICIOS DO TRABALHO (50)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate("Ben_Trab" = Transf_Out - BolsaFam)

# Cálculo dos beneficios do trabalho Domiciliares (50.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_1 = case_when(V2005 %in% c(17:19) ~ 0,
                             TRUE ~ Ben_Trab)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('Ben_Trab_D' = sum(Renda_1)) %>% 
  select(-Renda_1)

## Cálculo dos beneficios do trabalho Domiciliares Per Capita (50.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Ben_Trab_PC' = Ben_Trab_D / Num_Moradores) %>% 
  mutate(Ben_Trab_D = case_when(V2005 != 1 ~ NA_real_,
                                TRUE ~ Ben_Trab))


###
### CALCULO DO BOLSA FAMILIA DOMICILIAR (51)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_1 = case_when(V2005 %in% c(17:19) ~ 0,
                             TRUE ~ BolsaFam)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('PBF_Dom' = sum(Renda_1)) %>% 
  select(-Renda_1)

## Cálculo do bolsa familia Domiciliar Per Capita (51.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('PBF_PC' = PBF_Dom / Num_Moradores) %>% 
  mutate(PBF_Dom = case_when(V2005 != 1 ~ NA_real_,
                              TRUE ~ PBF_Dom))


###
### TRIBUTOS DIRETOS (52)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('Trib_D' = ImpTots + Cont)

## Cálculo dos tributos diretos Domiciliares (52.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_1 = case_when(V2005 %in% c(17:19) ~ 0,
                             TRUE ~ Trib_D)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('Trib_Dom' = sum(Renda_1)) %>% 
  select(-Renda_1)

## Cálculo dos beneficios do trabalho Domiciliares Per Capita (52.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Trib_Pc' = Trib_Dom / Num_Moradores) %>% 
  mutate(Trib_Dom = case_when(V2005 != 1 ~ NA_real_,
                              TRUE ~ Trib_Dom))


###
### CALCULO DO IRPF DOMICILIAR (53)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_1 = case_when(V2005 %in% c(17:19) ~ 0,
                             TRUE ~ ImpTots)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('IRPF_Dom' = sum(Renda_1)) %>% 
  select(-Renda_1)

## Cálculo do IRPF Domiciliar Per Capita (53.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('IRPF_PC' = IRPF_Dom / Num_Moradores) %>% 
  mutate(IRPF_Dom = case_when(V2005 != 1 ~ NA_real_,
                              TRUE ~ IRPF_Dom))


###
### CALCULO DAS CONTRIBUICOES PREVIDENCIARIAS (54)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_1 = case_when(V2005 %in% c(17:19) ~ 0,
                             TRUE ~ Cont)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('Cont_Dom' = sum(Renda_1)) %>% 
  select(-Renda_1)

## Cálculo das constribuições previdenciarias domiciliares Per Capita (54.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Cont_PC' = Cont_Dom / Num_Moradores) %>% 
  mutate(Cont_Dom = case_when(V2005 != 1 ~ NA_real_,
                              TRUE ~ Cont_Dom))


###
### BENEFÍCIOS AJUSTADOS - APOSENTADORIA E PENSÃO + BPC - RBU (55)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('Aposent_BPC' = tibble(V5004A2, Abono_A, BPC) %>% 
           rowSums(na.rm = T))

Visita_5_MM = Visita_5_MM %>% 
  mutate('Ben_Aju' = case_when(Aposent_BPC > 0 ~ Aposent_BPC - RBU,
                               TRUE ~ 0))


###
### CALCULO DAS PENSOES DOMICILIARES (56)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_1 = case_when(V2005 %in% c(17:19) ~ 0,
                             TRUE ~ Aposent_BPC)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('Pensoes_Dom' = sum(Renda_1)) %>% 
  select(-Renda_1)

## Cálculo das pensoes Domiciliares Per Capita (56.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate('Pensoes_PC' = Pensoes_Dom / Num_Moradores) %>% 
  mutate(Pensoes_Dom = case_when(V2005 != 1 ~ NA_real_,
                              TRUE ~ Pensoes))


###
### BASE DE CÁLCULO DO NOVO IRPF (57)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('Basecal3' = RII + Ben_Aju)


###
### RECEITA TRIBUTÁRIA (58)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('Receita' = RBU + Ben_Aju - Transf + Trib_D)


###
### RENDA BRUTA PÓS-REFORMA (59)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('RIB_RBU' = RII + RBU + Ben_Aju)

## Cálculo da Renda Domiciliar bruta pós-reforma (59.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_1 = case_when(V2005 %in% c(17:19) ~ 0,
                             TRUE ~ RIB_RBU)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('RDB_RBU' = sum(Renda_1)) %>% 
  select(-Renda_1)

## Cálculo da Renda Domiciliar bruta Per Capita pós-reforma (59.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('RDBPC_RBU' = RDB_RBU / Num_Moradores) %>% 
  mutate(RDB_RBU = case_when(V2005 != 1 ~ NA_real_,
                                TRUE ~ RDB_RBU))


###
### IMPOSTO TOTAL FINAL (60)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('ImpTot_F2' = Flat_Rate * Basecal3)

## Cálculo do imposto Domiciliar pós-reforma (60.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_1 = case_when(V2005 %in% c(17:19) ~ 0,
                             TRUE ~ ImpTot_F2)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('IR_RBU_Dom' = sum(Renda_1)) %>% 
  select(-Renda_1)

## Cálculo do imposto Domiciliar Per Capita pós-reforma (60.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('IR_RBU_PC' = IR_RBU_Dom / Num_Moradores) %>% 
  mutate(IR_RBU_Dom = case_when(V2005 != 1 ~ NA_real_,
                                TRUE ~ IR_RBU_Dom))


###
### RENDA DISPONÍVEL PÓS REFORMA - RID_RBU (61)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('RID2' = RII + RBU + Ben_Aju - Receita, 
         'RID_RBU' = RII + RBU + Ben_Aju - ImpTot_F2)

## Cálculo da Renda Domiciliar Disponível (61.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate(Renda_1 = case_when(V2005 %in% c(17:19) ~ 0,
                             TRUE ~ RID_RBU)) %>% 
  group_by(ID_DOMICILIO) %>% 
  mutate('RDD_RBU' = sum(Renda_1)) %>% 
  select(-Renda_1)

## Cálculo da Renda Domiciliar Disponível Per Capita (61.2)

Visita_5_MM = Visita_5_MM %>% 
  mutate('RDDPC_RBU' = RDD_RBU / Num_Moradores) %>% 
  mutate(RDD_RBU = case_when(V2005 != 1 ~ NA_real_,
                               TRUE ~ RDD_RBU))


###
### GANHOS E PERDAS (62)
###

Visita_5_MM = Visita_5_MM %>% 
  mutate('Ganhador' = case_when(RDDPC_RBU > RDDPC ~ 1,
                                 TRUE ~ 0), 
          'Ganho' = case_when(RDDPC_RBU > RDDPC ~ RDDPC_RBU - RDDPC, 
                              TRUE ~ 0), 
          'Perdedor' = case_when(RDDPC_RBU < RDDPC ~ 1,
                                 TRUE ~ 0), 
          'Perda' = case_when(RDDPC_RBU < RDDPC ~  RDDPC - RDDPC_RBU,  
                              TRUE ~ 0))



###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###                   INCORPORACAO DA AMOSTRA COMPLEXA                       ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###


###
### Ajustes (63)
###

## Contagem da populacao (63.1)

Visita_5_MM = Visita_5_MM %>% 
  mutate("Pop" = 1)

## Rotulando variaveis categoricas (63.2)

Amostra_Base = Visita_5_MM

Amostra_Base = pnadc_labeller(Amostra_Base, 
                              dictionary.file = 'dicionario_PNADC_microdados_2019_visita5_20210617.xls')

## Criando variavel de regioes (63.3)

Amostra_Base$UF = as.character(Amostra_Base$UF)

Amostra_Base = Amostra_Base %>% 
  mutate("Região" = case_when(UF == "Rondônia" ~ "Norte",
                              UF == "Acre" ~ "Norte",
                              UF == "Amazonas" ~ "Norte",
                              UF == "Roraima" ~ "Norte",
                              UF == "Pará" ~ "Norte",
                              UF == "Amapá" ~ "Norte",
                              UF == "Tocantins" ~ "Norte",
                              UF == "Maranhão" ~ "Nordeste",
                              UF == "Piauí" ~ "Nordeste",
                              UF == "Ceará" ~ "Nordeste",
                              UF == "Rio Grande do Norte" ~ "Nordeste",
                              UF == "Paraíba" ~ "Nordeste",
                              UF == "Pernambuco" ~ "Nordeste",
                              UF == "Alagoas" ~ "Nordeste",
                              UF == "Sergipe" ~ "Nordeste",
                              UF == "Bahia" ~ "Nordeste",
                              UF == "Minas Gerais" ~ "Sudeste",
                              UF == "Espírito Santo" ~ "Sudeste",
                              UF == "Rio de Janeiro" ~ "Sudeste",
                              UF == "São Paulo" ~ "Sudeste",
                              UF == "Paraná" ~ "Sul",
                              UF == "Santa Catarina" ~ "Sul",
                              UF == "Rio Grande do Sul" ~ "Sul",
                              UF == "Mato Grosso do Sul" ~ "Centro-Oeste",
                              UF == "Mato Grosso" ~ "Centro-Oeste",
                              UF == "Goiás" ~ "Centro-Oeste",
                              UF == "Distrito Federal" ~ "Centro-Oeste"))

## Amostra complexa (63.4)

Amostra_Complexa = svydesign(ids = ~UPA, strata = ~Estrato, weights = ~V1032,
                             data = Amostra_Base, nest = T)

## Amostra complexa - pobreza e desigualdade (63.5)

Amostra_Complexa2 = Amostra_Complexa %>% 
  convey::convey_prep()

## Limpando objeto temporario (63.6)

rm(Amostra_Base)



###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###                         RESULTADOS DAS SIMULACOES                        ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###


###
### VARIAVEIS DA PESQUISA E VARIAVEIS DERIVADAS (64)
###

## Rendimento bruto normal monetario - Trabalho Principal (64.1)

svytotal(x = ~V403312, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V403312 >= 0), na.rm = T)

## Rendimento bruto normal em produtos e mercadorias - Trabalho Principal (64.2)

svytotal(x = ~V403322, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V403322 >= 0), na.rm = T)

## Rendimento bruto normal monetario - Trabalho Secundario (64.3)

svytotal(x = ~V405012, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V405012 >= 0), na.rm = T)

## Rendimento bruto normal em produtos e mercadorias - Trabalho Secundario (64.4)

svytotal(x = ~V405022, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V405022 >= 0), na.rm = T)

## Rendimento bruto normal monetario - Outros Trabalhos (64.5)

svytotal(x = ~V405812, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V405812 >= 0), na.rm = T)

## Rendimento bruto normal em produtos e mercadorias - Outros Trabalhos (64.6)

svytotal(x = ~V405822, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V405822 >= 0), na.rm = T)

## Rendimento de BPC-LOAS (64.7)

svytotal(x = ~V5001A2, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V5001A2 >= 0), na.rm = T)

## Rendimento de Programa Bolsa Familia (64.8)

svytotal(x = ~V5002A2, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V5002A2 >= 0), na.rm = T)

## Rendimento de outros programas sociais (64.9)

svytotal(x = ~V5003A2, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V5003A2 >= 0), na.rm = T)

## Rendimento de aposentadoria ou pensao publica (64.10)

svytotal(x = ~V5004A2, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V5004A2 >= 0), na.rm = T)

## Rendimentos de pensÃ£o alimenticia, doaÃ§Ã£o ou mesada em dinheiro (64.11)

svytotal(x = ~V5006A2, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V5006A2 >= 0), na.rm = T)

## Rendimento de aluguel ou arrendamento (64.12)

svytotal(x = ~V5007A2, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, V5007A2 >= 0), na.rm = T)

## Rendimento mensal efetivo de todos os trabalhos para pessoas de 14 anos ou mais de idade (64.13)

svytotal(x = ~VD4020, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, VD4020 >= 0), na.rm = T)

## Rendimento mensal efetivo de todas as fontes (64.14)

svytotal(x = ~VD4022, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, VD4022 >= 0), na.rm = T)

## Rendimento efetivo recebido de programas sociais e outros (64.15)

svytotal(x = ~VD4047, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, VD4047 >= 0), na.rm = T)

## Rendimento efetivo recebido de outras fontes (64.16)

svytotal(x = ~VD4048, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, VD4048 >= 0), na.rm = T)

## RENDAS DIVERSAS - EXCLUSIVE RENDA MENSAL EM PRODUTOS E MERCADORIAS (64.17)

svytotal(x = ~Rendas_Diversas, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, Rendas_Diversas >= 0), na.rm = T)

## RENDA INDIVIDUAL - EXCLUSIVE RENDA MENSAL EM PRODUTOS E MERCADORIAS (64.19)

svytotal(x = ~Renda_Individual, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, Renda_Individual >= 0), na.rm = T)

## RENDA DOMICILIAR - EXCLUSIVE RENDA MENSAL EM PRODUTOS E MERCADORIAS - (64.20)

svytotal(x = ~Renda_Dom, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, Renda_Dom >= 0), na.rm = T)


###
### BENEFICIOS SOCIAIS (65)
###

## SALARIO FAMILIA (65.1)

svytotal(x = ~SalFam, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, SalFam > 0), na.rm = T)

## SEGURO DESEMPREGO (65.2)

svytotal(x = ~SegDemp, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, SegDemp > 0), na.rm = T)

## ABONO SALARIAL (65.3)

svytotal(x = ~AbonSal, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, AbonSal > 0), na.rm = T)

## ABONO ANUAL - DECIMO TERCEIRO SALARIO DO APOSENTADO E DO PENSIONISTA (65.4)

svytotal(x = ~Abono_A, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, Abono_A > 0), na.rm = T)

## BENEFICIO DE PRESTACAO CONTINUADA - BPC (65.5)

svytotal(x = ~BPC, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, BPC > 0), na.rm = T)

## BOLSA FAMILIA (65.6)

svytotal(x = ~BolsaFam, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, BolsaFam > 0), na.rm = T)

## DECIMO TERCEIRO SALARIO (65.7)

svytotal(x = ~Sal13, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, Sal13 > 0), na.rm = T)

## ADICIONAL DE FERIAS (65.8)

svytotal(x = ~A_Ferias, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, A_Ferias > 0), na.rm = T)



###
### CONTRIBUICOES SOCIAIS (66)
###

## EMPREGADO E TRABALHADOR AVULSO (66.1)

svytotal(x = ~Conte, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, Conte > 0), na.rm = T)

## EMPREGADO DOMESTICO (66.2)

svytotal(x = ~Conted, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, Conted > 0), na.rm = T)

## CONTRIBUINTE INDIVIDUAL (66.3)

svytotal(x = ~Contind, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, Contind > 0), na.rm = T)

## SERVIDOR PUBLICO ESTATUTARIO FEDERAL (66.4)

svytotal(x = ~ContFed, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, ContFed > 0), na.rm = T)

## SERVIDOR PUBLICO ESTATUTARIO ESTADUAL (66.5)

svytotal(x = ~ContEst, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, ContEst > 0), na.rm = T)

## SERVIDOR PUBLICO FEDERAL MILITAR (66.6)

svytotal(x = ~ContMil, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, ContMil > 0), na.rm = T)

## SERVIDOR PUBLICO MUNICIPAL (66.7)

svytotal(x = ~ContMun, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, ContMun > 0), na.rm = T)

## APOSENTADOS E PENSIONISTAS (66.8)

svytotal(x = ~ContAposent, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, ContAposent > 0), na.rm = T)


###
### IMPOSTO DE RENDA PESSOA FISICA 2020 - ANO BASE 2019 (67)
###


## CALCULO DO IMPOSTO - DECLARACAO COMPLETA (67.1)

svytotal(x = ~ImpTotc, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, ImpTotc > 0), na.rm = T)

## CALCULO DO IMPOSTO - DECLARACAO SIMPLIFICADA (67.2)

svytotal(x = ~ImpTots, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, ImpTots > 0), na.rm = T)


###
### INDICADORES DE POBREZA (68)
###

## Numero de Extremamente Pobres - Linha de Extrema pobreza do IPEA, R$ 422 (68.1)
# RDDPC = Renda Domiciliar Inicial Per Capita

svytotal(x = ~Pob_RDIPC, design = Amostra_Complexa, na.rm = T)

## Numero de Extremamente Pobres - Linha de Extrema Pobreza do IPEA, R$ 422 (68.2)
# RDDPC = Renda Domiciliar Bruta Per Capita

svytotal(x = ~Pob_RDBPC, design = Amostra_Complexa, na.rm = T)

## Numero de Extremamente Pobres - Linha de Extrema Pobreza do IPEA, R$ 422 (68.3)
# RDDPC = Renda Domiciliar Disponivel Per Capita

svytotal(x = ~Pob_RDDPC, design = Amostra_Complexa, na.rm = T)

## Hiato de Pobres - Linha de Extrema pobreza do IPEA (68.4)

svytotal(x = ~Hiato_RDIPC, design = Amostra_Complexa, na.rm = T)

## Hiato de Pobres - Linha de pobreza do IPEA (68.5)

svytotal(x = ~Hiato_RDBPC, design = Amostra_Complexa, na.rm = T)

## Hiato de Pobres - Linha de pobreza do IPEA (68.6)

svymean(x = ~Hiato_RDDPC, design = Amostra_Complexa, na.rm = T)

###
### SIMULACAO DA RBU - MODELO IMP. DE RENDA NEG. (69)
###

## Renda Basica Universal - RBU (69.1)

svytotal(x = ~RBU, design = Amostra_Complexa, na.rm = T)

## Transferencias (69.2)

svytotal(x = ~Transf, design = Amostra_Complexa, na.rm = T)

## Outras transferencias - excluindo aposentadoria e pensao + abono anual + BPC (69.3)

svytotal(x = ~Transf_Out, design = Amostra_Complexa, na.rm = T)

## Tributos diretos (69.4)

svytotal(x = ~Trib_D, design = Amostra_Complexa, na.rm = T)

## Beneficios ajustados - aposentadoria e pensao + BPC - RBU (69.5)

svytotal(x = ~Aposent_BPC, design = Amostra_Complexa, na.rm = T)

svytotal(x = ~Ben_Aju, design = Amostra_Complexa, na.rm = T)

# Numero de pessoas

svytotal(x = ~Pop, design = subset(Amostra_Complexa, Ben_Aju > 0), na.rm = T)

## Base de calculo do novo IRPF (69.6)

svytotal(x = ~Basecal3, design = Amostra_Complexa, na.rm = T)

## Receita tributaria (69.7)

svytotal(x = ~Receita, design = Amostra_Complexa, na.rm = T)

## Imposto total final (69.8)

svytotal(x = ~ImpTot_F2, design = Amostra_Complexa, na.rm = T)

## Renda disponivel pos-reforma (69.9)

svytotal(x = ~RID, design = Amostra_Complexa, na.rm = T)

svytotal(x = ~RID2, design = Amostra_Complexa, na.rm = T)

svytotal(x = ~RID_RBU, design = Amostra_Complexa, na.rm = T)

## Cálculo da Renda Domiciliar Disponível Per Capita (69.10)

svytotal(x = ~RDDPC_RBU, design = Amostra_Complexa, na.rm = T)

## Ganhos e perdas (69.11)

svytotal(x = ~Ganhador, design = Amostra_Complexa, na.rm = T)

svytotal(x = ~Ganho, design = Amostra_Complexa, na.rm = T)

svytotal(x = ~Perdedor, design = Amostra_Complexa, na.rm = T)

svytotal(x = ~Perda, design = Amostra_Complexa, na.rm = T)



###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###                        ORGANIZACAO DE RESULTADOS                         ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###


###
### RESULTADOS DA SIMULAÇÃO E COMPARACOES COM O SISTEMA VIGENTE (70)
###

## Efeitos Fiscais (70.1)

Orçamento_A = data.frame("Orçamento" = c("Renda Inicial", "Transferências", "Aposentadorias e Pensões", "Benefícios do Trabalho",
                                        "Bolsa Família", "Renda Bruta", "Tributos Diretos", "IRPF", "Contribuição Previdenciária",
                                        "Renda Disponível", "Custo Bruto da RBU", "Benefícios Ajustados", "Base de Cálculo",
                                        "Receita Fiscal sob RBU", "Renda Disponível sob RBU"),
                         "Centro-Oeste" = svytotal(~RII+Transf+Aposent_BPC+Ben_Trab+BolsaFam+RIB+Trib_D+ImpTots+Cont+RID+RBU+Ben_Aju+
                                               Basecal3+Receita+RID_RBU,
                                             design = subset(Amostra_Complexa, Região == "Centro-Oeste"),
                                             na.rm = T,
                                             estimate.only = F),
                         "Nordeste" = svytotal(~RII+Transf+Aposent_BPC+Ben_Trab+BolsaFam+RIB+Trib_D+ImpTots+Cont+RID+RBU+Ben_Aju+
                                               Basecal3+Receita+RID_RBU,
                                             design = subset(Amostra_Complexa, Região == "Nordeste"),
                                             na.rm = T,
                                             estimate.only = F),
                         "Norte" = svytotal(~RII+Transf+Aposent_BPC+Ben_Trab+BolsaFam+RIB+Trib_D+ImpTots+Cont+RID+RBU+Ben_Aju+
                                               Basecal3+Receita+RID_RBU,
                                             design = subset(Amostra_Complexa, Região == "Norte"),
                                             na.rm = T,
                                             estimate.only = F),
                         "Sudeste" = svytotal(~RII+Transf+Aposent_BPC+Ben_Trab+BolsaFam+RIB+Trib_D+ImpTots+Cont+RID+RBU+Ben_Aju+
                                              Basecal3+Receita+RID_RBU,
                                            design = subset(Amostra_Complexa, Região == "Sudeste"),
                                            na.rm = T,
                                            estimate.only = F),
                         "Sul" = svytotal(~RII+Transf+Aposent_BPC+Ben_Trab+BolsaFam+RIB+Trib_D+ImpTots+Cont+RID+RBU+Ben_Aju+
                                              Basecal3+Receita+RID_RBU,
                                            design = subset(Amostra_Complexa, Região == "Sul"),
                                            na.rm = T,
                                            estimate.only = F),
                         "Brasil" = svytotal(~RII+Transf+Aposent_BPC+Ben_Trab+BolsaFam+RIB+Trib_D+ImpTots+Cont+RID+RBU+Ben_Aju+
                                               Basecal3+Receita+RID_RBU,
                                             design = Amostra_Complexa)) %>% 
  transmute("Orçamento" = Orçamento, 
            "Centro-Oeste" = round((Centro.Oeste.total * 12) / 1000000000),
            "Nordeste" =  round((Nordeste.total * 12) / 1000000000),
            "Norte" = round((Norte.total * 12) / 1000000000),
            "Sudeste" = round((Sudeste.total * 12) / 1000000000),
            "Sul" = round((Sul.total * 12) / 1000000000),
            "Brasil" = round((Brasil.total * 12) / 1000000000))

# Tabela 1: efeitos orçamentários anuais

t1 = flextable::flextable(Orçamento_A) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  autofit(add_w = 0, add_h = 0) %>% 
  colformat_double(big.mark = " ", digits = 0)

save_as_pptx("Tabela 1: efeitos orçamentários" = t1,
             path = "C:/Users/chenr/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Tabela 1. Efeitos orçamentários anuais.pptx")

rm(t1, Orçamento_A)

## Efeitos Distributivos (70.2)

Orçamento_M = data.frame("Orçamento" = c("Renda inicial", "Transferências", "Aposentadorias e Pensões", 
                                        "Benefícios do trabalho", "PBF", "Renda bruta", "Tributos Diretos", 
                                        "IRPF", "Contribuição previdenciária", "Renda disponível"),
                         "Centro-Oeste" = svymean(~RDIPC+Transf_PC+Pensoes_PC+Ben_Trab_PC+PBF_PC+RDBPC+Trib_Pc+IRPF_PC+Cont_PC+RDDPC,
                                                   design = subset(Amostra_Complexa, Região == "Centro-Oeste"),
                                                   estimate.only = F),
                         "Nordeste" = svymean(~RDIPC+Transf_PC+Pensoes_PC+Ben_Trab_PC+PBF_PC+RDBPC+Trib_Pc+IRPF_PC+Cont_PC+RDDPC,
                                               design = subset(Amostra_Complexa, Região == "Nordeste"),
                                               estimate.only = F),
                         "Norte" = svymean(~RDIPC+Transf_PC+Pensoes_PC+Ben_Trab_PC+PBF_PC+RDBPC+Trib_Pc+IRPF_PC+Cont_PC+RDDPC,
                                            design = subset(Amostra_Complexa, Região == "Norte"),
                                            estimate.only = F),
                         "Sudeste" = svymean(~RDIPC+Transf_PC+Pensoes_PC+Ben_Trab_PC+PBF_PC+RDBPC+Trib_Pc+IRPF_PC+Cont_PC+RDDPC,
                                              design = subset(Amostra_Complexa, Região == "Sudeste"),
                                              estimate.only = F),
                         "Sul" = svymean(~RDIPC+Transf_PC+Pensoes_PC+Ben_Trab_PC+PBF_PC+RDBPC+Trib_Pc+IRPF_PC+Cont_PC+RDDPC,
                                          design = subset(Amostra_Complexa, Região == "Sul"),
                                          estimate.only = F),
                         "Brasil" = svymean(~RDIPC+Transf_PC+Pensoes_PC+Ben_Trab_PC+PBF_PC+RDBPC+Trib_Pc+IRPF_PC+Cont_PC+RDDPC,
                                             design = Amostra_Complexa,
                                            estimate.only = F)) %>% 
  transmute("Orçamento" = Orçamento,
            "Centro-Oeste" = round(Centro.Oeste.mean),
            "Nordeste" = round(Nordeste.mean),
            "Norte" = round(Norte.mean),
            "Sudeste" = round(Sudeste.mean),
            "Sul" = round(Sul.mean),
            "Brasil" = round(Brasil.mean))

# Tabela 2: efeitos orçamentários per capita (mensais)

t2 = flextable::flextable(Orçamento_M) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  style(i = c(1, 2, 6, 7, 10), j = 1, pr_t = fp_text_default(bold = T)) %>% 
  autofit(add_w = 0, add_h = 0) %>% 
  colformat_double(big.mark = " ", digits = 0)

save_as_pptx("Tabela 2: efeitos orçamentários" = t2,
             path = "C:/Users/chenr/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Tabela 2. Efeitos orçamentários per capita.pptx")

rm(t2)

## Medindo efeito de impostos e beneficios (70.3)

Rendas = Orçamento_M %>% 
  filter(Orçamento == "Renda inicial" | Orçamento == "Renda bruta" | Orçamento == "Renda disponível") %>% 
  pivot_longer(cols = c(`Centro-Oeste`, Nordeste, Norte, Sudeste, Sul, Brasil)) %>% 
  rename(Região = name,
         Valor = value)

ri = Rendas %>% filter(Orçamento == "Renda inicial")

rb = Rendas %>% filter(Orçamento == "Renda bruta")

rd = Rendas %>% filter(Orçamento == "Renda disponível")

df1 = c(ri$Valor)

df2 = c(rb$Valor)

df3 = c(rd$Valor)

re = Rendas[1:6, 2]

# Grafico 1: Efeito de impostos e beneficios na RDBPC

g1 = ggplot(data = re, aes(x = Região)) + 
  geom_bar(mapping = aes(y = as.numeric(df1), fill = 'df1'), 
           stat = "identity", 
           width = .30, 
           position = position_nudge(x = -.15)) + 
  geom_bar(mapping = aes(y = as.numeric(df2), fill = 'df2'), 
           stat = "identity", 
           width = .30, 
           position = position_nudge(x = .15)) + 
  geom_bar(mapping = aes(y =  as.numeric(df3), fill = 'df3'), 
           stat = "identity", 
           width = .30, 
           position = position_nudge(x = .45)) + 
  theme_gray() + 
  scale_y_continuous(breaks = seq(0, 2000, 250), limits = c(0, 2000)) +
  scale_fill_manual(labels = c("Renda Inicial", "Renda Bruta", "Renda Disponível"),
                    values = c('df1' = "deepskyblue4", 'df2' = "brown1", 'df3' = "aquamarine4")) + 
  labs(y = "R$ mensais de 2019",
       fill = " ") + 
  theme(legend.position = "top",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12, margin = margin(r = 7)))

g1

ggsave(filename = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Gráfico 1.Efeito de impostos e beneficios na RDBPC.png",
       plot = g1,
       units = "in",
       dpi = 600)

rm(Rendas, g1, re, df1, df2, df3, rb, ri, rd)

## Composição da RDBPC - em % - (70.4)

Comp = Orçamento_M %>% 
  filter(Orçamento == "Renda inicial" | 
           Orçamento == "Aposentadorias e Pensões" | 
           Orçamento == "Benefícios do trabalho" | 
           Orçamento == "PBF" | 
           Orçamento == "Renda bruta")

Comp =as.data.frame(t(Comp))

Comp = Comp[-1, ]

Comp = data.frame("Região" = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil"), 
                    "Renda incial" = round(as.numeric(Comp$RDIPC) / as.numeric(Comp$RDBPC) * 100),
                    "Aposentadorias e pensões" = round(as.numeric(Comp$Pensoes_PC) / as.numeric(Comp$RDBPC) * 100),
                    "Benefícios do trabalho" = round(as.numeric(Comp$Ben_Trab_PC) / as.numeric(Comp$RDBPC) * 100),
                    "PBF" = round(as.numeric(Comp$PBF_PC) / as.numeric(Comp$RDBPC) * 100))

colnames(Comp) = c('Região', 'Renda incial', 'Aposentadorias e pensões', 'Benefícios do trabalho', 'PBF')

Comp = Comp %>% 
  pivot_longer(cols = c(`Renda incial`, `Aposentadorias e pensões`, `Benefícios do trabalho`, PBF)) %>% 
  rename(Comp = name,
         Valor = value)

Comp = Comp %>% 
  mutate(Valor = case_when(Região == "Nordeste" & Comp == "Aposentadorias e pensões" ~ 28,
                           Região == "Norte" & Comp == "Aposentadorias e pensões" ~ 21,
                           Região == "Sul" & Comp == "Aposentadorias e pensões" ~ 24,
                           Região == "Brasil" & Comp == "Renda incial" ~ 74,
                           TRUE ~ Valor))

# Grafico 2: Composição da RDBPC - em %

g2 = ggplot(Comp) +
  theme_gray() +
  geom_tile(aes(Região, Valor, fill = Comp, width = .8, height = 1.5)) +
  labs(y = "%",
       fill = " ") +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  theme(legend.position = "top",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12, margin = margin(r = 7)))

g2

ggsave(filename = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Gráfico 2.Composição da RDBPC.png",
       plot = g2,
       units = "in",
       dpi = 600)

rm(Comp, g2)

## Tributos como Proporção da RDBPC (70.5)

trib = Orçamento_M %>% 
  filter(Orçamento == "IRPF" | 
           Orçamento == "Contribuição previdenciária" | 
           Orçamento == "Renda bruta")

trib = as.data.frame(t(trib))

trib = trib[-1, ]

trib = data.frame("Região" = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil"), 
                    "IRPF" = round(as.numeric(trib$IRPF_PC) / as.numeric(trib$RDBPC) * 100),
                    "Contribuição previdenciária" = round(as.numeric(trib$Cont_PC) / as.numeric(trib$RDBPC) * 100))

colnames(trib) = c('Região', 'IRPF', 'Contribuição previdenciária')

trib = trib %>% 
  pivot_longer(cols = c(IRPF, `Contribuição previdenciária`)) %>% 
  rename(Comp = name,
         Valor = value)

# Grafico 3: Tributos como proporcao da RDBPC - em %

g3 = ggplot(trib, aes(Região, Valor, fill = Comp)) +
  theme_gray() +
  geom_tile(aes(width = .8, height = .4)) +
  labs(y = "%",
       fill = " ") +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  scale_y_continuous(breaks = seq(2, 8, 1), limits = c(2, 8)) +
  theme(legend.position = "top",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12, margin = margin(r = 7)))

g3

ggsave(filename = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Gráfico 3.Tributos como proporção da RDBPC.png",
       plot = g3,
       units = "in",
       dpi = 600)

rm(trib, Orçamento_M, g3)


###
### RENDAS PER CAPITA (71)
###

Rendas = data.frame("Rendas" = c("RDIPC", "RDBSBFPC", "RDBPC", "RDDSBFPC", "RDDPC", "RDDPC_RBU"),
                    "Centro-Oeste" = svymean(~RDIPC+RDBSBFPC+RDBPC+RDDSBFPC+RDDPC+RDDPC_RBU, 
                                             design = subset(Amostra_Complexa, Região == "Centro-Oeste")),
                    "Nordeste" = svymean(~RDIPC+RDBSBFPC+RDBPC+RDDSBFPC+RDDPC+RDDPC_RBU, 
                                         design = subset(Amostra_Complexa, Região == "Nordeste")),
                    "Norte" = svymean(~RDIPC+RDBSBFPC+RDBPC+RDDSBFPC+RDDPC+RDDPC_RBU, 
                                      design = subset(Amostra_Complexa, Região == "Norte")),
                    "Sudeste" = svymean(~RDIPC+RDBSBFPC+RDBPC+RDDSBFPC+RDDPC+RDDPC_RBU, 
                                        design = subset(Amostra_Complexa, Região == "Sudeste")),
                    "Sul" = svymean(~RDIPC+RDBSBFPC+RDBPC+RDDSBFPC+RDDPC+RDDPC_RBU, 
                                    design = subset(Amostra_Complexa, Região == "Sul")),
                    "Brasil" = svymean(~RDIPC+RDBSBFPC+RDBPC+RDDSBFPC+RDDPC+RDDPC_RBU, 
                                    design = Amostra_Complexa)) %>% 
  transmute("Indicador" = Rendas,
            "Centro-Oeste" = round(Centro.Oeste.mean),
            "Nordeste" = round(Nordeste.mean),
            "Norte" = round(Norte.mean),
            "Sudeste" = round(Sudeste.mean),
            "Sul" = round(Sul.mean),
            "Brasil" = round(Brasil.mean))

RDPC = as.data.frame(t(Rendas))

RDPC = RDPC %>% 
  select(RDDPC, RDDPC_RBU)

RDPC = RDPC[-1, ]

RDPC = RDPC %>% 
  mutate("Região" = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil"),
         .before = 1)

RDPC = RDPC %>% 
  mutate("Variação" = round(((as.numeric(RDDPC_RBU) / as.numeric(RDDPC)) - 1) * 100, digits = 2))

RDPC = as.data.frame(t(RDPC))

RDPC = RDPC[-1, ]

RDPC = RDPC %>% 
  mutate(" " = c("Renda Disponível (Vig.)", "Renda Disponível (Ref.)", "Variação"),
         .before = 1)


# Tabela 3: Rendas Per Capita

t3 = flextable::flextable(RDPC) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  align(part = "body", align = "right") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  colformat_double(i = c(2, 3), big.mark = " ", digits = 0) %>% 
  autofit(add_w = 0, add_h = 0)

save_as_pptx("Tabela 3: Rendas domiciliares per capita médias" = t3,
             path = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Tabela 3.Rendas domiciliares per capita médias.pptx")

rm(RDPC, t3)


###
### INDICADORES DE POBREZA (72)
###

## Medidas de pobreza (72.1)
# Impacto com Bolsa-Familia - (Pob_RDIPC; Pob_RDBPC; Pob_RDDPC)
# Impacto sem Bolsa-Familia - (Pob_RDBSBFPC; Pob_RDDSBFPC)

Pob = data.frame("Indicador" = c("População", "Pobres_RDIPC", "Pobres_RDBSBFPC", "Pobres_RDBPC",
                                 "Pobres_RDDSBFPC", "Pobres_RDDPC"),
                 "Centro-Oeste" = svytotal(~Pop+Pob_RDIPC+Pob_RDBSBFPC+Pob_RDBPC+Pob_RDDSBFPC+Pob_RDDPC,
                                           design = subset(Amostra_Complexa, Região == "Centro-Oeste")),
                 "Nordeste" = svytotal(~Pop+Pob_RDIPC+Pob_RDBSBFPC+Pob_RDBPC+Pob_RDDSBFPC+Pob_RDDPC,
                                       design = subset(Amostra_Complexa, Região == "Nordeste")),
                 "Norte" = svytotal(~Pop+Pob_RDIPC+Pob_RDBSBFPC+Pob_RDBPC+Pob_RDDSBFPC+Pob_RDDPC,
                                    design = subset(Amostra_Complexa, Região == "Norte")),
                 "Sudeste" = svytotal(~Pop+Pob_RDIPC+Pob_RDBSBFPC+Pob_RDBPC+Pob_RDDSBFPC+Pob_RDDPC,
                                      design = subset(Amostra_Complexa, Região == "Sudeste")),
                 "Sul" = svytotal(~Pop+Pob_RDIPC+Pob_RDBSBFPC+Pob_RDBPC+Pob_RDDSBFPC+Pob_RDDPC,
                                  design = subset(Amostra_Complexa, Região == "Sul")),
                 "Brasil" = svytotal(~Pop+Pob_RDIPC+Pob_RDBSBFPC+Pob_RDBPC+Pob_RDDSBFPC+Pob_RDDPC,
                                     design = Amostra_Complexa)) %>% 
  transmute("Indicador" = Indicador, 
            "Centro-Oeste" = as.numeric(Centro.Oeste.total),
            "Nordeste" =  as.numeric(Nordeste.total),
            "Norte" = as.numeric(Norte.total),
            "Sudeste" = as.numeric(Sudeste.total),
            "Sul" = as.numeric(Sul.total),
            "Brasil" = as.numeric(Brasil.total))

## Hiato de pobreza (72.2)
# Impacto com Bolsa-Familia - (Pob_RDIPC; Pob_RDBPC; Pob_RDDPC)
# Impacto sem Bolsa-Familia - (Pob_RDBSBFPC; Pob_RDDSBFPC)

Pob2 = data.frame("Indicador" = c( "Hiato_RDIPC", "Hiato_RDBSBFPC", "Hiato_RDBPC", "Hiato_RDDSBFPC", "Hiato_RDDPC"),
                 "Centro-Oeste" = svymean(~Hiato_RDIPC+Hiato_RDBSBFPC+Hiato_RDBPC+Hiato_RDDSBFPC+Hiato_RDDPC,
                                           design = subset(Amostra_Complexa, Região == "Centro-Oeste"),
                                           na.rm = T),
                 "Nordeste" = svymean(~Hiato_RDIPC+Hiato_RDBSBFPC+Hiato_RDBPC+Hiato_RDDSBFPC+Hiato_RDDPC,
                                       design = subset(Amostra_Complexa, Região == "Nordeste"),
                                      na.rm = T),
                 "Norte" = svymean(~Hiato_RDIPC+Hiato_RDBSBFPC+Hiato_RDBPC+Hiato_RDDSBFPC+Hiato_RDDPC,
                                    design = subset(Amostra_Complexa, Região == "Norte"),
                                   na.rm = T),
                 "Sudeste" = svymean(~Hiato_RDIPC+Hiato_RDBSBFPC+Hiato_RDBPC+Hiato_RDDSBFPC+Hiato_RDDPC,
                                      design = subset(Amostra_Complexa, Região == "Sudeste"),
                                     na.rm = T),
                 "Sul" = svymean(~Hiato_RDIPC+Hiato_RDBSBFPC+Hiato_RDBPC+Hiato_RDDSBFPC+Hiato_RDDPC,
                                  design = subset(Amostra_Complexa, Região == "Sul"),
                                 na.rm = T),
                 "Brasil" = svymean(~Hiato_RDIPC+Hiato_RDBSBFPC+Hiato_RDBPC+Hiato_RDDSBFPC+Hiato_RDDPC,
                                     design = Amostra_Complexa,
                                    na.rm = T)) %>% 
  transmute("Indicador" = Indicador, 
            "Centro-Oeste" = as.numeric(round(Centro.Oeste.mean)),
            "Nordeste" =  as.numeric(round(Nordeste.mean)),
            "Norte" = as.numeric(round(Norte.mean)),
            "Sudeste" = as.numeric(round(Sudeste.mean)),
            "Sul" = as.numeric(round(Sul.mean)),
            "Brasil" = as.numeric(round(Brasil.mean)))


## Sumarizando estatisticas (72.3)

Pobreza = rbind.data.frame(Pob, Pob2)

rm(Pob, Pob2)

Pobreza = as.data.frame(t(Pobreza))

Pobreza = Pobreza[-1, ]

Pobreza = Pobreza %>% 
  mutate("Região" = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil"), .before = 1)

# Tabela 4: Indicadores de pobreza - sem PBF
# Tabela 5: Indicadores de pobreza - com PBF

t5 = data.frame("Região" = Pobreza$Região,
                     "População" = as.numeric(Pobreza$Pop),
                     "Pop (%)" = round((as.numeric(Pobreza$Pop) / 209496463) * 100, digits = 1),
                     "Pobres" = as.numeric(Pobreza$Pob_RDDPC),
                     "Pob. (%)" = round((as.numeric(Pobreza$Pob_RDDPC) / 45735529) * 100, digits = 1),
                     "RDDPC" = as.numeric(RDPC$RDDPC),
                     "Renda (%)" = round((as.numeric(RDPC$RDDPC) / 1319) * 100, digits = 1),
                     "Hiato de renda (R$)" = as.numeric(Pobreza$Hiato_RDDPC),
                     "Hiato (%)" = round((as.numeric(Pobreza$Hiato_RDDPC) / 196) * 100, digits = 1)) %>% 
  flextable::flextable() %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  colformat_double(j = c(2, 4, 6), big.mark = " ", digits = 0) %>% 
  colformat_double(j = c(3, 5, 7, 9), big.mark = ",", decimal.mark = ",", digits = 1) %>% 
  autofit(add_w = 0, add_h = 0)

t5

save_as_pptx("Tabela 5: Indicadores de pobreza - com PBF" = t5,
             path = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Tabela 5.Indicadores de pobreza com PBF.pptx")

rm(t4, t5)

## Anexo 1: Estatisticas de pobreza (72.4)

anexo1 = rbind.data.frame(Rendas, Pob2, Pob) %>% 
  flextable::flextable() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  colformat_double(j = c(2:7), big.mark = " ", digits = 0) %>% 
  colformat_double(i = c(1:11), prefix = "R$ ", digits = 0) %>% 
  colformat_double(i = c(1:6), prefix = "R$ ", big.mark = " ", digits = 0) %>% 
  autofit(add_w = 0, add_h = 0)

anexo1

save_as_pptx("Anexo 1: Estatisticas de pobreza" = anexo1,
             path = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Anexo 1.Estatisticas de pobreza.pptx")

rm(anexo1, Pob, Pob2, Rendas)


## Índice de Foster, Greer e Thorbecke - FGT (72.5)
# FGT - a = 0
# a var RDDSBFPC mede o impacto desconsiderando o Bolsa-Familia
# Enquanto a var RDDPC considera o BF

FGT = as.data.frame(t(data.frame("Centro-Oeste" = svyfgt(~RDDPC, 
                               subset(Amostra_Complexa2, 
                               Região == "Centro-Oeste"),
                               g = 0,
                               abs_thresh = 422,
                               na.rm = T),
                 "Nordeste" = svyfgt(~RDDPC,
                               subset(Amostra_Complexa2, 
                               Região == "Nordeste"),
                               g = 0,
                               abs_thresh = 422,
                               na.rm = T),
                 "Norte" = svyfgt(~RDDPC,
                               subset(Amostra_Complexa2, 
                               Região == "Norte"),
                               g = 0,
                               abs_thresh = 422,
                               na.rm = T),
                 "Sudeste" = svyfgt(~RDDPC,
                               subset(Amostra_Complexa2, 
                               Região == "Sudeste"),
                               g = 0,
                               abs_thresh = 422,
                               na.rm = T),
                 "Sul" = svyfgt(~RDDPC,
                                subset(Amostra_Complexa2, 
                                Região == "Sul"),
                                g = 0,
                                abs_thresh = 422,
                                na.rm = T),
                 "Brasil" = svyfgt(~RDDPC,
                                   Amostra_Complexa2, 
                                   g = 0,
                                   abs_thresh = 422,
                                   na.rm = T)) %>% 
  select(Centro.Oeste.fgt0, Nordeste.fgt0, Norte.fgt0, Sudeste.fgt0, Sul.fgt0, Brasil.fgt0)))

# FGT - a = 1

FGT1 = as.data.frame(t(data.frame("Centro-Oeste" = svyfgt(~RDDPC, 
                                         subset(Amostra_Complexa2, 
                                                Região == "Centro-Oeste"),
                                         g = 1,
                                         abs_thresh = 422,
                                         na.rm = T),
                 "Nordeste" = svyfgt(~RDDPC,
                                     subset(Amostra_Complexa2, 
                                            Região == "Nordeste"),
                                     g = 1,
                                     abs_thresh = 422,
                                     na.rm = T),
                 "Norte" = svyfgt(~RDDPC,
                                  subset(Amostra_Complexa2, 
                                         Região == "Norte"),
                                  g = 1,
                                  abs_thresh = 422,
                                  na.rm = T),
                 "Sudeste" = svyfgt(~RDDPC,
                                    subset(Amostra_Complexa2, 
                                           Região == "Sudeste"),
                                    g = 1,
                                    abs_thresh = 422,
                                    na.rm = T),
                 "Sul" = svyfgt(~RDDPC,
                                subset(Amostra_Complexa2, 
                                       Região == "Sul"),
                                g = 1,
                                abs_thresh = 422,
                                na.rm = T),
                 "Brasil" = svyfgt(~RDDPC,
                                   Amostra_Complexa2, 
                                   g = 1,
                                   abs_thresh = 422,
                                   na.rm = T)) %>% 
  select(Centro.Oeste.fgt1, Nordeste.fgt1, Norte.fgt1, Sudeste.fgt1, Sul.fgt1, Brasil.fgt1)))

# FGT - a = 2

FGT2 = as.data.frame(t(data.frame("Centro-Oeste" = svyfgt(~RDDPC, 
                                         subset(Amostra_Complexa2, 
                                                Região == "Centro-Oeste"),
                                         g = 2,
                                         abs_thresh = 422,
                                         na.rm = T),
                 "Nordeste" = svyfgt(~RDDPC,
                                     subset(Amostra_Complexa2, 
                                            Região == "Nordeste"),
                                     g = 2,
                                     abs_thresh = 422,
                                     na.rm = T),
                 "Norte" = svyfgt(~RDDPC,
                                  subset(Amostra_Complexa2, 
                                         Região == "Norte"),
                                  g = 2,
                                  abs_thresh = 422,
                                  na.rm = T),
                 "Sudeste" = svyfgt(~RDDPC,
                                    subset(Amostra_Complexa2, 
                                           Região == "Sudeste"),
                                    g = 2,
                                    abs_thresh = 422,
                                    na.rm = T),
                 "Sul" = svyfgt(~RDDPC,
                                subset(Amostra_Complexa2, 
                                       Região == "Sul"),
                                g = 2,
                                abs_thresh = 422,
                                na.rm = T),
                 "Brasil" = svyfgt(~RDDPC,
                                   Amostra_Complexa2, 
                                   g = 2,
                                   abs_thresh = 422,
                                   na.rm = T)) %>% 
  select(Centro.Oeste.fgt2, Nordeste.fgt2, Norte.fgt2, Sudeste.fgt2, Sul.fgt2, Brasil.fgt2)))

## FGT sumarizado (72.6)

fgt = data.frame("Região" = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil"),
                 "FGT (0)" = round(FGT$RDDPC, digits = 4),
                 "FGT (1)" = round(FGT1$RDDPC, digits = 4),
                 "FGT (2)" = round(FGT2$RDDPC, digits = 4),
                 "Hiato/RDDPC" = round(as.numeric(Pobreza$Hiato_RDDPC) / as.numeric(RDPC$RDDPC), digits = 4))

colnames(fgt) = c("Região", "FGT (0)", "FGT (1)", "FGT (2)", "Hiato/RDDPC")

# Tabela 6: Indices de pobreza sem Bolsa Familia
# Tabela 7: Indices de pobreza com Bolsa Familia

t7 = flextable::flextable(fgt) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  colformat_double(big.mark = ",", decimal.mark = ",", digits = 4) %>% 
  autofit(add_w = 0, add_h = 0)

t7

save_as_pptx("Tabela 6: Indices de pobreza com Bolsa Familia" = t7,
             path = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Tabela 7.Indices de pobreza com Bolsa Familia.pptx")

rm(t6, t7, fgt, RDPC, FGT, FGT1, FGT2, Pobreza)



###
### INDICES DE DESIGUALDADE (73)
###

## CURVAS DE LORENZ (73.1)
# Curva de Lorenz da RDIPC no sistema vigente

RDIPC_Lorenz = survey::svyby(~RDIPC,
                                by = ~Região,
                                design = Amostra_Complexa2,
                                convey::svylorenz,
                                na.rm = T)

RDIPC_Lorenz = as.data.frame(t(RDIPC_Lorenz))

RDIPC_Lorenz = RDIPC_Lorenz[2:12, ]

# Curva de Lorenz da RDBPC no sistema vigente

RDBPC_Lorenz = survey::svyby(~RDBPC,
                                by = ~Região,
                                design = Amostra_Complexa2,
                                convey::svylorenz,
                                na.rm = T)

RDBPC_Lorenz = as.data.frame(t(RDBPC_Lorenz))

RDBPC_Lorenz = RDBPC_Lorenz[2:12, ]

# Curva de Lorenz da RDDPC no sistema vigente

RDDPC_Lorenz = survey::svyby(~RDDPC,
                                by = ~Região,
                                design = Amostra_Complexa2,
                                convey::svylorenz,
                                na.rm = T)

RDDPC_Lorenz = as.data.frame(t(RDDPC_Lorenz))

RDDPC_Lorenz = RDDPC_Lorenz[2:12, ]

# Curva de Lorenz da RDDPC_RBU no sistema reformado

RDDPC_RBU_Lorenz = survey::svyby(~RDDPC_RBU,
                                 by = ~Região,
                                 design = Amostra_Complexa2,
                                 convey::svylorenz,
                                 na.rm = T)

RDDPC_RBU_Lorenz = as.data.frame(t(RDDPC_RBU_Lorenz))

RDDPC_RBU_Lorenz = RDDPC_RBU_Lorenz[2:12, ]

## Curvas de Lorenz - Regiões (73.2)
# Centro-Oeste - Rendas domiciliares per capita

CO = data.frame("Dist_Pop" = seq(0, 1, .1),
                "RDIPC" = round(as.numeric(RDIPC_Lorenz[ ,1]), digits = 4),
                "RDBPC" = round(as.numeric(RDBPC_Lorenz[ ,1]), digits = 4),
                "RDDPC" = round(as.numeric(RDDPC_Lorenz[ ,1]), digits = 4),
                "RDDPC_RBU" = round(as.numeric(RDDPC_RBU_Lorenz[ ,1]), digits = 4))

# Nordeste - Rendas domiciliares per capita

NE = data.frame("Dist_Pop" = seq(0, 1, .1),
                "RDIPC" = round(as.numeric(RDIPC_Lorenz[ ,2]), digits = 4),
                "RDBPC" = round(as.numeric(RDBPC_Lorenz[ ,2]), digits = 4),
                "RDDPC" = round(as.numeric(RDDPC_Lorenz[ ,2]), digits = 4),
                "RDDPC_RBU" = round(as.numeric(RDDPC_RBU_Lorenz[ ,2]), digits = 4))

# Norte - Rendas domiciliares per capita

NO = data.frame("Dist_Pop" = seq(0, 1, .1),
                "RDIPC" = round(as.numeric(RDIPC_Lorenz[ ,3]), digits = 4),
                "RDBPC" = round(as.numeric(RDBPC_Lorenz[ ,3]), digits = 4),
                "RDDPC" = round(as.numeric(RDDPC_Lorenz[ ,3]), digits = 4),
                "RDDPC_RBU" = round(as.numeric(RDDPC_RBU_Lorenz[ ,3]), digits = 4))

# Sudeste - Rendas domiciliares - per capita

SD = data.frame("Dist_Pop" = seq(0, 1, .1),
                "RDIPC" = round(as.numeric(RDIPC_Lorenz[ ,4]), digits = 4),
                "RDBPC" = round(as.numeric(RDBPC_Lorenz[ ,4]), digits = 4),
                "RDDPC" = round(as.numeric(RDDPC_Lorenz[ ,4]), digits = 4),
                "RDDPC_RBU" = round(as.numeric(RDDPC_RBU_Lorenz[ ,4]), digits = 4))

# Sul - Rendas domiciliares - per capita

SUL = data.frame("Dist_Pop" = seq(0, 1, .1),
                 "RDIPC" = round(as.numeric(RDIPC_Lorenz[ ,5]), digits = 4),
                 "RDBPC" = round(as.numeric(RDBPC_Lorenz[ ,5]), digits = 4),
                 "RDDPC" = round(as.numeric(RDDPC_Lorenz[ ,5]), digits = 4),
                 "RDDPC_RBU" = round(as.numeric(RDDPC_RBU_Lorenz[ ,5]), digits = 4))

# Limpeza

rm(RDIPC_Lorenz, RDBPC_Lorenz, RDDPC_Lorenz, RDDPC_RBU_Lorenz)

## Plotagem das curvas - Regiões (73.4)

L1 = ggplot(CO, aes(x = Dist_Pop)) + 
  theme_bw() + 
  geom_abline() + 
  geom_point(aes(y = RDIPC, col = "A"), size = 2) +
  geom_line(aes(y = RDIPC, col = "A"), size = 1) +
  geom_point(aes(y = RDBPC, col = "B"), size = 2) +
  geom_line(aes(y = RDBPC, col = "B"), size = 1) +
  geom_point(aes(y = RDDPC, col = "C"), size = 2) +
  geom_line(aes(y = RDDPC, col = "C"), size = 1) +
  geom_point(aes(y = RDDPC_RBU, col = "D"), size = 2) +
  geom_line(aes(y = RDDPC_RBU, col = "D"), size = 1) +
  labs(title = "Centro-Oeste",
       y = "Participação na renda domiciliar",
       x = " ") + 
  scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("RDIPC", "RDBPC", "RDDPC", "RDDPC sob RBU"),
                     values = c("A" = "blue", "B" = "aquamarine4", "C" = "red", "D" = "deepskyblue4"))

L2 = ggplot(NE, aes(x = Dist_Pop)) + 
  theme_bw() + 
  geom_abline() + 
  geom_point(aes(y = RDIPC, col = "A"), size = 2) +
  geom_line(aes(y = RDIPC, col = "A"), size = 1) +
  geom_point(aes(y = RDBPC, col = "B"), size = 2) +
  geom_line(aes(y = RDBPC, col = "B"), size = 1) +
  geom_point(aes(y = RDDPC, col = "C"), size = 2) +
  geom_line(aes(y = RDDPC, col = "C"), size = 1) +
  geom_point(aes(y = RDDPC_RBU, col = "D"), size = 2) +
  geom_line(aes(y = RDDPC_RBU, col = "D"), size = 1) +
  labs(title = "Nordeste",
       y = "Participação na renda domiciliar",
       x = " ") + 
  scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("RDIPC", "RDBPC", "RDDPC", "RDDPC sob RBU"),
                     values = c("A" = "blue", "B" = "aquamarine4", "C" = "red", "D" = "deepskyblue4"))

L3 = ggplot(NO, aes(x = Dist_Pop)) + 
  theme_bw() + 
  geom_abline() + 
  geom_point(aes(y = RDIPC, col = "A"), size = 2) +
  geom_line(aes(y = RDIPC, col = "A"), size = 1) +
  geom_point(aes(y = RDBPC, col = "B"), size = 2) +
  geom_line(aes(y = RDBPC, col = "B"), size = 1) +
  geom_point(aes(y = RDDPC, col = "C"), size = 2) +
  geom_line(aes(y = RDDPC, col = "C"), size = 1) +
  geom_point(aes(y = RDDPC_RBU, col = "D"), size = 2) +
  geom_line(aes(y = RDDPC_RBU, col = "D"), size = 1) +
  labs(title = "Norte",
       y = "Participação na renda domiciliar",
       x = " ") + 
  scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("RDIPC", "RDBPC", "RDDPC", "RDDPC sob RBU"),
                     values = c("A" = "blue", "B" = "aquamarine4", "C" = "red", "D" = "deepskyblue4"))

L4 = ggplot(SD, aes(x = Dist_Pop)) + 
  theme_bw() + 
  geom_abline() + 
  geom_point(aes(y = RDIPC, col = "A"), size = 2) +
  geom_line(aes(y = RDIPC, col = "A"), size = 1) +
  geom_point(aes(y = RDBPC, col = "B"), size = 2) +
  geom_line(aes(y = RDBPC, col = "B"), size = 1) +
  geom_point(aes(y = RDDPC, col = "C"), size = 2) +
  geom_line(aes(y = RDDPC, col = "C"), size = 1) +
  geom_point(aes(y = RDDPC_RBU, col = "D"), size = 2) +
  geom_line(aes(y = RDDPC_RBU, col = "D"), size = 1) +
  labs(title = "Sudeste",
       y = "Participação na renda domiciliar",
       x = " ") + 
  scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("RDIPC", "RDBPC", "RDDPC", "RDDPC sob RBU"),
                     values = c("A" = "blue", "B" = "aquamarine4", "C" = "red", "D" = "deepskyblue4"))

L5 = ggplot(SUL, aes(x = Dist_Pop)) + 
  theme_bw() + 
  geom_abline() + 
  geom_point(aes(y = RDIPC, col = "A"), size = 2) +
  geom_line(aes(y = RDIPC, col = "A"), size = 1) +
  geom_point(aes(y = RDBPC, col = "B"), size = 2) +
  geom_line(aes(y = RDBPC, col = "B"), size = 1) +
  geom_point(aes(y = RDDPC, col = "C"), size = 2) +
  geom_line(aes(y = RDDPC, col = "C"), size = 1) +
  geom_point(aes(y = RDDPC_RBU, col = "D"), size = 2) +
  geom_line(aes(y = RDDPC_RBU, col = "D"), size = 1) +
  labs(title = "Sul",
       y = "Participação na renda domiciliar",
       x = " ",
       color = " ") + 
  scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  theme(legend.position = "bottom",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("RDIPC", "RDBPC", "RDDPC", "RDDPC sob RBU"),
                     values = c("A" = "blue", "B" = "aquamarine4", "C" = "red", "D" = "deepskyblue4"))


## Curvas de Lorenz - Brasil (73.5)
# Curva de Lorenz da RDIPC no sistema vigente

RDIPC_Lorenz = svylorenz(~RDIPC, 
                            Amostra_Complexa2, 
                            quantiles = seq(0, 1, .1),
                            plot = T,
                            curve.col = "chartreuse4",
                            na.rm = T)

# Curva de Lorenz da RDBPC no sistema vigente

RDBPC_Lorenz = svylorenz(~RDBPC, 
                            Amostra_Complexa2, 
                            quantiles = seq(0, 1, .1),
                            plot = T,
                            curve.col = "aquamarine4",
                            na.rm = T)

# Curva de Lorenz da RDDPC no sistema vigente

RDDPC_Lorenz = svylorenz(~RDDPC, 
                            Amostra_Complexa2, 
                            quantiles = seq(0, 1, .1),
                            plot = T,
                            curve.col = "deepskyblue4",
                            na.rm = T)

# Curva de Lorenz da RDDPC_RBU no sistema reformado

RDDPC_RBU_Lorenz = svylorenz(~RDDPC_RBU, 
                             Amostra_Complexa2, 
                             quantiles = seq(0, 1, .1),
                             plot = T,
                             curve.col = "goldenrod4",
                             na.rm = T)

## Curva de Lorenz - Brasil - Rendas domiciliares per capita (73.6)

Brasil = data.frame("Dist_Pop" = seq(0, 1, .1),
                    RDIPC = round(coef(RDIPC_Lorenz), digits = 4),
                    RDBPC = round(coef(RDBPC_Lorenz), digits = 4),
                    RDDPC = round(coef(RDDPC_Lorenz), digits = 4),
                    RDDPC_RBU = round(coef(RDDPC_RBU_Lorenz), digits = 4))

# Plotage da curva - Brasil

L6 = ggplot(Brasil, aes(x = Dist_Pop)) + 
  theme_bw() + 
  geom_abline() + 
  geom_point(aes(y = RDIPC, col = "A"), size = 2) +
  geom_line(aes(y = RDIPC, col = "A"), size = 1) +
  geom_point(aes(y = RDBPC, col = "B"), size = 2) +
  geom_line(aes(y = RDBPC, col = "B"), size = 1) +
  geom_point(aes(y = RDDPC, col = "C"), size = 2) +
  geom_line(aes(y = RDDPC, col = "C"), size = 1) +
  geom_point(aes(y = RDDPC_RBU, col = "D"), size = 2) +
  geom_line(aes(y = RDDPC_RBU, col = "D"), size = 1) +
  labs(title = "Brasil",
       y = "Participação na renda domiciliar",
       x = " ") + 
  scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("RDIPC", "RDBPC", "RDDPC", "RDDPC sob RBU"),
                     values = c("A" = "blue", "B" = "aquamarine4", "C" = "red", "D" = "deepskyblue4"))

# Limpeza

rm(RDIPC_Lorenz, RDBPC_Lorenz, RDDPC_Lorenz, RDDPC_RBU_Lorenz)

## Gráfico 4: Curvas de Lorenz (73.7)

l = (L1 + L2 + L3) / (L4 + L5 + L6)

ggsave(filename = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Gráfico 4.Curvas de Lorenz.png",
       plot = l,
       width = 14,
       height = 9,
       units = "in",
       dpi = 600)

rm(L1, L2, L3, L4, L5, L6, l, NE, CO, NO, SD, SUL, Brasil)


###
### INDICES DE GINI (74)
###

## Indices de Gini da Regiões (74.1)
# Coeficiente de Gini RDIPC no sistema vigente

CG_RDIPC = survey::svyby(~RDIPC,
                            by = ~Região,
                            design = Amostra_Complexa2,
                            convey::svygini,
                            na.rm = T)

# Coeficiente de Gini RDBPC no sistema vigente

CG_RDBPC = survey::svyby(~RDBPC,
                            by = ~Região,
                            design = Amostra_Complexa2,
                            convey::svygini,
                            na.rm = T)

# Coeficiente de Gini da RDDPC no sistema vigente

CG_RDDPC = survey::svyby(
  ~RDDPC,
  by = ~Região,
  design = Amostra_Complexa2,
  convey::svygini,
  na.rm = T
)


# Coeficiente de Gini da RDDPC_RBU no sistema reformado

CG_RDDPC_RBU = survey::svyby(
  ~RDDPC_RBU,
  by = ~Região,
  design = Amostra_Complexa2,
  convey::svygini,
  na.rm = T
)

# Coeficiente de Gini regional

Gini_R = cbind(CG_RDIPC, CG_RDBPC, CG_RDDPC, CG_RDDPC_RBU) %>% 
  select(-Região, -se) %>% 
  round(digits = 4)

Gini_R = mutate(Gini_R, "Região" = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste",
                                     "Sul"), .before = 1)

## Indices de Gini do Brasil (74.2)

Gini_br1 = svygini(~RDIPC,
                   design = Amostra_Complexa2,
                   na.rm = T)

Gini_br2 = svygini(~RDBPC,
                   design = Amostra_Complexa2,
                   na.rm = T)

Gini_br3 = svygini(~RDDPC,
                   design = Amostra_Complexa2,
                   na.rm = T)

Gini_br4 = svygini(~RDDPC_RBU,
                   design = Amostra_Complexa2,
                   na.rm = T)

Gini_Br = cbind.data.frame(Gini_br1, Gini_br2, Gini_br3, Gini_br4) %>% 
  round(digits = 4)

colnames(Gini_Br) = c("RDIPC", "a", "RDBPC", "b", "RDDPC", "c", "RDDPC_RBU", "d")

Gini_Br = Gini_Br %>% 
  mutate("Região" = "Brasil", .before = 1) %>% 
  select(-a, -b, -c, -d)

GINI = rbind.data.frame(Gini_R, Gini_Br)

rm(CG_RDIPC, CG_RDBPC, CG_RDDPC, CG_RDDPC_RBU, Gini_br1, Gini_br2, Gini_br3, 
   Gini_br4, Gini_R, Gini_Br)


# Grafico 1

Gini = GINI %>% 
  mutate("Dif1" = round(RDBPC - RDIPC, digits = 4),
         "Dif2" = round(RDDPC - RDBPC, digits = 4),
         "Dif3" = round(RDDPC_RBU - RDDPC, digits = 4),
         "Dif4" = round(RDDPC_RBU - RDIPC, digits = 4)) %>% 
  pivot_longer(cols = c(RDIPC, RDBPC, RDDPC, RDDPC_RBU)) %>% 
  rename(Rendas = name,
         Gini = value)

Gini_RDI_vig = Gini %>% 
  filter(Rendas == "RDIPC")

Gini1 = filter(Gini, Rendas == c("RDIPC", "RDBPC"))

Dif1 = Gini1 %>% 
  filter(Rendas == "RDIPC") %>% 
  mutate("x_pos" = round((Gini + (Dif1 / 2)), digits = 4))

g1 = ggplot(Gini1) + 
  theme_gray() +
  geom_segment(data = Gini_RDI_vig,
               aes(x = Gini, y = Região,
                   xend = Gini_RDB_Vig$Gini,
                   yend = Gini_RDB_Vig$Região),
               color = "#aeb6bf",
               size = 4.5,
               alpha = .5) + 
  geom_point(aes(x = Gini, y = Região, color = Rendas),
             size = 4, show.legend = T) + 
  scale_x_continuous(breaks = seq(0, 1, .04)) + 
  theme(legend.position = "top") + 
  geom_text(data = Dif1,
            aes(label = paste("D: ",Dif1), x = x_pos, y = Região),
            fill = "white",
            color = "#4a4e4d",
            size = 3,
            family = "Segoe UI Semibold") + 
  labs(x = " ", colour = " ")

# Grafico 2

Gini_RDB_Vig = Gini %>% 
  filter(Rendas == "RDBPC")

Gini2 = filter(Gini, Rendas == c("RDDPC", "RDBPC"))

Dif2 = Gini2 %>% 
  filter(Rendas == "RDBPC") %>% 
  mutate("x_pos" = round((Gini + (Dif2 / 2)), digits = 4))

g2 = ggplot(Gini2) + 
  geom_segment(data = Gini_RDB_Vig,
               aes(x = Gini, y = Região,
                   xend = Gini_RDD_Vig$Gini,
                   yend = Gini_RDD_Vig$Região),
               color = "#aeb6bf",
               size = 4.5,
               alpha = .5) + 
  geom_point(aes(x = Gini, y = Região, color = Rendas),
             size = 4, show.legend = T) + 
  scale_x_continuous(breaks = seq(0, 1, .02)) + 
  theme(legend.position = "top") + 
  geom_text(data = Dif2,
            aes(label = paste("D: ",Dif2), x = x_pos, y = Região),
            fill = "white",
            color = "#4a4e4d",
            size = 3,
            family = "Segoe UI Semibold") + 
  labs(x = " ", y = " ", colour = " ")

# Grafico 3

Gini_RDD_Vig = Gini %>% 
  filter(Rendas == "RDDPC")

Gini3 = filter(Gini, Rendas == c("RDDPC", "RDDPC_RBU"))

Dif3 = Gini3 %>% 
  filter(Rendas == "RDDPC") %>% 
  mutate("x_pos" = round((Gini + (Dif3 / 2)), digits = 4))

g3 = ggplot(Gini3) + 
  geom_segment(data = Gini_RDD_Vig,
               aes(x = Gini, y = Região,
                   xend = Gini_RDD_Ref$Gini,
                   yend = Gini_RDD_Ref$Região),
               color = "#aeb6bf",
               size = 4.5,
               alpha = .5) + 
  geom_point(aes(x = Gini, y = Região, color = Rendas),
             size = 4, show.legend = T) + 
  scale_x_continuous(breaks = seq(0, 1, .04)) + 
  theme(legend.position = "bottom") + 
  geom_text(data = Dif3,
            aes(label = paste("D: ",Dif3), x = x_pos, y = Região),
            fill = "white",
            color = "#4a4e4d",
            size = 3,
            family = "Segoe UI Semibold") + 
  labs(x = " ",
       colour = " ")

# Grafico 4

Gini_RDD_Ref = Gini %>% 
  filter(Rendas == "RDDPC_RBU")

Gini4 = filter(Gini, Rendas == c("RDIPC", "RDDPC_RBU"))

Dif4 = Gini4 %>% 
  filter(Rendas == "RDIPC") %>% 
  mutate("x_pos" = round((Gini + (Dif4 / 2)), digits = 4))

g4 = ggplot(Gini4) + 
  geom_segment(data = Gini_RDD_Ref,
               aes(x = Gini, y = Região,
                   xend = Gini_RDI_vig$Gini,
                   yend = Gini_RDI_vig$Região),
               color = "#aeb6bf",
               size = 4.5,
               alpha = .5) + 
  geom_point(aes(x = Gini, y = Região, color = Rendas),
             size = 4, show.legend = T) + 
  scale_x_continuous(breaks = seq(0, 1, .08)) + 
  theme(legend.position = "bottom") + 
  geom_text(data = Dif4,
            aes(label = paste("D: ",Dif4), x = x_pos, y = Região),
            fill = "white",
            color = "#4a4e4d",
            size = 3,
            family = "Segoe UI Semibold") + 
  labs(x = " ",
       y = " ",
       colour = " ")

## Grafico 5: Indices de Gini (74.3)

g5 = (g1 + g2) / (g3 + g4)

ggsave(filename = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Grafico 5.Indices de Gini.png",
       plot = g5,
       width = 12,
       height = 9,
       units = "in",
       dpi = 600)

## Tabela 8: Indices de Gini (74.4)

t8 = flextable::flextable(GINI) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  autofit(add_w = 0, add_h = 0)

save_as_pptx("Tabela 8: ÍndiceS de Gini" = t8,
             path = "C:/Users/chenr/Meu Drive/UFPE/DISSERTAÇÃO/RESULTADOS/Tabela 8.ÍndiceS de Gini.pptx")

# Limpeza

rm(Dif1, Dif2, Dif3, Dif4, g1, g2, g3, g4, g5, Gini_RDB_Vig, Gini_RDD_Ref,
   Gini_RDD_Vig, Gini_RDI_vig, Gini1, Gini2, Gini3, Gini4, Gini, t8)


###
### QUANTIS DE RENDA (75)
###

# Percentis da RDDPC
# Percentis da RDDPC_RBU
# Percentis da RDDPC_RBU

summary(Visita_5_MM$IR_RBU_PC)

Centro_Oeste = svyquantile(~IR_RBU_PC,
                           subset(Amostra_Complexa, 
                                  Região == "Centro-Oeste"),
                           quantiles = c(.05, .1, .2, .3, .4, .5, .6, .7, .8, .85, .9, .95, .99)) %>% 
  coef() %>% 
  as.data.frame()

Nordeste = svyquantile(~IR_RBU_PC,
                       subset(Amostra_Complexa, 
                              Região == "Nordeste"),
                       quantiles = c(.05, .1, .2, .3, .4, .5, .6, .7, .8, .85, .9, .95, .99)) %>% 
  coef() %>% 
  as.data.frame()

Norte = svyquantile(~IR_RBU_PC,
                    subset(Amostra_Complexa, 
                           Região == "Norte"),
                    quantiles = c(.05, .1, .2, .3, .4, .5, .6, .7, .8, .85, .9, .95, .99)) %>% 
  coef() %>% 
  as.data.frame()

Sudeste = svyquantile(~IR_RBU_PC,
                      subset(Amostra_Complexa, 
                             Região == "Sudeste"),
                      quantiles = c(.05, .1, .2, .3, .4, .5, .6, .7, .8, .85, .9, .95, .99)) %>% 
  coef() %>% 
  as.data.frame()

Sul = svyquantile(~IR_RBU_PC,
                  subset(Amostra_Complexa, 
                         Região == "Sul"),
                  quantiles = c(.05, .1, .2, .3, .4, .5, .6, .7, .8, .85, .9, .95, .99)) %>% 
  coef() %>% 
  as.data.frame()

Brasil = svyquantile(~IR_RBU_PC,
                     Amostra_Complexa,
                     quantiles = c(.05, .1, .2, .3, .4, .5, .6, .7, .8, .85, .9, .95, .99)) %>% 
  coef() %>% 
  as.data.frame()

## Percentis de RDDPC (75.1)

Percentis1 = data.frame("Percentis" = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 85, 90, 95, 99), 
                       "Centro-Oeste" = round(Centro_Oeste$.),
                       "Nordeste" = round(Nordeste$.),
                       "Norte" = round(Norte$.),
                       "Sudeste" = round(Sudeste$.),
                       "Sul" = round(Sul$.),
                       "Brasil" = round(Brasil$.))

## Tabela 9: Percentis da RDDPC (75.2)

t9 = flextable::flextable(Percentis1) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  autofit(add_w = 0, add_h = 0) %>% 
  colformat_double(big.mark = " ", digits = 0)

save_as_pptx("Anexo 3: Percentis da IR_RBU_PC" = t9,
             path = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Anexo 3.Percentis da IR_RBU_PC.pptx")

rm(t9, Brasil, Nordeste, Norte, Sudeste, Sul, Centro_Oeste)

## Percentis de RDDPC_RBU (75.3)

Percentis = data.frame("Percentis" = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 85, 90, 95, 99), 
                       "Centro-Oeste" = round(Centro_Oeste$.),
                       "Var1" = round(((Centro_Oeste$. / Percentis1$Centro.Oeste) - 1) * 100),
                       "Nordeste" = round(Nordeste$.),
                       "Var2" = round(((Nordeste$. / Percentis1$Nordeste) - 1) * 100),
                       "Norte" = round(Norte$.),
                       "Var3" = round(((Norte$. / Percentis1$Norte) - 1) * 100),
                       "Sudeste" = round(Sudeste$.),
                       "Var4" = round(((Sudeste$. / Percentis1$Sudeste) - 1) * 100),
                       "Sul" = round(Sul$.),
                       "Var5" = round(((Sul$. / Percentis1$Sul) - 1) * 100),
                       "Brasil" = round(Brasil$.),
                       "Var6" = round(((Brasil$. / Percentis1$Brasil) - 1) * 100))

## Tabela 10: Percentis da RDDPC_RBU (75.4)

t10 = flextable::flextable(Percentis) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  autofit(add_w = 0, add_h = 0) %>% 
  colformat_double(big.mark = " ", digits = 0)

save_as_pptx("Tabela 10: Percentis da RDDPC_RBU" = t10,
             path = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Tabela 10.Percentis da RDDPC_RBU.pptx")

rm(Percentis, t10, Brasil, Nordeste, Norte, Sudeste, Sul, Centro_Oeste, Percentis1)


###
### RAZOES DE QUINTIS DE RENDA (76)
###

## índice de Parma - relacao 10% mais ricos sobre 40% mais pobres (76.1)
# Indice da RDDPC
# Indice da RDDPC_RBU

Parma = data.frame("Centro_Oeste" = svyqsr(~RDDPC_RBU,
                                                  subset(Amostra_Complexa2,
                                                         Região == "Centro-Oeste"),
                                                  alpha1 = .4,
                                                  alpha2 = .1),
                          "Nordeste" = svyqsr(~RDDPC_RBU,
                                                  subset(Amostra_Complexa2,
                                                         Região == "Nordeste"),
                                                  alpha1 = .4,
                                                  alpha2 = .1),
                          "Norte" = svyqsr(~RDDPC_RBU,
                                                  subset(Amostra_Complexa2,
                                                         Região == "Norte"),
                                                  alpha1 = .4,
                                                  alpha2 = .1),
                          "Sudeste" = svyqsr(~RDDPC_RBU,
                                                  subset(Amostra_Complexa2,
                                                         Região == "Sudeste"),
                                                  alpha1 = .4,
                                                  alpha2 = .1),
                          "Sul" = svyqsr(~RDDPC_RBU,
                                                  subset(Amostra_Complexa2,
                                                         Região == "Sul"),
                                                  alpha1 = .4,
                                                  alpha2 = .1),
                          "Brasil" = svyqsr(~RDDPC_RBU,
                                                  Amostra_Complexa2,
                                                  alpha1 = .4,
                                                  alpha2 = .1)) %>% 
  select(-Centro_Oeste.RDDPC_RBU, -Nordeste.RDDPC_RBU, -Norte.RDDPC_RBU, -Sudeste.RDDPC_RBU, -Sul.RDDPC_RBU, -Brasil.RDDPC_RBU)

colnames(Parma) = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil")

Parma = as.data.frame(t(Parma))

## índice dos 20% mais sobre os 80% menos (76.2)
# Indice da RDDPC
# Indice da RDDPC_RBU

Parma1 = data.frame("Centro_Oeste" = svyqsr(~RDDPC_RBU,
                                           subset(Amostra_Complexa2,
                                                  Região == "Centro-Oeste")),
                   "Nordeste" = svyqsr(~RDDPC_RBU,
                                       subset(Amostra_Complexa2,
                                              Região == "Nordeste")),
                   "Norte" = svyqsr(~RDDPC_RBU,
                                    subset(Amostra_Complexa2,
                                           Região == "Norte")),
                   "Sudeste" = svyqsr(~RDDPC_RBU,
                                      subset(Amostra_Complexa2,
                                             Região == "Sudeste")),
                   "Sul" = svyqsr(~RDDPC_RBU,
                                  subset(Amostra_Complexa2,
                                         Região == "Sul")),
                   "Brasil" = svyqsr(~RDDPC_RBU,
                                     Amostra_Complexa2)) %>% 
  select(-Centro_Oeste.RDDPC_RBU, -Nordeste.RDDPC_RBU, -Norte.RDDPC_RBU, -Sudeste.RDDPC_RBU, -Sul.RDDPC_RBU, -Brasil.RDDPC_RBU)

colnames(Parma1) = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil")

Parma1 = as.data.frame(t(Parma1))

## índice dos 20% mais sobre os 20% menos (76.3)
# Indice da RDDPC
# Indice da RDDPC_RBU

Parma2 = data.frame("Centro_Oeste" = svyqsr(~RDDPC_RBU,
                                           subset(Amostra_Complexa2,
                                                  Região == "Centro-Oeste"),
                                           alpha1 = .2,
                                           alpha2 = .2),
                   "Nordeste" = svyqsr(~RDDPC_RBU,
                                       subset(Amostra_Complexa2,
                                              Região == "Nordeste"),
                                       alpha1 = .2,
                                       alpha2 = .2),
                   "Norte" = svyqsr(~RDDPC_RBU,
                                    subset(Amostra_Complexa2,
                                           Região == "Norte"),
                                    alpha1 = .2,
                                    alpha2 = .2),
                   "Sudeste" = svyqsr(~RDDPC_RBU,
                                      subset(Amostra_Complexa2,
                                             Região == "Sudeste"),
                                      alpha1 = .2,
                                      alpha2 = .2),
                   "Sul" = svyqsr(~RDDPC_RBU,
                                  subset(Amostra_Complexa2,
                                         Região == "Sul"),
                                  alpha1 = .2,
                                  alpha2 = .2),
                   "Brasil" = svyqsr(~RDDPC_RBU,
                                     Amostra_Complexa2,
                                     alpha1 = .2,
                                     alpha2 = .2)) %>% 
  select(-Centro_Oeste.RDDPC_RBU, -Nordeste.RDDPC_RBU, -Norte.RDDPC_RBU, -Sudeste.RDDPC_RBU, -Sul.RDDPC_RBU, -Brasil.RDDPC_RBU)

colnames(Parma2) = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil")

Parma2 = as.data.frame(t(Parma2))


###
### ÍNDICE DE T DE THEIL: MEDIDAS DE ENTROPIA (77)
###

# Indice da RDDPC
# Indice da RDDPC_RBU

Theil = data.frame("Centro-Oeste" = svygeidec(~RDDPC_RBU,
                                                    ~UF,
                                                    subset(Amostra_Complexa2, RDDPC_RBU > 0 & Região == "Centro-Oeste"),
                                                    epsilon = 1),
                   "Nordeste" = svygeidec(~RDDPC_RBU,
                                                    ~UF,
                                                    subset(Amostra_Complexa2, RDDPC_RBU > 0 & Região == "Nordeste"),
                                                    epsilon = 1),
                   "Norte" = svygeidec(~RDDPC_RBU,
                                                    ~UF,
                                                    subset(Amostra_Complexa2, RDDPC_RBU > 0 & Região == "Norte"),
                                                    epsilon = 1),
                   "Sudeste" = svygeidec(~RDDPC_RBU,
                                                    ~UF,
                                                    subset(Amostra_Complexa2, RDDPC_RBU > 0 & Região == "Sudeste"),
                                                    epsilon = 1),
                   "Sul" = svygeidec(~RDDPC_RBU,
                                                    ~UF,
                                                    subset(Amostra_Complexa2, RDDPC_RBU > 0 & Região == "Sul"),
                                     epsilon = 1),
                   "Brasil" = svygeidec(~RDDPC_RBU,
                                        ~Região,
                                        subset(Amostra_Complexa2, RDDPC_RBU > 0),
                                                    epsilon = 1)) %>% 
  select(-Centro.Oeste.SE, -Nordeste.SE, -Norte.SE, -Sudeste.SE, -Sul.SE, -Brasil.SE)

colnames(Theil) = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil")

Theil = as.data.frame(t(Theil))


###
### SUMARIO DE INDICADORES DE DESIGUALDADE (78)
###

Desig = data.frame("Região" = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil"),
                   "Parma1" = as.numeric(round(Parma2$RDDPC_RBU, digits = 4)),
                   "Parma2" = as.numeric(round(Parma1$RDDPC_RBU, digits = 4)),
                   "Parma" = as.numeric(round(Parma$RDDPC_RBU, digits = 4)),
                   "Gini" = as.numeric(round(GINI$RDDPC_RBU, digits = 4)),
                   "Theil" = as.numeric(round(Theil$total, digits = 4)),
                   "Dentro" = as.numeric(round(Theil$within, digits = 4)),
                   "Entre" = as.numeric(round(Theil$between, digits = 4)))

# Tabela 11: Indices de desigualdade da RDDPC
# Tabela 12: Indices de desigualdade da RDDPC_RBU

t11 = flextable::flextable(Desig) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  add_header_row(colwidths = c(1, 3, 1, 3),
                 values = c(" ", "Razões de quintis", " ", "Decomposição do T-Theil")) %>% 
  autofit(add_w = 0, add_h = 0) %>% 
  colformat_double(big.mark = ",", decimal.mark = ",", digits = 4)

save_as_pptx("Tabela 11: Indices de desigualdade da RDDPC_RBU" = t11,
             path = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Tabela 11.Indices de desigualdade da RDDPC_RBU.pptx")

rm(Desig, t10, t11, Parma, Parma1, Parma2, Theil, GINI)


###
### ESTATÍSTICAS DESCRITIVAS PARA A RBU (79)
###

df1 = svyby(~Pop,
      by = ~UF,
      design = Amostra_Complexa, 
      svytotal,
      keep.var = F)

colnames(df1) = c("Região", "total")

df2 = svyby(~Num_Moradores,
      by = ~UF,
      design = Amostra_Complexa, 
      svymean,
      keep.var = F)

df3 = svytotal(~Pop,
               design = Amostra_Complexa) %>% 
  as.data.frame()

df3 = df3 %>% 
  mutate("Região" = "Brasil", .before = 1) %>% 
  select(Região, total)

df3 = rbind.data.frame(df1, df3)

df4 = svymean(~Num_Moradores,
              design = Amostra_Complexa) %>% 
  as.data.frame()

df4 = df4 %>% 
  mutate("Região" = "Brasil", .before = 1) %>% 
  select(Região, mean)

colnames(df4) = c("Região", "statistic")

df4 = rbind.data.frame(df2, df4)

df5 = data.frame("Estado" = df1$UF,
                 "Nº de pessoas" = df1$statistic,
                 "Pessoas/família" = round(df2$statistic, digits = 1),
                 "RBU/pessoa" = 422) %>% 
  mutate("Nº de famílias" = round(`Nº.de.pessoas` / `Pessoas.família`),
         "RBU/família" = round(`Pessoas.família` * `RBU.pessoa`)) %>% 
  select(Estado, `Nº.de.pessoas`, `Nº de famílias`, `Pessoas.família`, `RBU.pessoa`, `RBU/família`)

# Tabela 13: Estatisticas descritivas da para a RBU

t13 = flextable::flextable(df5) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  autofit(add_w = 0, add_h = 0) %>% 
  colformat_double(j = c(2, 3, 6), big.mark = " ", digits = 0) %>% 
  colformat_double(j = c(4), big.mark = ",", decimal.mark = ",", digits = 1)

save_as_pptx("Anexo 4: Estatisticas descritivas da para a RBU" = t13,
             path = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Anexo 4.Estatisticas descritivas da para a RBU.pptx")

rm(df1, df2, df3, df4, df5, t13)


###
### GANHOS E PERDAS (80)
###

## Regioes (80.1)
# Ganhadores e perderores

Ganhador = svyby(~Ganhador + Perdedor,
                 by = ~Região,
                 design = Amostra_Complexa,
                 svytotal,
                 keep.var = F)

colnames(Ganhador) = c("Região", "Ganhadores", "Perdedores")

#Ganhos e perdas

Perdedor = svyby(~Ganho + Perda,
                 by = ~Região,
                 design = Amostra_Complexa,
                 svymean,
                 keep.var = F)

colnames(Perdedor) = c("Região", "Ganho", "Perda")

## Brasil (80.2)
# Ganhadores e perderores

Ganhador2 = svytotal(~Ganhador + Perdedor,
                 design = Amostra_Complexa) %>% 
  as.data.frame()

Ganhador2 = t(Ganhador2)

Ganhador2 = Ganhador2[-2, ]

Ganhador2 = t(Ganhador2)

Ganhador2 = as.data.frame(Ganhador2)

Ganhador2 = Ganhador2 %>% 
  mutate("Região" = "Brasil", .before = 1)

colnames(Ganhador2) = c("Região", "Ganhadores", "Perdedores")

# Ganhos e perdas

Perdedor2 = svymean(~Ganho + Perda,
                  design = Amostra_Complexa) %>% 
  as.data.frame()

Perdedor2 = t(Perdedor2)

Perdedor2 = Perdedor2[-2, ]

Perdedor2 = t(Perdedor2)

Perdedor2 = as.data.frame(Perdedor2)

Perdedor2 = Perdedor2 %>% 
  mutate("Região" = "Brasil", .before = 1)

#

df1 = rbind.data.frame(Ganhador, Ganhador2)

#

df2 = rbind.data.frame(Perdedor, Perdedor2)

#

df1 = df1 %>% 
  mutate("percet" = Ganhadores + Perdedores)

## Sumario de ganhadores, perdedores, ganhos e perdas (80.3)

Tabela14 = data.frame("Região" = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste",
                                  "Sul", "Brasil"),
                     "Ganhadores" = df1$Ganhadores,
                     "(%) Região" = round((df1$Ganhadores / df1$percet) * 100, digits = 2),
                     "(%) Brasil" = round(((df1$Ganhadores / 132069363) * 100), digits = 2),
                     "Ganho médio" = round(df2$Ganho),
                     "Perdedores" = df1$Perdedores,
                     "(%) Região1" = round((df1$Perdedores / df1$percet) * 100, digits = 2),
                     "(%) Brasil1" = round(((df1$Perdedores / 77427100) * 100), digits = 2),
                     "Perda média" = round(df2$Perda))

## Tabela 13: Ganhadores, perdedores, ganhos e perdas (80.4)

t14 = flextable::flextable(Tabela14) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  autofit(add_w = 0, add_h = 0) %>% 
  colformat_double(j = c(3, 4, 7, 8), digits = 2, suffix = " %") %>% 
  colformat_double(j = c(5, 9), digits = 0, prefix = "R$ ")

save_as_pptx("Tabela 13: Ganhadores, perdedores, ganhos e perdas" = t14,
             path = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Tabela 13.Ganhadores, perdedores, ganhos e perdas.pptx")

rm(t14, Tabela14, df1, df2, Perdedor, Perdedor2, Ganhador, Ganhador2)


###
### PANORAMA INTRARREGIONAL (81)
###

## RENDAS DOMICILIARES (81.1)
# Rendas domiciliares per capita - Regiões

Rendas = svyby(~RDIPC + RDBPC + RDDPC + RDDPC_RBU,
               by = ~UF,
               Amostra_Complexa,
               svymean,
               keep.var = F,
               keep.names = F)

colnames(Rendas) = c("UF",
                     "RDIPC",
                     "RDBPC",
                     "RDDPC",
                     "RDDPC_RBU")

Rendas = Rendas %>% 
  mutate(RDIPC = round(RDIPC),
         RDBPC = round(RDBPC),
         RDDPC = round(RDDPC),
         RDDPC_RBU = round(RDDPC_RBU))

Rendas = Rendas %>% 
  mutate("Cód_UF" = case_when(UF == "Acre" ~ 12,
                              UF == "Bahia" ~ 29,
                              UF == "Minas Gerais" ~ 31,
                              UF == "Pernambuco" ~ 26,
                              UF == "São Paulo" ~ 35,
                              UF == "Alagoas" ~ 27,
                              UF == "Ceará" ~ 23,
                              UF == "Maranhão" ~ 21,
                              UF == "Piauí" ~ 22,
                              UF == "Rondônia" ~ 11,
                              UF == "Sergipe" ~ 28,
                              UF == "Amapá" ~ 16,
                              UF == "Distrito Federal" ~ 53,
                              UF == "Mato Grosso" ~ 51,
                              UF == "Paraíba" ~ 25,
                              UF == "Rio de Janeiro" ~ 33,
                              UF == "Roraima" ~ 14,
                              UF == "Tocantins" ~ 17,
                              UF == "Amazonas" ~ 13,
                              UF == "Espírito Santo" ~ 32,
                              UF == "Mato Grosso do Sul" ~ 50,
                              UF == "Paraná" ~ 41,
                              UF == "Rio Grande do Norte" ~ 24,
                              UF == "Santa Catarina" ~ 42,
                              UF == "Goiás" ~ 52,
                              UF == "Rio Grande do Sul" ~ 43,
                              UF == "Pará" ~ 15), .before = 1)

UFS = read_state(code_state = "all", year = 2020)

UFS = dplyr::left_join(UFS, Rendas, by = c("code_state" = "Cód_UF"))

## Mapas coropléticos das rendas (81.2)

no_axis = theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

#

g7 = ggplot(data = UFS) + 
  geom_sf(aes(fill = RDIPC), color = "white", size = .15) + 
  theme_minimal() + 
  theme(legend.position = "left") +
  scale_fill_viridis_c(option = "viridis") +
  no_axis

#

g8 = ggplot(data = UFS) + 
  geom_sf(aes(fill = RDBPC), color = "white", size = .15) + 
  theme_minimal() + 
  scale_fill_viridis_c(option = "viridis") +
  no_axis

#

g9 = ggplot(data = UFS) + 
  geom_sf(aes(fill = RDDPC), color = "white", size = .15) + 
  theme_minimal() + 
  theme(legend.position = "left") +
  scale_fill_viridis_c(option = "viridis") +
  no_axis

#

g10 = ggplot(data = UFS) + 
  geom_sf(aes(fill = RDDPC_RBU), color = "white", size = .15) + 
  theme_minimal() + 
  scale_fill_viridis_c(option = "viridis") +
  no_axis

## Grafico 6: Panorama intrarregional (81.3)

g6 = (g7 + g8) / (g9 + g10)


ggsave(filename = "C:/Users/chenr/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Grafico 6.Panorama intrarregional.png",
       plot = g6,
       width = 12,
       height = 9,
       units = "in",
       dpi = 600)

rm(g7, g8, g9, g10, g6, Rendas)

## Alternativa: Grafico de haltere estendido (81.4)

dados1 = UFS %>% 
  select(abbrev_state, name_state, RDDPC, RDDPC_RBU) %>% 
  mutate(diff = RDDPC_RBU - RDDPC) %>% 
  pivot_longer(cols = c(RDDPC, RDDPC_RBU),
               names_to = "Rendas",
               values_to = "Valor")

rddpc = dados1 %>% 
  filter(Rendas == "RDDPC")

rddpc_RBU = dados1 %>% 
  filter(Rendas == "RDDPC_RBU")

## Grafico 7: Mudanças na renda disponiivel dos estados sob RBU (81.5)
# Grafico inicial

g7 = ggplot(dados1)+
  geom_segment(data = rddpc,
               aes(x = Valor, y = name_state,
                   yend = rddpc_RBU$name_state, xend = rddpc_RBU$Valor),
               color = "#aeb6bf",
               size = 4.5,
               alpha = .5) +
  
  geom_point(aes(x = Valor, y = name_state, color = Rendas), size = 4, show.legend = TRUE)+
  theme_gray()+
  labs(x = " ",
       y = " ",
       color = " ")+
  theme(legend.position = "top",
        axis.text = element_text(size = 12))+
  scale_x_continuous(breaks = seq(500, 2250, 250),
                     limits = c(500, 2250))+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  geom_vline(xintercept = 1319, linetype = "solid", size = .5, alpha = .8, color = "midnightblue")+
  geom_text(x = 1300, y = "Sergipe", label = "MÉDIA DO BRASIL", angle = 90, size = 3, color = "royalblue4", 
            family = "Segoe UI")

ggsave(filename = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Grafico 7.Mudanças na renda disponiivel dos estados sob RBU.png",
       plot = g7,
       width = 12,
       height = 9,
       units = "in",
       dpi = 600)

rm(g7, dados1, rddpc, rddpc_RBU, Rendas)


## Índices de Gini estaduais (81.6)
# Coeficiente de Gini da RDIPC no sistema vigente

CG_Rend_Ini = survey::svyby(
  ~RDIPC,
  by = ~UF,
  design = Amostra_Complexa2,
  convey::svygini,
  na.rm = T
)

# Coeficiente de Gini da RDBPC no sistema vigente

CG_Rend_Brt = survey::svyby(
  ~RDBPC,
  by = ~UF,
  design = Amostra_Complexa2,
  convey::svygini,
  na.rm = T
)

# Coeficiente de Gini da RDDPC no sistema vigente

CG_Rend_Dsp = survey::svyby(
  ~RDDPC,
  by = ~UF,
  design = Amostra_Complexa2,
  convey::svygini,
  na.rm = T
)

# Coeficiente de Gini da RDDPC_RBU no sistema reformado

CG_Rend_Dsp2 = survey::svyby(
  ~RDDPC_RBU,
  by = ~UF,
  design = Amostra_Complexa2,
  convey::svygini,
  na.rm = T
)

# Coeficiente de Gini estadual

Gini_E = cbind(CG_Rend_Ini, CG_Rend_Brt, CG_Rend_Dsp, CG_Rend_Dsp2) %>% 
  select(-UF, -se) %>% 
  round(digits = 4)

rm(CG_Rend_Ini, CG_Rend_Brt, CG_Rend_Dsp, CG_Rend_Dsp2)

Gini_E = mutate(Gini_E, "UF" = c("Acre", 
                                     "Alagoas", 
                                     "Amapá", 
                                     "Amazonas",
                                     "Bahia",
                                     "Ceará",
                                     "Distrito Federal",
                                     "Espírito Santo",
                                     "Goiás",
                                     "Maranhão",
                                     "Mato Grosso",
                                     "Mato Grosso do Sul",
                                     "Minas Gerais",
                                     "Pará",
                                     "Paraíba",
                                     "Paraná",
                                     "Pernambuco",
                                     "Piauí",
                                     "Rio de Janeiro",
                                     "Rio Grande do Norte",
                                     "Rio Grande do Sul",
                                     "Rondônia",
                                     "Roraima",
                                     "Santa Catarina",
                                     "São Paulo",
                                     "Sergipe",
                                     "Tocantins"), .before = 1) %>% 
  mutate("Cód_UF" = case_when(UF == "Acre" ~ 12,
                              UF == "Bahia" ~ 29,
                              UF == "Minas Gerais" ~ 31,
                              UF == "Pernambuco" ~ 26,
                              UF == "São Paulo" ~ 35,
                              UF == "Alagoas" ~ 27,
                              UF == "Ceará" ~ 23,
                              UF == "Maranhão" ~ 21,
                              UF == "Piauí" ~ 22,
                              UF == "Rondônia" ~ 11,
                              UF == "Sergipe" ~ 28,
                              UF == "Amapá" ~ 16,
                              UF == "Distrito Federal" ~ 53,
                              UF == "Mato Grosso" ~ 51,
                              UF == "Paraíba" ~ 25,
                              UF == "Rio de Janeiro" ~ 33,
                              UF == "Roraima" ~ 14,
                              UF == "Tocantins" ~ 17,
                              UF == "Amazonas" ~ 13,
                              UF == "Espírito Santo" ~ 32,
                              UF == "Mato Grosso do Sul" ~ 50,
                              UF == "Paraná" ~ 41,
                              UF == "Rio Grande do Norte" ~ 24,
                              UF == "Santa Catarina" ~ 42,
                              UF == "Goiás" ~ 52,
                              UF == "Rio Grande do Sul" ~ 43,
                              UF == "Pará" ~ 15), .before = 1)

UFS = dplyr::left_join(UFS, Gini_E, by = c("code_state" = "Cód_UF")) %>% 
  select()

## Grafico 8: Índices de Gini dos estados (81.7)

g1 = ggplot(data = UFS) + 
  geom_sf(aes(fill = RDDPC.y), color = "white", size = .15) + 
  theme_minimal() + 
  labs(fill = " ",
       subtitle = "Índice de Gini da RDDPC")+
  theme(legend.position = "left",
        axis.text = element_text(size = 12)) +
  scale_fill_viridis_c(option = "cividis", direction = -1) +
  no_axis

#

g2 = ggplot(data = UFS) + 
  geom_sf(aes(fill = RDDPC_RBU.y), color = "white", size = .15) + 
  theme_minimal() + 
  labs(fill = " ",
       subtitle = "Índice de Gini da RDDPC sob RBU")+
  theme(axis.text = element_text(size = 12)) +
  scale_fill_viridis_c(option = "cividis", direction = -1) +
  no_axis

g8 = g1 + g2

ggsave(filename = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Grafico 8.Índices de Gini dos estados.png",
       plot = g8,
       width = 12,
       height = 9,
       units = "in",
       dpi = 600)

rm(g1, g2, g8, no_axis)

## Tabela resumo dos Gini estaduais (81.8)

intra = UFS %>% 
  select(name_state, RDIPC.x, RDBPC.x, RDDPC.x, RDDPC_RBU.x, RDIPC.y, RDBPC.y, RDDPC.y, RDDPC_RBU.y) %>% 
  as.data.frame() %>% 
  select(-geom)

colnames(intra) = c("Estados", "RDIPC", "RDBPC", "RDDPC", "RDDPC_RBU", "Gini_RDIPC",
                    "Gini_RDBPC", "Gini_RDDPC", "Gini_RDDPC_RBU")

## Tabela 15: Rendas e indices de Gini estaduais (81.9)

t15 = flextable::flextable(intra) %>% 
  theme_booktabs() %>% 
  align(i = 1, part = "header", align = "center") %>% 
  bold(i = 1, part = "header", bold = T) %>% 
  add_header_row(values = c(" ", "Renda domiciliar", "Índice de Gini"),
                 colwidths = c(1, 4, 4)) %>% 
  autofit(add_w = 0, add_h = 0)

save_as_pptx("Tabela 15: Rendas e indice de Gini estaduais" = t15,
             path = "C:/Users/carlos.sousa/OneDrive/UFPE/DISSERTAÇÃO/RESULTADOS/Tabela 15.Rendas e indice de Gini estaduais.pptx")

rm(t15, intra, UFS)


###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###
###                                   FINAL                                  ###
###>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###