# Script to process data of IHFR and SRAG hospitalizations ---------------------

# Loading libraries
library(ISOweek)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

# Loading functions
source("functions/read.sivep.R")


# 1.  Downloading data ---------------------------------------------------------
# Downloading the most recent SIVEP database from our repository
# https://github.com/covid19br/central_covid
# SIVEP date
data.sivep <- "2021_02_01"

download_sivep <- download.file(url = paste0("https://github.com/covid19br/central_covid/blob/master/dados/SIVEP-Gripe/SRAGHospitalizado_", data.sivep, "?raw=true"),
                                destfile = "data/raw/SRAGHospitalizado_2021_02_01.csv.xz")


# Reading SIVEP w/ acessory function
data_raw <- read.sivep(dir = "data/raw/", escala = "pais", data = data.sivep)

# Setting the last date to cut database
last.date <- "2020_09_28"

# Filtering data to last.date
df <- data_raw %>%
  filter(dt_notific <= as_date(last.date))

# Converting age to numeric
df$nu_idade_n <- as.numeric(df$nu_idade_n)

# Defining age classes
df <- df %>%
  mutate(age_clas = case_when(nu_idade_n >= 0 & nu_idade_n <= 19 ~ "age_0_19",
                              nu_idade_n > 19 & nu_idade_n <= 39 ~ "age_20_39",
                              nu_idade_n > 39 & nu_idade_n <= 59 ~ "age_40_59",
                              nu_idade_n >= 60 ~ "age_60"))


# Filtering COVID hospitalizations only

covid <- df %>%
  filter(!is.na(age_clas)) %>%
  select(dt_sin_pri, evolucao, age_clas, sg_uf, uti)

# Creating column w/ epidemiological week
  filter(hospital == 1) %>%
  filter(pcr_sars2 == 1 | classi_fin == 5)  %>%
  filter(evolucao == 1 | evolucao == 2) %>%
## Semana epidemiologica brasileira
covid$week <- epiweek(covid$dt_sin_pri) ####semana epidemiol?gica come?ando no domingo


###################SRAG#################################

srag <- dados %>%
  filter(hospital == 1) %>%
  filter(evolucao == 1 | evolucao == 2) %>%
  filter(!is.na(age_clas)) %>%
  select(dt_sin_pri, evolucao, age_clas,sg_uf, uti)

# Classificando semana epidemiologica por estado
## Semana epidemiologica brasileira
srag$week <- epiweek(srag$dt_sin_pri) ####semana epidemiol?gica come?ando no domingo

####################summary dos casos########################

##COVID##
tabela  <-
  covid %>%
  group_by(week, age_clas, sg_uf) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao == 2))

tabela$let_obs <- tabela$obitos/(tabela$obitos + tabela$sobre)

##SRAG###
tabela2  <-
  srag %>%
  group_by(week, age_clas, sg_uf) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao == 2))

tabela2$let_obs <- tabela2$obitos/(tabela2$obitos + tabela2$sobre)


write.csv(tabela, file = paste0("output/dados/", "summary_covid_IFHR","_", last.date,".csv"),
          row.names = FALSE)
write.csv(tabela2, file = paste0("output/dados/", "summary_srag_IFHR","_", last.date,".csv"),
          row.names = FALSE)

####################summary dos casos UTI ########################

##Casos##
##COVID##
tabela_uti  <-
  covid %>%
  filter(uti == 1 | uti == 2) %>%
  group_by(week, age_clas, sg_uf) %>%
  summarise(N = n(),
            leito = sum(uti == 2), uti = sum(uti == 1))

tabela_uti$uti_obs <- tabela_uti$uti / (tabela_uti$leito + tabela_uti$uti)

##SRAG###
tabela2_uti  <-
  srag %>%
  filter(uti == 1 | uti == 2) %>%
  group_by(week, age_clas, sg_uf) %>%
  summarise(N = n(),
            leito = sum(uti == 2), uti = sum(uti == 1))

tabela2_uti$uti_obs <- tabela2_uti$uti/(tabela2_uti$leito + tabela2_uti$uti)

write.csv(tabela_uti, file = paste0("output/dados/", "summary_covid_IFHR_uti","_", last.date,".csv"),
          row.names = FALSE)
write.csv(tabela2_uti, file = paste0("output/dados/", "summary_srag_IFHR_uti","_", last.date,".csv"),
          row.names = FALSE)

##Death##
##COVID##
tabela_uti_death  <-
  covid %>%
  filter(uti == 1 | uti == 2) %>%
  filter(evolucao == 2) %>%
  group_by(week, age_clas, sg_uf) %>%
  summarise(N = n(),
            leito = sum(uti == 2), uti = sum(uti == 1))

tabela_uti_death$uti_obs <- tabela_uti_death$uti / (tabela_uti_death$leito + tabela_uti_death$uti)

##SRAG###
tabela2_uti_death  <-
  srag %>%
  filter(uti == 1 | uti == 2) %>%
  filter(evolucao == 2) %>%
  group_by(week, age_clas, sg_uf) %>%
  summarise(N = n(),
            leito = sum(uti == 2), uti = sum(uti == 1))

tabela2_uti_death$uti_obs <- tabela2_uti_death$uti / (tabela2_uti_death$leito + tabela2_uti_death$uti)


write.csv(tabela_uti_death, file = paste0("output/dados/", "summary_covid_IFHR_uti_obs","_", last.date,".csv"),
          row.names = FALSE)
write.csv(tabela2_uti_death, file = paste0("output/dados/", "summary_srag_IFHR_uti_obs","_", last.date,".csv"),
          row.names = FALSE)

####HOSPITALIZADOS

hosp_week <- srag %>%
  group_by(week, sg_uf) %>%
  summarise(hosp = n())

hosp_week <- hosp_week %>%
  filter(week < 36 & week >= 10)

write.csv(hosp_week, file = paste0("output/dados/", "hospitalizados_srag_week","_", last.date,".csv"),
          row.names = FALSE)
