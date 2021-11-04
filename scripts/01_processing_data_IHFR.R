# Script to process data of in-hospital mortality and SRAG hospitalizations

# Loading libraries
library(ISOweek)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

# Loading functions
source("functions/read.sivep.R")
source("functions/end.of.epiweek.R")

# 1.  Downloading and filtering data -------------------------------------------
# Downloading the most recent SIVEP database from our repository
# https://github.com/covid19br/central_covid
# SIVEP date
data.sivep <- "2021_11_01"
file_suffix <- paste0(data.sivep, c(".csv.xz", ".csv.21.xz"))

dir_name <- paste0("data/raw/part_", seq_along(file_suffix))
file_name <- paste0(dir_name, "/SRAGHospitalizado_", file_suffix)

if (!all(dir.exists(dir_name))) {sapply(dir_name, dir.create, recursive = TRUE)}

for (i in seq_along(file_name)) {
  if (!file.exists(file_name[i])) {
    download.file(url = paste0("https://github.com/covid19br/central_covid/blob/master/dados/SIVEP-Gripe/SRAGHospitalizado_", file_suffix[i], "?raw=true"),
                  destfile = file_name[i])
  }
}


# Reading SIVEP w/ accessory function
data_raw_list <- lapply(dir_name, function(x) read.sivep(dir = x, escala = "pais", data = data.sivep))
data_raw <- bind_rows(data_raw_list)

# Setting the last date to cut database
last.date <-  "2021-07-30"

# Filtering data to last.date using date of first symptoms
df <- data_raw %>%
  filter(dt_sin_pri <= as_date(last.date))

# Converting age to numeric
df$nu_idade_n <- as.numeric(df$nu_idade_n)

# Defining age classes
df <- df %>%
  mutate(age_clas = case_when(nu_idade_n >= 0 & nu_idade_n <= 19 ~ "age_0_19",
                              nu_idade_n > 19 & nu_idade_n <= 39 ~ "age_20_39",
                              nu_idade_n > 39 & nu_idade_n <= 59 ~ "age_40_59",
                              nu_idade_n > 59 & nu_idade_n <= 79 ~ "age_60_79",
                              nu_idade_n >= 80 ~ 'age_80'))


# Filtering COVID hospitalizations only
covid <- df %>%
  filter(!is.na(age_clas)) %>%
  filter(hospital == 1) %>%
  filter(pcr_sars2 == 1 | classi_fin == 5)  %>%
  filter(evolucao == 1 | evolucao == 2) %>%
  select(dt_sin_pri, evolucao, age_clas, sg_uf, uti)

# Creating column w/ epidemiological week
covid$week <- end.of.epiweek(covid$dt_sin_pri)


# Filtering SRAG hospitalizations only

srag <- df %>%
  filter(hospital == 1) %>%
  filter(evolucao == 1 | evolucao == 2) %>%
  filter(!is.na(age_clas)) %>%
  select(dt_sin_pri, evolucao, age_clas,sg_uf, uti)

# Creating column w/ epidemiological week
srag$week <- end.of.epiweek(srag$dt_sin_pri)

# 2. Summarizing letality ------------------------------------------------------

# 2.1. COVID ----
tabela  <- covid %>%
  group_by(week, age_clas, sg_uf) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao == 2))

tabela$let_obs <- tabela$obitos/(tabela$obitos + tabela$sobre)

# 2.2. SRAG ----
tabela2  <-
  srag %>%
  group_by(week, age_clas, sg_uf) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao == 2))

tabela2$let_obs <- tabela2$obitos/(tabela2$obitos + tabela2$sobre)


if (!dir.exists("data/processed")) {dir.create("data/processed", recursive = TRUE)}

# 2.3. Exporting data ----
write.csv(tabela,
          file = paste0("data/processed/", "summary_covid_IFHR", "_", last.date, ".csv"),
          row.names = FALSE)
write.csv(tabela2,
          file = paste0("data/processed/", "summary_srag_IFHR", "_", last.date, ".csv"),
          row.names = FALSE)


# 3. Summarizing SRAG hospitalizations -----------------------------------------
hosp_week <- tabela2 %>%
  group_by(week, sg_uf) %>%
  summarise(hosp = n()) %>%
  #filter(week < 36 & week >= 10)
  filter(week >= 10)

write.csv(hosp_week,
          file = paste0("data/processed/", "hospitalizados_srag_week", "_", last.date, ".csv"),
          row.names = FALSE)


#####proportion of hospitalizations by age group
prop1 <- tabela %>%
  group_by(sg_uf, week, age_clas)%>%
  summarise(hosp=sum(sobre,obitos)) %>%
  mutate(freq=hosp/sum(hosp))

prop2 <- tabela2 %>%
  group_by(sg_uf, week, age_clas)%>%
  summarise(hosp=sum(sobre,obitos)) %>%
  mutate(freq=hosp/sum(hosp))

write.csv(prop1,
          file = paste0("data/processed/", "proporcao_covid_week", "_", last.date, ".csv"),
          row.names = FALSE)

write.csv(prop2,
          file = paste0("data/processed/", "proporcao_srag_week", "_", last.date, ".csv"),
          row.names = FALSE)


