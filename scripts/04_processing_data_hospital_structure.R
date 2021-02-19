library(tmap)
library(dplyr)
library(sf)
library(readr)
library(spData)
library(devtools)
library(viridis)
library(RColorBrewer)
library(ggpubr)
library(GGally)

saude <- read.csv("data/raw/hospital_indicatiors.csv")
covid <- read.csv("outputs/model_table_glm_covid_IFHR.csv")
sus <- read.csv("data/raw/sus_dependent.csv")

# correcao a 100.000
saude <- saude %>%
  mutate(prop_med_100000 = prop_med_10000 * 10,
         prop_MI_100000 = prop_MI_10000 * 10,
         UTI_100000 = UTI_10mil * 10)
names(covid)

dados <- covid %>%
  group_by(sg_uf) %>%
  filter(fit == max(fit)) %>%
  select(sg_uf, fit) %>%
  as.data.frame()

###junta medico e dados##

dados2 <- full_join(dados, saude, by = c("sg_uf" = "sigla"))

# shape de estados - não tem sigla ¬¬-----
br <- brazilmaps::get_brmap(geo = "State")

# junta o shapefile do brasil com a tabela de dados----
med_int_shape <- br %>% left_join(dados2, by = "State")

med_int_shape$IHFR <- med_int_shape$fit

# mapa peak IHFR
med_int_shape$sg_uf
