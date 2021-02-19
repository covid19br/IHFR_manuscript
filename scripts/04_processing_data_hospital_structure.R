library(dplyr)
library(sf)
library(spData)
library(brazilmaps) #remotes::install_github("rpradosiqueira/brazilmaps")

saude <- read.csv("data/raw/hospital_indicators.csv")
covid <- read.csv("outputs/model_table_glm_covid_IFHR.csv")
sus <- read.csv("data/raw/sus_dependent.csv")

# Correcting data for 100.000 hab
saude <- saude %>%
  mutate(prop_med_100000 = prop_med_10000 * 10,
         prop_MI_100000 = prop_MI_10000 * 10,
         UTI_100000 = UTI_10mil * 10)

dados <- covid %>%
  group_by(sg_uf) %>%
  filter(fit == max(fit)) %>%
  select(sg_uf, fit) %>%
  as.data.frame()

# Binding data
dados2 <- full_join(dados, saude, by = c("sg_uf" = "sigla"))

# Reading shapefile of states - no acronyms ¬¬ ---------------------------------
br <- brazilmaps::get_brmap(geo = "State")

# Binding data and shapefile ---------------------------------------------------
med_int_shape <- br %>%
  left_join(dados2, by = "State") %>%
  mutate(IHFR = fit)

# Exporting data for the next scripts ------------------------------------------
if (!dir.exists("data/shapefile/")) {dir.create("data/shapefile", recursive = TRUE)}

# WIP!!!!
st_write(med_int_shape, "data/shapefile/hospital_indicators.shp")
