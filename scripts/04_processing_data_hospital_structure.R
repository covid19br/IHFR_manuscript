library(dplyr)
library(sf)
library(spData)
library(brazilmaps) #remotes::install_github("rpradosiqueira/brazilmaps")

saude <- read.csv("data/raw/hospital_indicators.csv", row.names = 1)
covid <- read.csv("outputs/model_table_glm_covid_IFHR.csv")


# Correcting data for 100.000 hab
saude <- saude %>%
  mutate(pmed = prop_med_10000 * 10,
         pMI = prop_MI_10000 * 10,
         UTI = UTI_10mil * 10) %>%
  rename(SUS = sus_dep_hab) %>%
  select(-prop_med_10000, -prop_MI_10000, -UTI_10mil)

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
  left_join(dados2, by = c("State", "nome")) %>%
  mutate(IHFR = fit)

# Exporting data for the next scripts ------------------------------------------
if (!dir.exists("data/shapefile/")) {dir.create("data/shapefile", recursive = TRUE)}

st_write(med_int_shape, "data/shapefile/hospital_indicators.shp", append = FALSE)

# Exporting in csv format
med_int_csv <- med_int_shape %>%
  as.data.frame() %>%
  select(-geometry)

write.csv(med_int_csv, "data/processed/04-med_int.csv", row.names = FALSE)
