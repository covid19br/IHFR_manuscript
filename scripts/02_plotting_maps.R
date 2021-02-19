# base para plotar variáveis por estado para figuras (esqueleto apenas para organizar as pastas)
# brazil maps installation
#remotes::install_github("rpradosiqueira/brazilmaps")
library(brazilmaps)
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

saude <- read.csv("dados_infra_hospitalar/processados/indic_saude.csv")
covid <- read.csv("output/dados/model_table_glm_covid_IFHR.csv")
sus <- read.csv("dados_infra_hospitalar/brutos/sus_dependente.csv")

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
med_int_shape$label <- med_int_shape$sg_uf
med_int_shape$label[med_int_shape$sg_uf %in% c("DF", "ES","RJ", "SE", "AL", "PE", "PB", "RN", "SC", "GO", "PI", "AC")] <- NA
med_int_shape
fit_map <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_text("label", size = 0.5, remove.overlap = T) +
  tm_fill("IHFR",
          title = "Peak IHFR",
          style = "cont",
          colorNA = "gray") +
  tm_style("col_blind") +
  tm_layout(legend.title.size = 1,
            frame = FALSE,
            title = "A.",
            title.size = 0.8,
            title.position = c("left", "top"))
fit_map
#test <- locator()
# mapa intensivistas
med_int <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("prop_MI_100000",
          title = "Intensivists", style = "cont") +
  tm_style("col_blind") +
  tm_layout(legend.title.size = 1,
            frame = FALSE,
            title = "B.",
            title.size = 0.8,
            title.position = c("left", "top"))
med_int

med <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("prop_med_100000",
          title = "Physicians", style = "cont") +
  tm_style("col_blind") +
  tm_layout(legend.title.size = 1,
            frame = FALSE,
            title = "C.",
            title.size = 0.8,
            title.position = c("left", "top"))
med

# mapa intensivistas
uti <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("UTI_100000",
          title = "ICU",
          style = "cont") +
  tm_style("col_blind") +
  tm_layout(legend.title.size = 1,
            frame = FALSE,
            title = "D.",
            title.size = 0.8,
            title.position = c("left", "top"))
uti

##mapa plano sus dependente

sus <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("sus_dep_hab",
          title = "% SUS-dependent",
          style = "cont") +
  tm_style("col_blind") +
  tm_layout(legend.title.size = 1,
            #legend.outside = TRUE,
            frame = FALSE,
            title = "E.",
            title.size = 0.8,
            title.position = c("left", "top"))
# sus

a <- tmap_arrange(fit_map, med_int, med, uti, sus)
a
tmap_save(a, "plots/maps/indicadores_saude.png")
tmap_save(a, "plots/maps/indicadores_saude.svg")


# Scatter plots ----------------------------------------------------------------
# Com correlacao de Spearman
y_pos <- 0.86

a <- ggplot(med_int_shape, aes(x = prop_MI_100000, y = IHFR)) +
  geom_point(size = size) +
  stat_cor(method = "spearman", label.x = 3.8, label.y = y_pos, size = 3) +
  theme_classic(base_size = 10) +
  xlab("Intensivists/100,000 hab") +
  ylab("IHFR peak")


b <- ggplot(med_int_shape, aes(x = prop_med_100000, y = IHFR)) +
  geom_point(size = size) +
  stat_cor(method = "spearman", label.x = 250, label.y = y_pos, size = 3) +
  theme_classic(base_size = 10) +
  #geom_smooth(method = lm) +
  xlab("Physicians/100,000 hab") +
  ylab("")


c <- ggplot(med_int_shape, aes(x = UTI_100000, y = IHFR)) +
  geom_point(size = size) +
  stat_cor(method = "spearman", label.x = 35, label.y = y_pos, size = 3) +
  theme_classic(base_size = 10) +
  xlab("ICU beds/100,000 hab") +
  ylab("IHFR peak")

d <- ggplot(med_int_shape, aes(x = sus_dep_hab, y = IHFR)) +
  geom_point(size = size) +
  stat_cor(method = "spearman", label.x = 0.77, label.y = y_pos, size = 3) +
  theme_classic(base_size = 10) +
  xlab("SUS dependent %") +
  ylab("")

ggarrange(a, b, c, d,
          labels = paste0(LETTERS[1:4], "."),
          font.label = list(size = 11, face = "plain"))
ggsave("plots/maps/scatter_plot_end.png", width = 6, height = 5)

