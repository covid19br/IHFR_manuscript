# Script to plot in-hospital mortality and hospitalizations

# Libraries
library(ggplot2)
library(dplyr)
library(wesanderson)
library(stringr)
library(readr)
library(ggpubr)
library(cowplot)
# Also library libmagick++-dev needed

# Loading functions
source("functions/plots_ufs_fac.R")

pal <- wes_palette("Zissou1", 4, "continuous")

# Reading model outputs
df <- read_csv("outputs/model_table_glm_covid_IFHR.csv")

# Hospitalization data
hosp_week <- read_csv("data/processed/hospitalizados_srag_week_2021_03_26.csv")

# Converting age group for the legend
class_new <- c("0-19", "20-39", "40-59", "60-74", "75 or over")
names(class_new) <- unique(df$age_clas)

df$age_clas <- str_replace_all(df$age_clas, class_new)
df$age_clas <- str_replace_all(df$age_clas, class_new)

# Ordering states by letality
uf_ordered <- df %>% filter(age_clas == "75 or over") %>%
  group_by(sg_uf) %>%
  summarise(max = max(fit)) %>%
  arrange(desc(max)) %>%
  select(sg_uf) %>%
  pull()


# 1. Plotting for each state separately ----------------------------------------

# Binding in-hospital mortality data and hospitalizations
df_hosp <- left_join(df, hosp_week, by = c("week", "sg_uf")) %>%
  # Creating column w/ factor for the second axis
  group_by(sg_uf) %>%
  mutate(fac = ceiling(max(hosp)/100) * 100, bar = hosp/fac)

# Making plots by UF using accessory function
legenda <- plot_ufs_fac(df = df_hosp, strip_size = 8, strip_color = "white", pal = pal)
legenda_bw <- plot_ufs_fac(df = df_hosp, file_prefix = "bw_covid", strip_size = 8, strip_color = "white", bw = TRUE)


# 2. Reading plots on disk and arranging in a panel ----------------------------
# This is the only way to arrange graphics w/ two y-axis varying in scale

# 2.1. Drawing the plots -------------------------------------------------------
# File paths to figure
filename <- list.files("figs/ufs", pattern = '^covid', full.names = TRUE)

# Getting the exact UF acronym with respect to the filename
uf <- sapply(strsplit(filename, "covid_"), function(x) x[2]) %>%
  gsub(".png", "", .)

# Drawing the graphs
plot_list <- list()

for (i in 1:length(filename)) {
  # making plots
  plot_list[[i]] <- ggdraw() +
    draw_image(filename[i])
  # creating objects named after the UF
  assign(uf[i], plot_list[[i]], envir = .GlobalEnv)
}


# 2.2. Arranging and saving the main plot --------------------------------------
tiff("figs/figure_01.tiff",
     width = 180, height = 210, units = "mm", res = 300)
ggarrange(
  # by hand using ordered UF
  # paste(uf_ordered, collapse = ", ")
  SE, ES, RO, AM, PA, TO, PE, AP, RJ, MA, CE, AL, PB, MS, AC, RN, GO, BA, RS, PI, MT, SC, MG, PR, DF, SP,
  legenda,
  nrow = 7,
  ncol = 4,
  # left = "In-hospital mortality",
  # bottom = "Week",
  labels = paste0(LETTERS[seq_along(uf_ordered)], ".   ", uf_ordered),
  font.label = list(size = 9, face = "plain")
)
dev.off()

# 3. Black and white plots -----------------------------------------------------

# 3.1. Drawing the plots -------------------------------------------------------
# File paths to figure
filename_bw <- list.files("figs/ufs", pattern = '^bw_covid', full.names = TRUE)

# Getting the exact UF acronym with respect to the filename
uf_bw <- sapply(strsplit(filename_bw, "covid_"), function(x) x[2]) %>%
  gsub(".png", "", .)

# Drawing the graphs
plot_list_bw <- list()

for (i in 1:length(filename_bw)) {
  # making plots
  plot_list_bw[[i]] <- ggdraw() +
    draw_image(filename_bw[i])
  # creating objects named after the UF
  assign(uf_bw[i], plot_list_bw[[i]], envir = .GlobalEnv)
}


# 3.2. Arranging and saving the main plot --------------------------------------
tiff("figs/figure_01_bw.tiff",
     width = 180, height = 210, units = "mm", res = 300)
ggarrange(
  # by hand using ordered UF
  # paste(uf_ordered, collapse = ", ")
  SE, ES, RO, AM, PA, TO, PE, AP, RJ, MA, CE, AL, PB, MS, AC, RN, GO, BA, RS, PI, MT, SC, MG, PR, DF, SP,
  legenda_bw,
  nrow = 7,
  ncol = 4,
  # left = "In-hospital mortality",
  # bottom = "Week",
  labels = paste0(LETTERS[1:22], ".   ", uf_ordered),
  font.label = list(size = 9, face = "plain")
)
dev.off()
