# Script to plot IHFR and hospitalizations -------------------------------------

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
source("functions/get_legend.R")

pal <- wes_palette("Zissou1", 4, "continuous")

# Reading model outputs
df <- read_csv("outputs/model_table_glm_covid_IFHR.csv")

# Hospitalization data
hosp_week <- read_csv("data/processed/hospitalizados_srag_week_2020_09_28.csv")

# Converting age group for the legend
class_new <- c("0-19", "20-39", "40-59", "60 or over")
names(class_new) <- unique(df$age_clas)

df$age_clas <- str_replace_all(df$age_clas, class_new)
df$age_clas <- str_replace_all(df$age_clas, class_new)

# Ordering states by letality
uf_ordered <- df %>% filter(age_clas == "60 or over") %>%
  group_by(sg_uf) %>%
  summarise(max = max(fit)) %>%
  arrange(desc(max)) %>%
  select(sg_uf) %>%
  pull()


# 1. Basic plot ----------------------------------------------------------------
p1 <- ggplot(df, aes(x = week, y = let_obs, group = age_clas)) +
  geom_point(aes(color = age_clas), size = 0.95, alpha = 0.7) +
  geom_line(aes(y = fit, color = age_clas), size = 0.2) +
  scale_color_manual(values = pal, name = "Age class") +
  facet_wrap(~sg_uf, ncol = 4) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = age_clas), alpha = 0.2) +
  scale_fill_manual(values = pal, name = "Age class") +
  theme_classic() +
  xlab("Week") +
  ylab("In-hospital lethality") +
  theme(strip.background = element_blank(),
        legend.position = c(0.95, 0.6),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.direction = "horizontal")

# Extracting legend from p1
legenda <- get_legend(p1)

# 2. Plotting for each state separately ----------------------------------------

# Binding IHFR data and hospitalizations
df_hosp <- left_join(df, hosp_week, by = c("week", "sg_uf")) %>%
  # Creating column w/ factor for the second axis
  group_by(sg_uf) %>%
  mutate(fac = ceiling(max(hosp)/100) * 100, bar = hosp/fac)

# Making plots by UF using accessory function
plot_ufs_fac(df = df_hosp, strip_size = 8, strip_color = "white")


# 3. Reading plots on disk and arranging in a panel ----------------------------
# This is the only way to arrange graphics w/ two y-axis varying in scale

# 3.1. Drawing the plots -------------------------------------------------------
# File paths to figure
filename <- list.files("figs/ufs", pattern = 'covid', full.names = TRUE)

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


# 3.2. Arranging and saving the main plot --------------------------------------
tiff("figs/figure01.tiff",
     width = 180, height = 210, units = "mm", res = 300)
ggarrange(
  # by hand using ordered UF
  # paste(uf_ordered, collapse = ", ")
  AM, ES, RO, PA, AL, RJ, PB, MA, CE, BA, PE, RN, GO, MT, SP, RS, PI, MS, DF, SC, PR, MG,
  legenda,
  nrow = 6,
  ncol = 4,
  # left = "IHFR",
  # bottom = "Week",
  labels = paste0(LETTERS[1:22], ".   ", uf_ordered),
  font.label = list(size = 9, face = "plain")
)
dev.off()
