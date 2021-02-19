# Script to plot maps ----------------------------------------------------------

# Libraries
library(tmap)
library(dplyr)
library(sf)
library(spData)
library(ggpubr)
library(GGally)

# Reading data
med_int_shape <- read_sf("data/shapefile/hospital_indicators.shp")

# Modifying map labels to fill later by hand
med_int_shape$label <- med_int_shape$sg_uf
med_int_shape$label[med_int_shape$sg_uf %in% c("DF", "ES","RJ", "SE", "AL", "PE", "PB", "RN", "SC", "GO", "PI", "AC")] <- NA

# 1. Peak IHFR map -------------------------------------------------------------
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

# 2. Intensivists map ----------------------------------------------------------
med_int <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("pMI",
          title = "Intensivists", style = "cont") +
  tm_style("col_blind") +
  tm_layout(legend.title.size = 1,
            frame = FALSE,
            title = "B.",
            title.size = 0.8,
            title.position = c("left", "top"))
med_int

# 3. Physicians map ------------------------------------------------------------
med <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("pmed",
          title = "Physicians", style = "cont") +
  tm_style("col_blind") +
  tm_layout(legend.title.size = 1,
            frame = FALSE,
            title = "C.",
            title.size = 0.8,
            title.position = c("left", "top"))
med

# 4. ICU map -------------------------------------------------------------------
uti <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("UTI",
          title = "ICU",
          style = "cont") +
  tm_style("col_blind") +
  tm_layout(legend.title.size = 1,
            frame = FALSE,
            title = "D.",
            title.size = 0.8,
            title.position = c("left", "top"))
uti


# 5. SUS dependent--------------------------------------------------------------
sus <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("SUS",
          title = "% SUS-dependent",
          style = "cont") +
  tm_style("col_blind") +
  tm_layout(legend.title.size = 1,
            #legend.outside = TRUE,
            frame = FALSE,
            title = "E.",
            title.size = 0.8,
            title.position = c("left", "top"))

sus

# 6.range and save -------------------------------------------------------------
a <- tmap_arrange(fit_map, med_int, med, uti, sus)
a
tmap_save(a, "figs/figure_02.tiff")
tmap_save(a, "figs/figure_02.svg")


