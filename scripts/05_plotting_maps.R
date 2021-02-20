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
med_int_shape$label[is.na(med_int_shape$label)] <- "MT"
#med_int_shape$label[med_int_shape$sg_uf %in% c("DF", "ES", "RJ", "SE", "AL", "PE", "PB", "RN", "SC", "GO", "PI", "AC")] <- NA

# 1. Peak IHFR map -------------------------------------------------------------
my_layout <- tm_layout(legend.title.size = 1,
                       frame = FALSE,
                       title.size = 0.8,
                       title.position = c("left", "top"))

fit_map <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_text("label", size = 0.5, remove.overlap = T, auto.placement = FALSE) +
  tm_fill("IHFR",
          title = "Peak IHFR",
          style = "cont",
          colorNA = "gray") +
  tm_style("col_blind") +
  tm_layout(title = "A.") +
  tm_text("") +
  my_layout

fit_map

# 2. Intensivists map ----------------------------------------------------------
med_int <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("pMI",
          title = "Intensivists", style = "cont") +
  tm_style("col_blind") +
  tm_layout(title = "B.") +
  my_layout

med_int

# 3. Physicians map ------------------------------------------------------------
med <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("pmed",
          title = "Physicians", style = "cont") +
  tm_style("col_blind") +
  tm_layout(title = "C.") +
  my_layout

med

# 4. ICU map -------------------------------------------------------------------
uti <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("UTI",
          title = "ICU",
          style = "cont") +
  tm_style("col_blind") +
  tm_layout(title = "D.") +
  my_layout

uti


# 5. SUS dependent--------------------------------------------------------------
sus <- tm_shape(med_int_shape) +
  tm_borders() +
  tm_fill("SUS",
          title = "% SUS-dependent",
          style = "cont") +
  tm_style("col_blind") +
  tm_layout(title = "E.") +
  my_layout

sus

# 6. Arrange and save -------------------------------------------------------------
a <- tmap_arrange(fit_map, med_int, med, uti, sus)
a
tmap_save(a, "figs/figure_02.tiff")
tmap_save(a, "figs/figure_02.svg")

# Same figure, now black and white
aa <- tmap_arrange(fit_map + tm_style("bw") + tm_layout(title = "A.") + my_layout,
                   med_int + tm_style("bw") + tm_layout(title = "B.") + my_layout,
                   med + tm_style("bw") + tm_layout(title = "C.") + my_layout,
                   uti + tm_style("bw") + tm_layout(title = "D.") + my_layout,
                   sus + tm_style("bw") + tm_layout(title = "E.") + my_layout)
aa
tmap_save(aa, "figs/figure_02_bw.tiff")
tmap_save(aa, "figs/figure_02_bw.svg")

