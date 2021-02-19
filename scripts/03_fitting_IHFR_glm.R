# Libraries
library(readr)
library(dplyr)
library(bbmle)
library(tidyr)
# Loading functions
source("../nowcasting/fct/get.last.date.R")
source("functions/cut_weeks.R")
source("functions/model_selection.R")

# Loading and filtering data ---------------------------------------------------
last_date <- "2020_09_28"

covid <- read_csv(paste0("output/dados/summary_covid_IFHR_", last_date, ".csv"))
srag <- read_csv(paste0("output/dados/summary_srag_IFHR_", last_date, ".csv"))

# Filtering weeks and cases: from 50rd case and cutting the last 4 weeks -------
cut_ini <- 30 #cases
cut_end <- 4 #weeks

# Using function to cut weeks and add epi week column standardized for all states
df_covid <- cut_weeks(covid, cut_ini, cut_end)
df_srag <- cut_weeks(srag, cut_ini, cut_end)

# Checking N per state
n_covid <- df_covid %>%
  group_by(sg_uf) %>%
  summarise(n = sum(sobre))

n_srag <- df_srag %>%
  group_by(sg_uf) %>%
  summarise(n = sum(sobre))

# Selecting only states w/ N > 1000
estados <- n_covid$sg_uf[n_covid$n > 1000]

df_covid <- df_covid %>%
  filter(sg_uf %in% estados)

df_srag <- df_srag %>%
  filter(sg_uf %in% estados)

# Fitting glm------------------------------------------------------------------
mod_srag <- model_selection(df_srag)
mod_covid <- model_selection(df_covid)

# Model selection results
mod_srag$aic_tab
mod_covid$aic_tab

# Inspecting coefficients from the best model
summary(mod_srag$modelos$m_full)
summary(mod_covid$modelos$m_full)

# Calculating predicted values + SE
## COVID
pred_covid_glm <- predict(mod_covid$modelos$m_full, type = "response", se.fit = TRUE)
df_covid_glm <- data.frame(df_covid, fit = pred_covid_glm$fit,
                           upr = pred_covid_glm$fit + 2*pred_covid_glm$se.fit,
                           lwr = pred_covid_glm$fit - 2*pred_covid_glm$se.fit)

## SRAG
pred_srag_glm <- predict(mod_srag$modelos$m_full, type = "response", se.fit = TRUE)
df_srag_glm <- data.frame(df_srag, fit = pred_srag_glm$fit, se.fit = pred_srag_glm$se.fit,
                          upr = pred_srag_glm$fit + 2*pred_srag_glm$se.fit,
                          lwr = pred_srag_glm$fit - 2*pred_srag_glm$se.fit)


# Formatting AIC table
aic_df <- as.data.frame(mod_covid$aic_tab) %>%
  mutate(across(where(is.numeric), round, 2),
         model = c("Week * Age + Week * UF",
                   "Week + Age + UF",
                   "Week * Age",
                   "Week + Age",
                   "Week * UF",
                   "Week + UF",
                   "Week"),
         weight = ifelse(.$weight == 1, "1", "<0.0001")) %>%
  relocate(model, .before = AICc)

# Exporting tables -------------------------------------------------------------
write.csv(df_covid_glm, paste0("output/dados/model_table_glm_covid_IFHR.csv"),
          row.names = FALSE)
write.csv(df_srag_glm, paste0("output/dados/model_table_glm_srag_IFHR.csv"),
          row.names = FALSE)
write.csv(aic_df, paste0("output/dados/aic_table_glm_srag_IFHR.csv"),
          row.names = TRUE)
