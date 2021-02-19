# Script para graficos

## aqui tem três opções de gráfico:
## 1. só pontos;
## 2. só pontos logit;
## 3. pontos com as barras ao fundo (tem que ser um a um)
## 4. uma forma de juntar os graficos do item 3 com grid.arrange

# Libraries
library(ggplot2)
library(dplyr)
library(wesanderson)
library(stringr)
library(readr)
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
#last_date <-  "2020_09_28"

pal <- wes_palette("Zissou1", 4, "continuous")

# Dados, valores previstos pelo modelo e IC 95%
df_covid_glm <- read_csv("output/dados/model_table_glm_covid_IFHR.csv")
df_srag_glm <- read_csv("output/dados/model_table_glm_srag_IFHR.csv")

# Dados de hospitalizados para barra no fundo do gráfico
hosp_week <- read_csv("output/dados/hospitalizados_srag_week_2020_09_28.csv")

# Deixando a classe preparada para legenda
class_new <- c("0-19", "20-39", "40-59", "60 or over")
names(class_new) <- unique(df_covid_glm$age_clas)

df_covid_glm$age_clas <- str_replace_all(df_covid_glm$age_clas, class_new)
df_srag_glm$age_clas <- str_replace_all(df_srag_glm$age_clas, class_new)

# Ordenando pela maior letalidade
uf_ordered <- df_covid_glm %>% filter(age_clas == "60 or over") %>%
  group_by(sg_uf) %>%
  summarise(max = max(fit)) %>%
  arrange(desc(max)) %>%
  select(sg_uf) %>%
  pull()

# Definindo titulos comuns
leg_title <- "Age class"
title <- "COVID"

# 1. Plot sem as barras ----
# plot function
plot_mod <- function(df,
                     lwr = 'lwr',
                     upr = 'upr',
                     title = "COVID-19") {
  ggplot(df, aes(x = week, y = let_obs, group = age_clas)) +
    geom_point(aes(color = age_clas), size = 0.95, alpha = 0.7) +
    #geom_line(aes(color = age_clas), size = 0.1) +
    geom_line(aes(y = fit, color = age_clas), size = 0.2) +
    scale_color_manual(values = pal, name = leg_title) +
    #scale_color_brewer(palette = "Dark2") +
    facet_wrap(~sg_uf, ncol = 4) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = age_clas), alpha = 0.2) +
    scale_fill_manual(values = pal, name = leg_title) +
    theme_classic() +
    theme(legend.position = c(0.88, 0.03),
          legend.direction = "vertical") +
    xlab("Week") +
    ylab("In-hospital lethality") +
    theme(strip.background = element_blank()) +
    ggtitle(title)
}

# Exportando os gráficos de covid e srag
png("plots/model_selection/glm_covid_week_state.png", res = 300, width = 2400, height = 2400)
plot_mod(df = df_covid_glm)
dev.off()

png("plots/model_selection/glm_srag_week_state.png", res = 300, width = 2100, height = 2100)
plot_mod(df = df_srag_glm, title = "SRAG")
dev.off()


# 2. Plot logit sem as barras ----
# plot logit function
logit <- function(x) log(x)/log(1 - x)

plot_logit <- function(df,
                     lwr = 'lwr',
                     upr = 'upr',
                     title = "COVID-19") {
  ggplot(df, aes(x = week, y = let_obs, group = age_clas)) +
    geom_point(aes(color = age_clas), size = 0.1, alpha = 0.7) +
    scale_color_manual(values = pal, name = leg_title) +
    #geom_line(aes(color = age_clas), size = 0.1) +
    geom_line(aes(y = fit, color = age_clas), size = 0.2) +
    scale_color_manual(values = pal, name = leg_title) +
    facet_wrap(~sg_uf, ncol = 4) +
    coord_trans(y = "logit") +
    ylim(0.01, 0.9) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = age_clas), alpha = 0.2) +
    scale_fill_manual(values = pal, name = leg_title) +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          strip.background = element_blank()) +
    xlab("Week") +
    ylab("In-hospital lethality") +
    ggtitle(title)
}


# Exportando os gráficos de covid e srag
png("plots/model_selection/glm_logit_covid_week_state.png", res = 300, width = 2400, height = 2400)
plot_logit(df = df_covid_glm)
dev.off()

png("plots/model_selection/glm_logit_srag_week_state.png", res = 300, width = 2100, height = 2100)
plot_logit(df = df_srag_glm, title = "SRAG")
dev.off()


# 3. Plot com as barras de hospitalizados ao fundo ----

# Juntando os dados de covid e srag com hospitalizados
df_covid <- left_join(df_covid_glm, hosp_week, by = c("week", "sg_uf"))
df_srag <- left_join(df_srag_glm, hosp_week) %>%
  filter(!is.na(hosp))

# Criando o vetor com o fator para o segundo eixo
glm_df <- df_covid %>%
  group_by(sg_uf) %>%
  mutate(fac = ceiling(max(hosp)/100)*100, bar = hosp/fac)

glm_df_srag <- df_srag %>%
  group_by(sg_uf) %>%
  mutate(fac = ceiling(max(hosp)/100)*100, bar = hosp/fac) %>%
  filter(!is.na(hosp))

# funcao para plotar e salvar por estado
plot_ufs_fac <- function(glm_df,
                         file_prefix = "covid",
                         x_lab = "",
                         y_lab = "",
                         axis_size = 6,
                         strip_size = 6) {
  fac_df <- glm_df %>% select(sg_uf, fac) %>% distinct() %>% arrange(desc(fac))
  # vai ter que ser plot por plot maalesef
  for (i in seq_along(fac_df$sg_uf)) {
    uf <- fac_df$sg_uf[i]
    fac_lab <- fac_df$fac[i]
    glm_df2 <- glm_df %>% filter(sg_uf == uf)
    plott <- ggplot(glm_df2, aes(x = week, y = fit)) +
      geom_bar(data = glm_df2, aes(x = week, y = bar, width = 1),
               stat = "identity", fill = "gray",
               position = position_dodge(), alpha = 0.15) +
      geom_line(aes(color = age_clas), size = 0.2) +
      geom_point(aes(x = week, y = let_obs, color = age_clas), size = 0.75, alpha = 0.7) +
      scale_color_manual(values = pal, name = leg_title) +
      facet_wrap(~ sg_uf, ncol = 4, scales = "free") +
      #geom_text(aes(label = fac_lab, x = 30, y = 1)) + # só para checar que a escala está boa tirar quando for fazer
      geom_ribbon(aes(ymin = lwr,
                      ymax = upr,
                      fill = age_clas), alpha = 0.2) +
      scale_fill_manual(values = pal, name = leg_title) +
      theme_classic() +
      theme(legend.position = "none",
            #legend.position = element_blank(),
            legend.direction = "horizontal",
            axis.text = element_text(size = axis_size),
            strip.text = element_text(hjust = 0, size = strip_size, color = "white"),
            strip.background = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0.05), "cm")
      ) +
      xlab(x_lab) +
      scale_y_continuous(
        name = y_lab,
        limits = c(0, 1),
        sec.axis = sec_axis(~ . * fac_lab))
    print(plott)
    ggsave(filename = paste0("plots/model_selection/estados/", file_prefix, "_", uf, ".png"), width = 45, height = 35, units = "mm")
    #assign(uf, plott, envir = .GlobalEnv) #para ver se dá para usar ggarrange, sqn
  }
}

# Rodando a funcao para covid e srag
plot_ufs_fac(glm_df = glm_df, strip_size = 8)
#plot_ufs_fac(glm_df = glm_df_srag, file_prefix = "srag")

# 4. Mais uma tentativa de grid, NOT a fracasito ----
filename <- list.files("plots/model_selection/estados", pattern = 'covid', full.names = TRUE)

# Função para printar a imagem no gráfico
fracasito  <- function(filename){
  p <- ggdraw() +
    draw_image(filename)
  return(p)
}

uf <- sapply(strsplit(filename, "covid_"), function(x) x[2]) %>%
  gsub(".png", "", .)

plot_list <- lapply(filename, fracasito)
for (i in 1:length(filename)) {assign(uf[i], plot_list[[i]], envir = .GlobalEnv)}

# Função acessória para extrair a legenda
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Extraindo a legenda do plot do item 1
ex <- plot_mod(df_covid_glm) +
  theme(legend.position = c(0.95, 0.6),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.direction = "horizontal")
legenda <- get_legend(ex)


png("plots/model_selection/main_figure.png",
    width = 180, height = 210, units = "mm", res = 300)
ggarrange(
  # by hand, it happens...
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
