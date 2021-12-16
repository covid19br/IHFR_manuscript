# Script to calculate odds ratio and its ratio before and after IHRF peak

# loading libraries
library(dplyr)
library(tidyr)
library(brazilmaps)
library(tmap)
library(readr)
library(ggplot2)
install.packages("questionr")

# Reading data
dados <- read.csv("outputs/model_table_gam_covid_IFHR.csv")


lista<-c(unique(dados$age_clas))

datalist<-list()

for ( i in lista) {
  
  age <- dados %>% filter(age_clas == i)
  
  # 1. IHFR inicial, final e pico ####
  
  # minimo
  min <- age %>% group_by(sg_uf) %>%
    filter(fit == min(fit)) %>%
    select(week, sg_uf, fit, lwr, upr)
  
  names(min) <- c("week_min", "sg_uf", "fit_min", "lwr_min", "upr_min")
  
  # Final
  max <- age %>% group_by(sg_uf) %>%
    filter(fit == max(fit)) %>%
    select(week, sg_uf, fit, lwr, upr)
  
  names(max) <- c("week_max", "sg_uf", "fit_max", "lwr_max", "upr_max")
  
  # juntando tudo
  
  odds <- full_join(min, max, by="sg_uf")
  
  # 2. Odds ratio inicio, fim e pico ####
  
  # Odds ratio na m]ao
  odds$or <- odds$fit_max/odds$fit_min
 

###criando uma lista com os data frames por faixa etária
  
  odds$age_clas<-i
  
  datalist[[i]]<-odds
}

###juntando os data frames por faixa etária
bind_data <- data.frame(bind_rows(datalist))

level_order <- factor(bind_data$sg_uf, level=c("RS", "SC", "PR", "SP", "RJ","ES", "MG", "MS", "MT", "GO", "DF","BA", "SE",
                                           "AL","PE", "PB", "RN", "CE","PI", "MA","TO", "RO", "PA", "AM", "AC", "AP", "RR"))


tiff("SI/SI_figure_02.tiff",
    width = 200, height = 160, units = "mm", res = 300)

ggplot(bind_data,aes(age_clas,level_order, fill=or))+
  geom_tile(size=0.1) + 
  scale_fill_gradient2(high = "red", mid="orange", low = "yellow", midpoint = 2.6, 
                       limits=c(0,6))+
  #scale_fill_viridis(discrete = FALSE)+
  geom_text(aes(label = round(or, 1))) +
  theme_bw(base_size = 12)+
  xlab("Age class")+
  ylab("State")+
  labs(fill = "Odds Ratio")

dev.off()
  
###salvando o csv

write.csv(bind_data,"data/processed/odds_ratio.csv")













