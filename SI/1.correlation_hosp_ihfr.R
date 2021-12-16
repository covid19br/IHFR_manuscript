library(dplyr)
library(readr)
library(astsa)
library(stats)
library(ggplot2)
library(viridis)

df <- read_csv("outputs/model_table_gam_covid_IFHR.csv")
hosp_week <- read_csv("data/processed/hospitalizados_srag_week_2021-07-30.csv")


junt<-inner_join(df,hosp_week, by=c("week", "sg_uf"))

###fazendo a correlação das séries

corre<- junt %>% group_by(sg_uf,age_clas) %>%
                 summarise(corre=cor(fit,hosp))


level_order <- factor(corre$sg_uf, level=c("RS", "SC", "PR", "SP", "RJ","ES", "MG", "MS", "MT", "GO", "DF","BA", "SE",
                                            "AL","PE", "PB", "RN", "CE","PI", "MA","TO", "RO", "PA", "AM", "AC", "AP", "RR"))


tiff("SI/SI_figure_01.tiff",
         width = 200, height = 160, units = "mm", res = 300)

ggplot(corre,aes(age_clas,level_order, fill=corre))+
  geom_tile(size=0.1) + 
  scale_fill_gradient2(high = "red", mid="orange", low = "yellow")+
  #scale_fill_viridis(discrete = FALSE)+
  geom_text(aes(label = round(corre, 1))) +
  theme_bw(base_size = 12)+
  xlab("Age class")+
  ylab("State")+
  labs(fill = "Correlation")
  
dev.off()


write.csv(corre, "data/processed/correlation_hosp_ihfr.csv")
