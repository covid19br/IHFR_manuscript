# Script to make scatter plots and correlation analysis
# With Spearman correlation

# Libraries
library(ggplot2)
library(ggpubr)

df <- read.csv("data/processed/04-med_int.csv")

# Fixing y-position for the text
y_pos <- 0.86
my_size <- 3

# 1. Intensivists --------------------------------------------------------------
a <- ggplot(df, aes(x = pMI, y = IHFR)) +
  geom_point(size = my_size) +
  stat_cor(method = "spearman", label.x = 3.8, label.y = y_pos, size = my_size) +
  theme_classic(base_size = 10) +
  xlab("Intensivists/100,000 hab") +
  ylab("In-hospital mortality peak")

# 2. Physicians ----------------------------------------------------------------
b <- ggplot(df, aes(x = pmed, y = IHFR)) +
  geom_point(size = my_size) +
  stat_cor(method = "spearman", label.x = 250, label.y = y_pos, size = my_size) +
  theme_classic(base_size = 10) +
  #geom_smooth(method = lm) +
  xlab("Physicians/100,000 hab") +
  ylab("")


# 3. ICU beds ------------------------------------------------------------------
c <- ggplot(df, aes(x = UTI, y = IHFR)) +
  geom_point(size = my_size) +
  stat_cor(method = "spearman", label.x = 35, label.y = y_pos, size = my_size) +
  theme_classic(base_size = 10) +
  xlab("ICU beds/100,000 hab") +
  ylab("In-hospital mortality peak")

# 4. SUS -----------------------------------------------------------------------
d <- ggplot(df, aes(x = SUS, y = IHFR)) +
  geom_point(size = my_size) +
  stat_cor(method = "spearman", label.x = 0.77, label.y = y_pos, size = my_size) +
  theme_classic(base_size = 10) +
  xlab("SUS dependent %") +
  ylab("")

ggarrange(a, b, c, d,
          labels = paste0(LETTERS[1:4], "."),
          font.label = list(size = 11, face = "plain"))
ggsave("figs/figure_03.tiff", width = 6, height = 5)
