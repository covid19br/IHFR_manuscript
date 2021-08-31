# Model selection

# Function using gam -----------------------------------------------------------
gam_model_selection <- function(data) {

  message("Fitting m_full ...")
  m_full <- gam(cbind(obitos, sobre) ~ age_clas + sg_uf + s(week_uf, by = age_clas) +
                   s(week_uf, by = sg_uf), method = "REML",  data = data,
                 family = binomial("logit"))
  ## Apenas aditivos de semana, idade e uf
  message("Fitting m_1 ...")
  m_1 <- gam(cbind(obitos, sobre) ~ s(week_uf) + age_clas + sg_uf,
             method = "REML", data = data,
             family = binomial("logit"))
  ## Apenas efeito da semana e idade, nao ha diferenca entre estados
  message("Fitting m_2 ...")
  m_2 <- gam(cbind(obitos, sobre) ~  age_clas + s(week_uf, by = age_clas),
             method = "REML",  data = data,
             family = binomial("logit"))
  ## Apenas efeito aditivo da semana e idade, nao ha diferenca  entre estados
  message("Fitting m_3 ...")
  m_3 <- gam(cbind(obitos, sobre) ~ s(week_uf) + age_clas,
             method = "REML",  data = data,
             family = binomial("logit"))
  ## Apenas efeito da semana e estado, nao ha diferenca entre classe etaua
  message("Fitting m_4 ...")
  m_4 <- gam(cbind(obitos, sobre) ~  sg_uf + s(week_uf, by = sg_uf),
             method = "REML",  data = data,
             family = binomial("logit"))
  ## Apenas efeito aditivo da semana e estado, nao ha diferenca entre classe etaria
  message("Fitting m_5 ...")
  m_5 <- gam(cbind(obitos, sobre) ~ s(week_uf)+ sg_uf,
             method = "REML",  data = data,
             family = binomial("logit"))
  ## Apenas o efeito do tempo
  message("Fitting m_6 ...")
  m_6 <- gam(cbind(obitos, sobre) ~ s(week_uf),
             method = "REML",  data = data,
             family = binomial("logit"))
  ## Comparando estes modelos com AIC
  res <- list(modelos = list(m_full = m_full,
                             m_1 = m_1, m_2 = m_2, m_3 = m_3,
                             m_4 = m_4, m_5 = m_5, m_6 = m_6),
              aic_tab = AICctab(m_full, m_1, m_2, m_3, m_4, m_5, m_6, base = TRUE, weights = TRUE))
  return(res)
}
