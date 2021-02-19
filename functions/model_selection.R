#' Function to fit glm to IHFR data
#'
#' @param data data.frame containing the columns: week_uf, age_aclass, sg_uf
#'
#' @return
#'
#' @export
#'
#' @examples
model_selection <- function(data) {
  ## Cheio: semana, idade, uf, com letalidade diferencial entre estados
  m_full <- glm(cbind(obitos, sobre) ~ week_uf + I(week_uf^2) + age_clas + sg_uf +
                  week_uf:age_clas + I(week_uf^2):age_clas +
                  week_uf:sg_uf + I(week_uf^2):sg_uf +
                  age_clas:sg_uf,
                family = binomial(link = "logit"), data = data)
  ## Apenas aditivos de semana, idade e uf
  m_1 <- glm(cbind(obitos, sobre) ~ week_uf + I(week_uf^2) + age_clas + sg_uf,
             family = binomial(link = "logit"), data = data)
  ## Apenas efeito da semana e idade, não há diferença entre estados
  m_2 <- glm(cbind(obitos, sobre) ~ week_uf + I(week_uf^2) + age_clas +
               week_uf:age_clas + I(week_uf^2):age_clas,
             family = binomial(link = "logit"), data = data)
  ## Apenas efeito aditivo da semana e idade, não há diferença entre estados
  m_3 <- glm(cbind(obitos, sobre) ~ week_uf + I(week_uf^2) + age_clas,
             family = binomial(link = "logit"), data = data)
  ## Apenas efeito da semana e estado, não há diferença entre classe etária
  m_4 <- glm(cbind(obitos, sobre) ~ week_uf + I(week_uf^2) + sg_uf +
               week_uf:sg_uf + I(week_uf^2):sg_uf,
             family = binomial(link = "logit"), data = data)
  ## Apenas efeito aditivo da semana e estado, não há diferença entre classe etária
  m_5 <- glm(cbind(obitos, sobre) ~ week_uf + I(week_uf^2) + sg_uf,
             family = binomial(link = "logit"), data = data)
  ## Apenas o efeito do tempo
  m_6 <- glm(cbind(obitos, sobre) ~ week_uf + I(week_uf^2),
             family = binomial(link = "logit"), data = data)
  ## Comparando estes modelos com AIC
  res <- list(modelos = list(m_full = m_full,
                             m_1 = m_1, m_2 = m_2, m_3 = m_3,
                             m_4 = m_4, m_5 = m_5, m_6 = m_6),
              aic_tab = AICctab(m_full, m_1, m_2, m_3, m_4, m_5, m_6, base = TRUE, weights = TRUE))
  return(res)
}
