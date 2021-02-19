#' Function to cut weeks and create column w/ epi week relative to all states
#'
#' @param data data frame w/ week colum as numeric
#' @param cut_ini minimum cases to start counting
#' @param cut_end last n weeks to cut
#'
#' @export
#'
cut_weeks <- function(data, #data frame w/ week colum as numeric
                      cut_ini, #minimum cases to start counting
                      cut_end){ #last n weeks to cut

  ## Filtering last n weeks
  weeks <- unique(data$week)
  cut_weeks <- c(weeks[(length(weeks) - cut_end):length(weeks)])
  df <- data %>%
    filter(!week %in% cut_weeks) %>%
    drop_na()

  ## Finding week of first n cases per state
  #case per state
  data$hosp <- data$sobre + data$obitos
  week_uf <- aggregate(hosp ~ week + sg_uf, data = data, FUN = sum) %>%
    group_by(sg_uf) %>%
    filter(hosp >= cut_ini) %>%
    summarise(min = min(week))

  ## Adding colum of epi week per state to df
  df_week <- df %>%
    left_join(week_uf, by = "sg_uf") %>%
    mutate(week_uf = (week - min) + 1) %>%
    filter(week_uf > 0)

  return(df_week)
}
