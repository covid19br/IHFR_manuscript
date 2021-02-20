#' Function to plot IHFR data and hospitalizations with two y-axis
#'
#' @param df data frame for plotting
#' @param file_prefix file prefix
#' @param x_lab label for x-axis
#' @param y_lab label for y-axis
#' @param axis_size size for axis
#' @param strip_size size for strip text
#' @param strip_color color for strip text
#' @param bw Logical. Use Black and white color palette
#' @param pal string with color palette
#'
#' @return Returns the legend with respect to the graphics written on disk
#'
#' @export
#'
#' @examples
plot_ufs_fac <- function(df,
                         file_prefix = "covid",
                         x_lab = "",
                         y_lab = "",
                         axis_size = 6,
                         strip_size = 6,
                         strip_color,
                         bw = FALSE,
                         pal = pal) {

  leg_title <- "Age class"

  fac_df <- df %>% select(sg_uf, fac) %>% distinct() %>% arrange(desc(fac))

  # Loop over states ----
  for (i in seq_along(fac_df$sg_uf)) {
    uf <- fac_df$sg_uf[i]
    fac_lab <- fac_df$fac[i]
    df2 <- df %>% filter(sg_uf == uf)

    my_theme <- theme(legend.position = "none",
                      axis.text = element_text(size = axis_size),
                      strip.text = element_text(hjust = 0, size = strip_size, color = strip_color),
                      strip.background = element_blank(),
                      plot.margin = unit(c(0, 0, 0, 0.05), "cm"))

    legend_theme <- theme(strip.background = element_blank(),
                          legend.position = c(0.95, 0.6),
                          legend.title = element_text(size = 7),
                          legend.text = element_text(size = 7),
                          legend.direction = "horizontal")

    # Making the colored plot ----
    if (!bw) {

    plott <- ggplot(df2, aes(x = week, y = fit)) +
      geom_bar(data = df2, aes(x = week, y = bar, width = 1),
               stat = "identity", fill = "gray",
               position = position_dodge(), alpha = 0.15) +
      geom_line(aes(color = age_clas), size = 0.2) +
      geom_point(aes(x = week, y = let_obs, color = age_clas), size = 0.75, alpha = 0.7) +
      scale_color_manual(values = pal, name = leg_title) +
      facet_wrap(~ sg_uf, ncol = 4, scales = "free") +
      geom_ribbon(aes(ymin = lwr,
                      ymax = upr,
                      fill = age_clas), alpha = 0.2) +
      scale_fill_manual(values = pal, name = leg_title) +
      theme_classic() +
      xlab(x_lab) +
      scale_y_continuous(
        name = y_lab,
        limits = c(0, 1),
        sec.axis = sec_axis(~ . * fac_lab)) +
      legend_theme


    } else {

      # Making the Black and White plot ----
      pal <- rep("black", 4)

      plott <- ggplot(df2, aes(x = week, y = fit)) +
        geom_bar(data = df2, aes(x = week, y = bar, width = 1),
                 stat = "identity", fill = "gray",
                 position = position_dodge(), alpha = 0.15) +
        geom_line(aes(color = age_clas), size = 0.2) +
        scale_color_manual(values = pal, name = leg_title) +
        geom_point(aes(x = week, y = let_obs, shape = age_clas), size = 0.75, alpha = 0.7) +
        #scale_color_manual(values = pal, name = leg_title) +
        scale_shape(name = leg_title) +
        facet_wrap(~ sg_uf, ncol = 4, scales = "free") +
        geom_ribbon(aes(ymin = lwr,
                        ymax = upr,
                        fill = age_clas), alpha = 0.2) +
        scale_fill_manual(values = pal, name = leg_title) +
        theme_classic() +
        xlab(x_lab) +
        scale_y_continuous(
          name = y_lab,
          limits = c(0, 1),
          sec.axis = sec_axis(~ . * fac_lab)) +
        legend_theme

    }

    legenda <- get_legend(plott)

    plott <- plott + my_theme
    print(plott)

    # Saving plot ----
    if (!dir.exists("figs/ufs")) {dir.create("figs/ufs", recursive = TRUE)}

    ggsave(filename = paste0("figs/ufs/", file_prefix, "_", uf, ".png"), width = 45, height = 35, units = "mm")
  }
  return(legenda)
}
