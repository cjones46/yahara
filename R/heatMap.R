#' Create the heat map for the Yahara interface.
#'
#' @param data A dataframe of coefficients to plot, from heatMapCoefs().
#' @param outcome A string indicating what term, if any, has been selected.
#' @param option A string indicating what option, if any, has been selected.
#' @param option_order The result of `getOptionOrder()`, a character vector indicating the order of the terms.
#' @param pretty_ordering_word A string to be used in the title indicating what target quantity is being graphed.
#' @param term_order The result of `getOptionOrder()`, a character vector indicating the order of the terms.
#' @param pretty_value_word A string to be used in the title indicating what target quantity is being graphed.
#' @param transparent_y_axis_label The longest y-axis label from the parallel coordinates plot, to ensure widths are consistent.
#' @param my_colors A Yahara-specific list of colors.
#'
#' @returns A ggplot object.
myHeatMap = function(data, outcome, option, option_order, pretty_ordering_word, term_order, pretty_value_word, transparent_y_axis_label, my_colors) {
  myMargin = 0.5
  m = max(abs(data$value), na.rm = T)

  plot = data %>%
    ggplot(aes(y = factor(options, levels=option_order), x = response, fill = value)) +
    geom_tile(width = 0.75, height = 0.75) +
    scale_fill_viridis_c(na.value = "white", option = "cividis", limits = c(-m, m))

  plot = plot +
    annotate("segment",
             x = 1:length(term_order),
             xend = 1:length(term_order),
             y = -Inf,
             yend = Inf,
             color = my_colors[["default"]]) +
    annotate("segment",
             x = which(term_order == outcome),
             xend = which(term_order == outcome),
             y = -Inf,
             yend = Inf,
             color = "black", linewidth = 1.5)

  if (option != "none") {
    plot = plot +
      annotate("rect",
               xmin = which(term_order == outcome) - myMargin,
               xmax = which(term_order == outcome) + myMargin,
               ymin = which(option_order == option) - myMargin,
               ymax = which(option_order == option) + myMargin,
               color = "black", fill = NA, linewidth = 2)
  }

  return(plot +
           theme(legend.position = "bottom",
                 axis.ticks = element_blank(),
                 #axis.ticks.x = element_line()
                 panel.grid = element_blank(),
                 axis.text.y = element_text(color = "transparent"),
                 axis.text.x = element_blank()
           ) +
           scale_x_discrete(
             sec.axis = dup_axis(name = NULL, labels = NULL,
                                 guide = guide_axis(
                                   theme = theme(
                                     axis.ticks.x = element_line()
                                   )
                                 ))
           ) +
           scale_y_discrete(
             labels = rep(transparent_y_axis_label, length(option_order)),
             sec.axis = dup_axis(name = NULL, labels = NULL,
                                 guide = guide_axis(
                                   theme = theme(
                                     axis.ticks.y = element_line()
                                   )
                                 ))
           ) +
           labs(x = NULL, y = "", fill = NULL, caption = paste0("<span style = 'font-size:20pt'>Effects of Multiverse Options on ", pretty_value_word, ": <span style = 'color:#EE2C2C'>Click</span> me!<br><span style = 'font-size:15pt'>White rows represent reference categories.<br></span>")))
}
