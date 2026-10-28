#' Create the marginal decisions plot for the Yahara interface.
#'
#' @param mv_data A Yahara-specific list of multiverse data.
#' @param universes A numeric vector of universes to highlight, either via brushing or the highlight argument.
#' @param option A string indicating which option has been selected, if any.
#' @param option_order A character vector indicating the order of the options.
#' @param pretty_ordering_word A string to be used in the title indicating how the options are being ordered. Note to self - this argument is no longer used.
#' @param my_colors A Yahara-specific list of colors.
#'
#' @returns A ggplot object.
marginalDecisions = function(mv_data, universes, option, option_order, pretty_ordering_word, my_colors) {

  if (option != "none") {
    lollipop_data = mv_data[["WIDE_MV"]] %>%
      mutate(selected = .data[[option]])
  } else if (!mean(mv_data[["WIDE_MV"]]$universe %in% universes) %in% 0:1) {
    lollipop_data = mv_data[["WIDE_MV"]] %>%
      mutate(selected = universe %in% universes)
  } else {
    lollipop_data = mv_data[["WIDE_MV"]] %>%
      mutate(selected = TRUE)
  }

  lollipop_data = lollipop_data %>%
    select(universe, selected, all_of(mv_data[["MULTIVERSE_OPTIONS"]])) %>%
    distinct() %>%
    pivot_longer(-c(selected, universe)) %>%
    group_by(name, selected) %>%
    summarize(perc = mean(value, na.rm = T), .groups = "drop_last") %>%
    pivot_wider(names_from = selected, values_from = perc, names_prefix = "perc_among_") %>%
    ungroup()

  summary_all = mv_data[["WIDE_MV"]] %>%
    select(universe, all_of(mv_data[["MULTIVERSE_OPTIONS"]])) %>%
    distinct() %>%
    select(-universe) %>%
    summarize(across(everything(), mean)) %>%
    pivot_longer(everything()) %>%
    rename(perc_among_ALL = value)

  full_data = left_join(summary_all, lollipop_data, by = join_by(name)) %>%
    mutate(name = factor(name, levels = option_order))

  # Set up the default conditions for the plot
  plot = ggplot(full_data, aes(y = name))
  additionalSubtitleInfo = "<span style = 'font-size:15pt'>Brush the line plot or click the heat map for more detail here.</span>"
  defaultSize = 6

  # If there's a partial brush selection OR option, add the lollipop and update additionalSubtitleInfo.
  if ((!mean(mv_data[["WIDE_MV"]]$universe %in% universes) %in% 0:1) | option != "none") {
    plot = plot +
      geom_segment(aes(x = perc_among_ALL, xend = perc_among_TRUE), linewidth = 2, color = my_colors[["faded"]]) +
      geom_point(aes(x = perc_among_TRUE), color = my_colors[["selected"]], size = 1.1*defaultSize, pch = 15, na.rm = T)

    additionalSubtitleInfo = "<span style = 'font-size:15pt'>Red shows % among selected universes.</span>"

  }
  # Add the default gray points on top of whatever may or may not be there.
  plot = plot +
    geom_point(aes(x = perc_among_ALL), color = my_colors[["default"]], size = defaultSize, na.rm = T)

  boldSelected = rep("plain", length(option_order))
  boldSelected[which(option_order == option)] = "bold"
  redSelected = rep("black", length(option_order))
  redSelected[which(option_order == option)] = my_colors[["selected"]]

  return(
    plot +
      scale_x_continuous(labels = scales::percent, limits = c(0, 1.005)) +
      scale_y_discrete(
        labels = v_str_force(option_order, truncate = "middle", pad = "beginning", max.length = 15)
      ) +
      theme(
        legend.position = "none",
        axis.text.y = element_markdown(hjust = 0.5, face = boldSelected, color = redSelected),
        axis.ticks.x = element_blank()
      ) +
      labs(x = "\nPercent of Universes", y = NULL, caption = paste0("<br><br><br><br><br><span style = 'font-size:20pt'>Multiverse Option Frequency</span><br>", additionalSubtitleInfo))
  )
}
