#' Create the marginal histogram for the Yahara interface.
#'
#' @param mv_data Yahara-specific list of multiverse data.
#' @param y A string to indicate the target quantity variable to go on the y axis.
#' @param universes A numeric vector of universes to highlight, perhaps through brushing or the highlight argument.
#' @param outcome A string indicating which term has been selected, if any.
#' @param option A string indicating which option has been selected, if any.
#' @param topRowScale A reactive() coming from Yahara that helps with the top row margins.
#' @param sigLinesIntercept A number coming from Yahara that indicates where the significance lines should be annotated.
#' @param pretty_value_word A string to be used in the title indicating the target quantity.
#' @param my_colors Yahara-specific list of colors.
#'
#' @returns A ggplot object.
marginalHist = function(mv_data, y, universes, outcome, option, topRowScale, sigLinesIntercept, pretty_value_word, my_colors) {

  df_wide = mv_data[["WIDE_MV"]] %>% pivot_wider(names_from = "term", values_from = all_of(y), id_cols = c("universe", mv_data[["MULTIVERSE_OPTIONS"]]))

  # If all or none of the universes are selected, and there's no selected option, make it all gray
  if (mean(mv_data[["WIDE_MV"]]$universe %in% universes) %in% 0:1 & option == "none") {
    plot = ggplot(df_wide, aes(y = .data[[outcome]])) +
      geom_histogram(
        color = "gray30", fill = my_colors[["default"]],
        bins = 30, boundary = sigLinesIntercept, na.rm = T)

    # If there is a selected option, color by that
  } else if (option != "none") {
    plot = ggplot(data = NULL, aes(y = .data[[outcome]])) +
      geom_histogram(data = df_wide %>% filter(.data[[option]]),
                     color = "black", fill = my_colors[["selected"]],
                     bins = 30, boundary = sigLinesIntercept, na.rm = T) +
      geom_histogram(data = df_wide %>% filter(!.data[[option]]),
                     color = "black", fill = my_colors[["faded"]], alpha = 0.5,
                     bins = 30, boundary = sigLinesIntercept, na.rm = T)

    # Otherwise, color by partial brush selection
  } else {
    plot = ggplot(data = NULL, aes(y = .data[[outcome]])) +
      geom_histogram(data = df_wide %>% filter(universe %in% universes),
                     color = "black", fill = my_colors[["selected"]],
                     bins = 30, boundary = sigLinesIntercept, na.rm = T) +
      geom_histogram(data = df_wide %>% filter(!universe %in% universes),
                     color = "black", fill = my_colors[["faded"]], alpha = 0.5,
                     bins = 30, boundary = sigLinesIntercept, na.rm = T)

  }

  myTitle = paste0(pretty_value_word, " of:\n", str_force(outcome, max.length = 30))
  mySubtitle = ifelse(option != "none", paste0("Colored by ", option), ifelse(mean(mv_data[["WIDE_MV"]]$universe %in% universes) %in% 0:1, "Click the line plot or heat map to change this view.", "Colored by Custom Selection"))

  return(
    plot +
      scale_x_continuous(breaks = scales::breaks_pretty(),
                         expand = expansion(c(0, 0.1)),
                         limits = c(0, max(df_wide$universe))) +
      scale_y_continuous(limits = c(ifelse(pretty_value_word == "P-values", -0.05, topRowScale[["low"]] - topRowScale[["margin"]]), topRowScale[["high"]] + topRowScale[["margin"]])) +
      theme(
        axis.ticks = element_blank()
      ) +
      labs(x = "\nNumber of Universes", y = NULL, title = myTitle, subtitle = mySubtitle) +
      geom_hline(linetype = "dashed", color = my_colors[["sigLines"]], yintercept = c(-1, 1) * sigLinesIntercept, na.rm = T)
  )
}
