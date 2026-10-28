#' Create the parallel coordinates plot for the Yahara interface.
#'
#' @param mv_data Yahara-specific list of multiverse data.
#' @param y The name of the target quantity variable for the y axis.
#' @param universes A numeric vector of universes to highlight, e.g. through brushing or the highlight argument.
#' @param outcome A string indicating the term to highlight.
#' @param option A string indicating whether there is a selected option.
#' @param topRowScale A reactive() coming from Yahara that helps with the margins for the top row.
#' @param sigLinesIntercept A number that indicates where the confidence level lines should be annotated.
#' @param pretty_value_word A string to be used in the title indicating what target quantity is being graphed.
#' @param term_order A character vector indicating what order the terms should be plotted in.
#' @param my_colors A Yahara-specific list of colors.
#'
#' @returns A ggplot object.
pcPlot = function(mv_data, y, universes, outcome, option, topRowScale, sigLinesIntercept, pretty_value_word, term_order, my_colors) {

  df = mv_data[["WIDE_MV"]] %>%
    mutate(term = factor(term, levels = term_order))

  # Adding on missing universe-term combinations
  # Get all universe-term combinations
  combos = expand.grid(unique(df$universe), unique(df$term))
  colnames(combos) = c("universe", "term")
  # The universe-term combinations we need to add on:
  missing = anti_join(combos, df %>% distinct(universe, term), by = join_by(universe, term)) %>%
    mutate(estimate = NA, std.error = NA, statistic = NA, p.value = NA)
  # Let's merge on their options:
  missing_with_options = left_join(missing, df %>% select(universe, all_of(mv_data[["MULTIVERSE_OPTIONS"]])) %>% distinct(), by = join_by(universe)) %>%
    mutate(term = factor(term, levels = term_order))

  # Finally, add these rows onto df
  df = bind_rows(df, missing_with_options)

  na_percentages = df %>%
    group_by(term) %>%
    summarize(perc_na = mean(is.na(.data[[y]])),
              med = median(.data[[y]], na.rm = T))

  df = df %>%
    ungroup() %>%
    mutate(not_na = !is.na(.data[[y]])) %>%
    group_by(term) %>%
    mutate(!!y := case_when(
      is.na(.data[[y]]) ~ median(.data[[y]], na.rm = T),
      .default = .data[[y]]
    )) %>%
    arrange(term)

  # Default view: None or all universes are selected, and there's no option selected.
  if (mean(df$universe %in% universes) %in% 0:1 & option == "none") {
    plot = df %>%
      ggplot(aes(term, .data[[y]])) +
      geom_line(aes(group = universe), color = my_colors[["default"]], alpha = appropriateAlpha(max(df$universe)))

    # If there is a selected option, color by that.
  } else if (option != "none") {

    not_selected = df %>% filter(!.data[[option]])
    selected = df %>% filter(.data[[option]])
    plot = df %>%
      ggplot(aes(term, .data[[y]])) +
      geom_line(data = not_selected, aes(group = universe), color = my_colors[["faded"]], alpha = appropriateAlpha(length(unique(not_selected$universe)))) +
      geom_line(data = selected, aes(group = universe), color = my_colors[["selected"]], alpha = appropriateAlpha(length(unique(selected$universe))))

    # Otherwise, there's some selected universes, so color by those. There's never both selected universes and selected option.
  } else {

    not_selected = df %>% filter(!(universe %in% universes))
    selected = df %>% filter(universe %in% universes)
    plot = df %>%
      ggplot(aes(term, .data[[y]])) +
      geom_line(data = not_selected, aes(group = universe), color = my_colors[["faded"]], alpha = appropriateAlpha(length(unique(not_selected$universe)))) +
      geom_line(data = selected, aes(group = universe), color = my_colors[["selected"]], alpha = appropriateAlpha(length(unique(selected$universe))))
  }

  # Add the NA points
  plot = plot +
    geom_point(data = na_percentages, aes(y = med, size = perc_na), pch = 21, col = "black", bg = "white", na.rm = T) +
    scale_size_area()

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

  myTitle = paste0("All ", pretty_value_word, ": <span style = 'color:#EE2C2C'>Click</span> or <span style = 'color:#EE2C2C'>Brush</span> me!")
  mySubtitle = ifelse(option != "none", paste0("Colored by ", option), ifelse(mean(df$universe %in% universes) %in% 0:1, "", "Colored by Custom Selection"))
  boldSelected = rep("plain", length(term_order))
  boldSelected[which(term_order == outcome)] = "bold"

  return(
    plot +
      scale_alpha(range = c(0, 1)) +
      scale_x_discrete(
        labels = v_str_force(term_order, truncate = "middle")
      ) +
      scale_y_continuous(
        limits = c(ifelse(pretty_value_word == "P-values", -0.05, topRowScale[["low"]] - topRowScale[["margin"]]), topRowScale[["high"]] + topRowScale[["margin"]]),
        sec.axis = dup_axis(name = NULL,
                            labels = NULL,
                            guide = guide_axis(
                              theme = theme(
                                axis.ticks.y = element_line(color = "transparent")
                              )
                            ))
      ) +
      theme(legend.position = "none",
            axis.text.x = ggtext::element_markdown(angle = 90, hjust = 0.5, vjust = 0.5, face = boldSelected),
            axis.ticks.y = element_blank()
      ) +
      labs(x = NULL, y = pretty_value_word, title = myTitle, subtitle = mySubtitle) +
      geom_hline(linetype = "dashed", color = my_colors[["sigLines"]], yintercept = c(-1, 1) * sigLinesIntercept, na.rm = T)
  )
}
