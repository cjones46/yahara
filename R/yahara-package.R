#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr across all_of anti_join arrange bind_rows case_when desc distinct everything filter group_by join_by left_join mutate pull relocate rename row_number select summarize tibble ungroup
#' @importFrom ggplot2 aes annotate dup_axis element_blank element_line element_text expansion geom_histogram geom_hline geom_line geom_point geom_segment geom_tile get_guide_data ggplot ggplotGrob guide_axis labs scale_alpha scale_fill_viridis_c scale_size_area scale_x_continuous scale_x_discrete scale_y_continuous scale_y_discrete theme theme_bw theme_set unit
#' @importFrom shiny brushOpts brushedPoints column fluidPage fluidRow nearPoints observeEvent plotOutput radioButtons reactive reactiveVal renderPlot selectizeInput shinyApp updateSelectizeInput
#' @importFrom data.table :=
#' @importFrom magrittr %>%
#' @importFrom stats median qnorm reorder sd
## usethis namespace: end

utils::globalVariables(c("name", "value", "universe", "term", "estimate", "std.error", "statistic",
                         "p.value", "decision", "selected", "perc", "perc_among_ALL", "perc_among_TRUE",
                         "response", "med", "perc_na", ".parameter_assignment", ".code", ".results", ".errors",
                         "s", ".", ".data"))

ignore_unused_imports <- function() {
  utils::str
}

## mockable bindings: start
## mockable bindings: end
NULL
