#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import ggplot2
#' @import shiny
#' @importFrom data.table :=
#' @importFrom magrittr %>%
#' @importFrom stats median
#' @importFrom stats qnorm
#' @importFrom stats reorder
#' @importFrom stats sd
## usethis namespace: end

utils::globalVariables(c("name", "value", "universe", "term", "estimate", "std.error", "statistic",
                         "p.value", "decision", "selected", "perc", "perc_among_ALL", "perc_among_TRUE",
                         "response", "med", "perc_na", ".parameter_assignment", ".code", ".results", ".errors",
                         "s", "."))

ignore_unused_imports <- function() {
  utils::str
}

## mockable bindings: start
## mockable bindings: end
NULL
