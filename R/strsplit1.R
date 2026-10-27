#' Split a string
#'
#' @param x This is the description of what x is
#' @param split This is the description of what split is
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x = "alfa,bravo,charlie,delta"
#' strsplit1(x, split = ",")
strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}
