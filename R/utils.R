#' Internal helper that forces a string to have certain pretty characteristics for printing.
#'
#' @param x The string to edit.
#' @param max.length The length of the string that will be output.
#' @param truncate One of `"beginning"`, `"middle"`, or `"end"`, indicating where a too-long string should be cut off.
#' @param pad One of `"beginning"` or `"end"` indicating where a too-short string should be padded.
#'
#' @returns A string with the desired characteristics.
str_force = function(x, max.length = 10, truncate = "end", pad = "end") {
  if (str_length(x) > max.length) {

    if (truncate == "end") {
      return(paste0(str_sub(x, 1, max.length), "..."))
    } else if (truncate == "beginning") {
      return(paste0("...", str_sub(x, str_length(x)-max.length+1, str_length(x))))
    } else if (truncate == "middle") {
      beginning_margin = floor(0.5*(max.length-3))
      end_margin = ceiling(0.5*(max.length-3))
      return(paste0(str_sub(x, 1, beginning_margin), "...", str_sub(x, str_length(x) - end_margin + 1, str_length(x))))
    }

  } else {

    if (pad == "end") {
      return(paste0(x, str_c(rep("&nbsp;", max.length - str_length(x)), collapse = "")))
    } else if (pad == "beginning") {
      return(paste0(str_c(rep("&nbsp;", max.length - str_length(x)), collapse = ""), x))
    }

  }
}

#' Vectorized version of `str_force`.
#'
#' @name v_str_force
#' @inheritParams str_force
v_str_force = Vectorize(str_force, "x", USE.NAMES = F)


# Helper function that assists heat map coefficient extraction
#' Internal helper that assists with heat map coefficient extraction in `heatMapCoefs()`.
#'
#' @param i The index of the coefficient to be extracted.
#' @param x The value of the coefficient to be extracted.
#' @param mv_data Yahara-specific list of multiverse related data.
#'
#' @returns A vector of coefficients.
heatMapExtractionHelper = function(i, x, mv_data) {
  col = numeric(length(mv_data[["REGRESSION_OPTIONS"]]))
  col[i] = x
  return(col)
}


#' Internal helper function to convert the radio buttons to the appropriate variable name.
#'
#' @param type The value of the radio buttons, "Test Statistics" or "P-values".
#'
#' @returns A string variable name, "statistic" or "p.value".
getValueVariable = function(type) {
  if (type == "Test Statistics") {
    return("statistic")
  } else if (type == "P-values") {
    return("p.value")
  }
}

#' Internal helper function that returns the option order for this set of multiverse data.
#'
#' @param mv_data Yahara-specific list of multiverse related data.
#'
#' @returns A vector of sorted options.
getOptionOrder = function(mv_data) {

  df_wide = mv_data[["REGRESSION_MV"]] %>% pivot_wider(names_from = "term", values_from = "statistic", id_cols = c("universe", all_of(mv_data[["REGRESSION_OPTIONS"]])))
  x = df_wide[,mv_data[["REGRESSION_OPTIONS"]]] %>% as.matrix()
  y = df_wide[,mv_data[["TERMS"]]] %>% as.matrix()

  # Impute NAs with the median of the column
  y = y %>% as.data.frame() %>% mutate(row = row_number()) %>% pivot_longer(-row) %>% group_by(name) %>% mutate(value = case_when(is.na(value) ~ median(value, na.rm = T), .default = value)) %>% pivot_wider(id_cols = row) %>% select(-row) %>% as.matrix()


  # Use lambda.min from cv.glmnet
  fit = glmnet(x, y, family = "mgaussian", lambda = cv.glmnet(x, y, family = "mgaussian")$lambda.min)

  coefs = tibble(response = character(0), options = character(0), value = numeric(0))
  for (i in 1:length(fit$beta)) {
    coefs = bind_rows(coefs,
                      tibble(
                        response = mv_data[["TERMS"]][i],
                        options = fit$beta[[i]]@Dimnames[[1]],
                        value = heatMapExtractionHelper(fit$beta[[i]]@i + 1, fit$beta[[i]]@x, mv_data) #+1 needed since the "i" index starts at 0 for some reason
                      ))
  }
  coefs = coefs %>% bind_rows(
    expand.grid(response = mv_data[["TERMS"]], options = setdiff(mv_data[["MULTIVERSE_OPTIONS"]], mv_data[["REGRESSION_OPTIONS"]])) %>% mutate(value = NA)
  )

  decisions = mv_data[["LONG_MV"]] %>% select(-c(universe, term, estimate, std.error, statistic, p.value)) %>% colnames()
  # Order first by decision, with the order determined by the maximum magnitude coefficient therein
  # Order second by coefficient magnitude
  coefs %>%
    replace_na(list(value = 0)) %>%
    mutate(
      decision = str_match(options, str_c("^", decisions, collapse = "|"))[,1],
      decision = reorder(decision, value, function(x) max(abs(x), na.rm = T))
    ) %>%
    group_by(decision, options) %>%
    summarize(within_option_max = max(abs(value), na.rm = T), .groups = "drop_last") %>%
    pull(options) %>%
    return()
}

#' Internal helper function to compute the appropriate alpha for the parallel coordinates plot given the number of universes.
#'
#' @param n_universes Number of universes.
#'
#' @returns An alpha value between 0.1 and 1.
appropriateAlpha = function(n_universes) {
  return(max(0.1, 1 - n_universes * 0.9/100))
}
