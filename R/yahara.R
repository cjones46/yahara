#' Launch Yahara Interface
#'
#' @param M A multiverse object which has been executed using `multiverse::execute_multiverse()`.
#' @param fit_name The name of the object within the multiverse where `broom::tidy()` has been run upon the results of a model object.
#' @param highlight A call to `exprs()` containing a series of comma-separated logical expressions to
#' @param outcome_branch Works with `single_term`. If your multiple estimands come from a single predictor upon multiple outcomes, enter the name of the multiverse branch defining the outcomes here. Leave alone otherwise.
#' @param single_term Works with `outcome_branch`. If your multiple estimands come from a single predictor upon multiple outcomes, enter the name of the single predictor here. Leave alone otherwise.
#' @param conf.level A number in (0, 1) indicating the confidence level at which to annotate dashed lines. Default is 0.95.
#'
#' @return Launches a Shiny app.
#' @export
#'
#' @examples
#' #yahara(M, "fit_name")
yahara = function(M, fit_name, highlight = exprs(), outcome_branch = "", single_term = "", conf.level = 0.95) {

  theme_set(theme_bw() + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), plot.title = element_markdown(size = 20), plot.subtitle = element_text(size = 15), plot.caption = element_markdown(hjust = 0), axis.ticks.length = unit(0.25, "cm")))

  MY_COLORS = list(
    "selected" = "#EE2C2C",
    "default" = "gray40",
    "faded" = "gray80",
    "sigLines" = muted("blue")
  )

  LONG_MV = multiverse::expand(M) %>%
    multiverse::extract_variables(!!fit_name) %>%
    unnest( cols = c(!!fit_name) ) %>%
    rename("universe" = ".universe") %>%
    select(-c(.parameter_assignment, .code, .results, .errors))

  # everything comes from LONG_MV... this is where I should incorporate it.

  if (outcome_branch != "") {
    LONG_MV = LONG_MV %>%
      filter(term == single_term) %>%
      select(-term) %>%
      rename("term" = outcome_branch) %>%
      relocate(term, .before = "estimate")
  }

  WIDE_MV = LONG_MV %>%
    dummy_cols(select_columns = colnames(LONG_MV)[2:(ncol(LONG_MV)-5)],
               #remove_first_dummy = TRUE,
               remove_selected_columns = TRUE) %>%
    filter(term != "(Intercept)")

  TERMS = WIDE_MV %>% distinct(term) %>% pull(term)

  MULTIVERSE_OPTIONS = colnames(WIDE_MV)[7:ncol(WIDE_MV)] # is 7 always the correct number?

  REGRESSION_MV = LONG_MV %>%
    dummy_cols(select_columns = colnames(LONG_MV)[2:(ncol(LONG_MV)-5)],
               remove_first_dummy = TRUE,
               remove_selected_columns = TRUE) %>%
    filter(term != "(Intercept)")

  REGRESSION_OPTIONS = colnames(REGRESSION_MV)[7:ncol(REGRESSION_MV)]

  if (outcome_branch != "") {

    long_universe_specifications = LONG_MV %>%
      distinct(across(all_of(colnames(LONG_MV)[2:(length(colnames(LONG_MV))-5)]))) %>%
      mutate(universe = row_number())

    LONG_MV = LONG_MV %>%
      select(-universe) %>%
      left_join(long_universe_specifications,
                by = colnames(long_universe_specifications %>% select(-universe)))

    wide_universe_specifications = WIDE_MV %>%
      distinct(across(all_of(MULTIVERSE_OPTIONS))) %>%
      mutate(universe = row_number())

    WIDE_MV = WIDE_MV %>%
      select(-universe) %>%
      left_join(wide_universe_specifications,
                by = colnames(wide_universe_specifications %>% select(-universe)))

    regression_universe_specifications = REGRESSION_MV %>%
      distinct(across(all_of(REGRESSION_OPTIONS))) %>%
      mutate(universe = row_number())

    REGRESSION_MV = REGRESSION_MV %>%
      select(-universe) %>%
      left_join(regression_universe_specifications,
                by = colnames(regression_universe_specifications %>% select(-universe)))

  }

  # Change 0/1 dummy_cols columns to T/F for easy filtering
  WIDE_MV = WIDE_MV %>%
    mutate(across(all_of(MULTIVERSE_OPTIONS), ~ as.logical(.x)))
  REGRESSION_MV = REGRESSION_MV %>%
    mutate(across(all_of(REGRESSION_OPTIONS), ~ as.logical(.x)))

  MV_DATA = list(
    LONG_MV = LONG_MV,
    WIDE_MV = WIDE_MV,
    TERMS = TERMS,
    REGRESSION_MV = REGRESSION_MV,
    MULTIVERSE_OPTIONS = MULTIVERSE_OPTIONS,
    REGRESSION_OPTIONS = REGRESSION_OPTIONS
  )

  # these can stay!
  INITIAL_TERM_ORDER = WIDE_MV %>% group_by(term) %>% summarize(s = sd(statistic, na.rm = T)) %>% arrange(desc(s)) %>% pull(term)
  TERM_SELECT_BOX_LABEL = "Focus on a term by clicking on the line plot, or by selecting it here."
  OPTION_ORDER = getOptionOrder(MV_DATA)

  ui = fluidPage(
    fluidRow(
      column(width = 3,
             radioButtons("valueTypeSelect", "Which target quantity to Use?",
                          c("Test Statistics", "P-values"),
                          selected = "Test Statistics")
      ),
      column(width = 3,
             selectizeInput("termSelect", TERM_SELECT_BOX_LABEL, INITIAL_TERM_ORDER, INITIAL_TERM_ORDER[1]),
      )
    ),
    fluidRow(
      column(width = 7, plotOutput("pcplot", brush = brushOpts("pcplotBrush", resetOnNew = TRUE), click = "pcplotClick"), style = "padding: 0;"),
      column(width = 5, plotOutput("marginalHist"), style = "padding: 0;")
    ),
    fluidRow(
      column(width = 7, plotOutput("heatmap", click = "heatmapClick"), style = "padding: 0;"),
      column(width = 5, plotOutput("marginalDecisions"), style = "padding: 0;")
    )


  )

  server = function(input, output, session) {

    pcplot_ggplot = reactive(pcPlot(MV_DATA, target_quantity(), selected_universes(), selected_term(), selected_option(), topRowScale(), sigLinesIntercept(), input$valueTypeSelect, term_order(), MY_COLORS))
    marginalHist_ggplot = reactive(marginalHist(MV_DATA, target_quantity(), selected_universes(), selected_term(), selected_option(), topRowScale(), sigLinesIntercept(), input$valueTypeSelect, MY_COLORS))
    heatMap_ggplot = reactive(myHeatMap(heatMapCoefs(), selected_term(), selected_option(), OPTION_ORDER, input$optionOrderSelect, term_order(), input$valueTypeSelect, get_guide_data(pcplot_ggplot(), "y")$.label[which.max(nchar(get_guide_data(pcplot_ggplot(), "y")$.label))], MY_COLORS))
    lollipop_ggplot = reactive(marginalDecisions(MV_DATA, selected_universes(), selected_option(), OPTION_ORDER, input$optionOrderSelect, MY_COLORS))

    right_column_grobs = reactive({
      pcplot_grob = ggplotGrob(pcplot_ggplot())
      marginalHist_grob = ggplotGrob(marginalHist_ggplot())
      heatMap_grob = ggplotGrob(heatMap_ggplot())
      lollipop_grob = ggplotGrob(lollipop_ggplot())

      # Align the marginalHist heights to the pcplot heights.
      marginalHist_grob$heights = pcplot_grob$heights

      # Move the X axis title up
      initial_title_row = marginalHist_grob$layout[which(marginalHist_grob$layout$name == "xlab-b"),]$t
      new_title_row = marginalHist_grob$layout[which(marginalHist_grob$layout$name == "axis-b"),]$t
      marginalHist_grob$layout[which(marginalHist_grob$layout$name == "xlab-b"), c("t", "b")] = rep(new_title_row, 2)

      # Align the lollipop heights to the heat map heights.
      lollipop_grob$heights = heatMap_grob$heights
      # Move the caption up.
      initial_cap_row = lollipop_grob$layout[which(lollipop_grob$layout$name == "caption"),]$t
      new_cap_row = lollipop_grob$layout[which(lollipop_grob$layout$name == "axis-b"),]$t + 2
      lollipop_grob$layout[which(lollipop_grob$layout$name == "caption"), c("t", "b")] = rep(new_cap_row, 2)
      # Switch the heights
      cap_row_height = lollipop_grob$heights[initial_cap_row]
      lollipop_grob$heights[initial_cap_row] = lollipop_grob$heights[new_cap_row]
      lollipop_grob$heights[new_cap_row] = cap_row_height

      return(list(marginalHist_grob, lollipop_grob))
    })

    # Define plot outputs via renderPlot() via helpers
    output$pcplot = renderPlot(pcplot_ggplot())
    output$marginalHist = renderPlot(grid.draw(right_column_grobs()[[1]]))
    output$heatmap = renderPlot(heatMap_ggplot())
    output$marginalDecisions = renderPlot(grid.draw(right_column_grobs()[[2]]))


    # Define Reactive Values

    selected_universes = reactiveVal(1:nrow(WIDE_MV)) # By default, all universes are selected.
    if (nrow(LONG_MV %>% filter(!!!highlight) < nrow(LONG_MV))) {
      selected_universes(LONG_MV %>% filter(!!!highlight) %>% pluck("universe"))
    }
    selected_option = reactiveVal("none") # The tool can exist without a selected option; we'll use "none" for this.

    target_quantity = reactive(getValueVariable(input$valueTypeSelect))

    term_order = reactive(WIDE_MV %>%
                            group_by(term) %>%
                            summarize(s = sd(.data[[target_quantity()]], na.rm = T)) %>%
                            arrange(desc(s)) %>%
                            pull(term)
    )
    # statistic is the default selected value
    selected_term = reactiveVal(WIDE_MV %>% group_by(term) %>% summarize(s = sd(statistic, na.rm = T)) %>% arrange(desc(s)) %>% pull(term) %>% .[1])

    topRowScale = reactive({
      range = summarize(WIDE_MV, low = min(.data[[target_quantity()]]),
                        high = max(.data[[target_quantity()]]))
      width = range$high-range$low
      return(list("low" = range$low, "high" = range$high, "margin" = 0.1*width))
    })
    sigLinesIntercept = reactive({
      ifelse(input$valueTypeSelect == "Test Statistics",
             qnorm(1 - (1-conf.level)/2),
             ifelse(input$valueTypeSelect == "P-values",
                    1-conf.level,
                    NA))
    })
    heatMapCoefs = reactive({

      df_wide = REGRESSION_MV %>% pivot_wider(names_from = "term", values_from = target_quantity(), id_cols = c("universe", all_of(REGRESSION_OPTIONS)))
      x = df_wide[,REGRESSION_OPTIONS] %>% as.matrix()
      y = df_wide[,TERMS] %>% as.matrix()

      # Impute NAs with the median of the column
      y = y %>% as.data.frame() %>% mutate(row = row_number()) %>% pivot_longer(-row) %>% group_by(name) %>% mutate(value = case_when(is.na(value) ~ median(value, na.rm = T), .default = value)) %>% pivot_wider(id_cols = row) %>% select(-row) %>% as.matrix()


      # Use lambda.min from cv.glmnet
      fit = glmnet(x, y, family = "mgaussian", lambda = cv.glmnet(x, y, family = "mgaussian")$lambda.min)

      coefs = tibble(response = character(0), options = character(0), value = numeric(0))
      for (i in 1:length(fit$beta)) {
        coefs = bind_rows(coefs,
                          tibble(
                            response = TERMS[i],
                            options = fit$beta[[i]]@Dimnames[[1]],
                            value = heatMapExtractionHelper(fit$beta[[i]]@i + 1, fit$beta[[i]]@x, MV_DATA) #+1 needed since the "i" index starts at 0 for some reason
                          ))
      }
      coefs = coefs %>% bind_rows(
        expand.grid(response = TERMS, options = setdiff(MULTIVERSE_OPTIONS, REGRESSION_OPTIONS)) %>% mutate(value = NA)
      ) %>%
        mutate(response = factor(response, levels = term_order()))
      return(coefs)
    })

    # Observe Events

    # When PCPlot is Brushed:
    # * Update selected universes according to the brush
    # * Update selected outcome
    # * Update termSelect box
    # * Reset selected option to nothing

    observeEvent(
      input$pcplotBrush,
      {
        bp = brushedPoints(
          WIDE_MV %>% mutate(term = factor(term, term_order())),
          input$pcplotBrush,
          yvar = getValueVariable(input$valueTypeSelect)
        )
        # Step 1: Update selected universes
        selected_universes(
          bp$universe
        )
        # Step 2: Update selected outcome
        if (nrow(bp) == 0) {
          # do nothing
        } else {
          selected_term(bp[1, "term"] %>% pluck("term") %>% as.character())
        }

        # Step 3: Update termSelect
        updateSelectizeInput(
          session,
          "termSelect",
          TERM_SELECT_BOX_LABEL, term_order(), selected_term()
        )

        # Step 4: Reset selected option to nothing
        selected_option("none")
      }
    )

    # When pc plot is clicked:
    # * Update selected outcome (the COLUMN that was clicked)
    # * Update the selectizeInput
    # * Reset selected option to "none"

    observeEvent(
      input$pcplotClick,
      {

        val = input$pcplotClick$x %>% round()
        val = max(1, val)
        val = min(length(term_order()), val)
        selected_term(term_order()[val])

        # Step 2: update termSelect box
        updateSelectizeInput(
          session,
          "termSelect",
          TERM_SELECT_BOX_LABEL, term_order(), selected_term()
        )

        # Step 3: Update selected option
        selected_option("none")

        # Step 4: Reset selected universes to all
        # selected_universes(1:nrow(mv_data[["WIDE_MV"]]))
      }
    )
    # When heat map is clicked:
    # * Update selected outcome (the COLUMN that was clicked)
    # * Update termSelect box
    # * Update selected option (the ROW that was clicked)
    # * Reset selected universes to all

    observeEvent(
      input$heatmapClick,
      {
        # Get the clicked spot
        clickedSpot = nearPoints(
          heatMapCoefs(),
          input$heatmapClick,
          xvar = "response",
          yvar = "options",
          maxpoints = 1, # Give me just the closest row
          threshold = 1000, # Large enough to definitely capture one whole tile
        )

        # Step 1: Update selected outcome
        selected_term(clickedSpot %>% pluck("response") %>% as.character())

        # Step 2: Update termSelect
        updateSelectizeInput(
          session,
          "termSelect",
          TERM_SELECT_BOX_LABEL, term_order(), selected_term()
        )

        # Step 3: Update selected option
        selected_option(clickedSpot %>% pluck("options"))

        # Step 4: Reset selected universes to all
        selected_universes(1:nrow(WIDE_MV))
      }
    )
    # selected_term is not a reactive() because it needs to update on click. Therefore, we need to tell it manually to update with the select input.
    observeEvent(
      input$termSelect,
      selected_term(input$termSelect)
    )

    # When the
    observeEvent(
      input$valueTypeSelect,
      updateSelectizeInput(
        session,
        "termSelect",
        TERM_SELECT_BOX_LABEL, term_order(), selected_term()
      )
    )
  }

  shinyApp(ui, server)
}
