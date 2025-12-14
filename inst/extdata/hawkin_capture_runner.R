#' Hawkin Capture-style runner
#'
#' Source this file then call `hawkin_capture_pipeline()` to pull Hawkin Dynamics
#' data and cache it locally for rapid visualization. This script keeps external
#' dependencies intentionally light so you can run it on a scheduler or jump
#' straight into Shiny prototyping.
#'
#' Required environment variables:
#' * `HAWKIN_REFRESH_TOKEN`: your Hawkin Dynamics refresh token.
#' * `HAWKIN_REGION`: API region (Americas, Europe, Asia/Pacific, Dev). Defaults to "Americas".
#' * `HAWKIN_ORG_NAME`: optional org endpoint when provided by Hawkin support.
#'
#' Typical usage:
#' ```r
#' source(system.file("extdata", "hawkin_capture_runner.R", package = "hawkinR"))
#' cache <- hawkin_capture_pipeline(days = 14, max_forcetime = 25)
#' preview_hawkin_capture(cache)
#' ```

# nocov start

hawkin_capture_pipeline <- function(days = 7,
                                    max_forcetime = 15,
                                    output_dir = "hawkin_capture_cache",
                                    tz = Sys.timezone()) {
  if (!requireNamespace("hawkinR", quietly = TRUE)) {
    stop("hawkinR must be installed to run the capture pipeline.")
  }

  refresh_token <- Sys.getenv("HAWKIN_REFRESH_TOKEN")
  region <- Sys.getenv("HAWKIN_REGION", unset = "Americas")
  org_name <- Sys.getenv("HAWKIN_ORG_NAME", unset = "")

  if (!nzchar(refresh_token)) {
    stop("Set HAWKIN_REFRESH_TOKEN in your environment before running.")
  }

  access <- hawkinR::get_access(
    refreshToken = refresh_token,
    region = region,
    org_name = if (nzchar(org_name)) org_name else NULL
  )

  now <- as.numeric(Sys.time())
  window_start <- now - days * 24 * 60 * 60

  message("Fetching tests from the last ", days, " days (region: ", region, ")...")
  tests <- hawkinR::get_tests(from = window_start, to = now)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  saveRDS(tests, file.path(output_dir, "tests.rds"))
  message("Saved ", nrow(tests), " tests to ", file.path(output_dir, "tests.rds"))

  # Pull force-time traces for a subset to control runtime
  forcetime_ids <- head(tests$id, max_forcetime)
  force_curves <- lapply(forcetime_ids, function(id) {
    message("Downloading force-time for test ", id, "...")
    hawkinR::get_forcetime(testId = id)
  })
  names(force_curves) <- forcetime_ids

  saveRDS(force_curves, file.path(output_dir, "forcetime.rds"))
  message("Saved ", length(force_curves), " force-time traces to ", file.path(output_dir, "forcetime.rds"))

  list(
    access = access,
    tests = tests,
    forcetime = force_curves,
    tz = tz,
    output_dir = normalizePath(output_dir)
  )
}

preview_hawkin_capture <- function(cache) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Install the 'shiny' package to preview the dashboard.")
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Install the 'plotly' package to preview the force-time explorer.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Install the 'dplyr' package to aggregate test data.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Install the 'tidyr' package to tidy test data.")
  }

  tests <- cache$tests
  forcetime <- cache$forcetime

  ui <- shiny::fluidPage(
    shiny::titlePanel("Hawkin Capture Preview"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          "test_id",
          "Test",
          choices = tests$id,
          selected = tests$id[1]
        ),
        shiny::helpText("Data cached at:", cache$output_dir)
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Summary",
            shiny::tableOutput("summary_table")
          ),
          shiny::tabPanel(
            "Force-Time",
            plotly::plotlyOutput("ft_plot")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    output$summary_table <- shiny::renderTable({
      tests |>
        dplyr::mutate(startTime = as.POSIXct(startTime, origin = "1970-01-01", tz = cache$tz)) |>
        dplyr::arrange(dplyr::desc(startTime)) |>
        dplyr::select(id, athleteName, testTypeName, startTime, testDuration)
    })

    output$ft_plot <- plotly::renderPlotly({
      ft <- forcetime[[input$test_id]]
      validate <- function(x) if (is.null(x) || NROW(x) == 0) NULL else x
      ft <- validate(ft)
      if (is.null(ft)) {
        return(plotly::plot_ly() |> plotly::layout(title = "No force-time data available"))
      }
      ft |>
        tidyr::unnest(data) |>
        plotly::plot_ly(x = ~time, y = ~force) |>
        plotly::add_lines(name = "Force") |>
        plotly::layout(
          title = paste("Force-Time | Test", input$test_id),
          xaxis = list(title = "Time (s)"),
          yaxis = list(title = "Force (N)")
        )
    })
  }

  shiny::shinyApp(ui, server)
}

# nocov end
