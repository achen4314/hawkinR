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

  tz <- if (is.na(tz) || !nzchar(tz)) "UTC" else tz
  env <- .validate_hawkin_env()

  access <- hawkinR::get_access(
    refreshToken = env$refresh_token,
    region = env$region,
    org_name = if (nzchar(env$org_name)) env$org_name else NULL
  )

  now <- as.numeric(Sys.time())
  window_start <- now - days * 24 * 60 * 60

  message(
    "Fetching tests from the last ", days, " days (region: ", env$region,
    if (nzchar(env$org_name)) paste0(", org: ", env$org_name) else "",
    ")..."
  )
  tests <- hawkinR::get_tests(from = window_start, to = now)

  if (is.null(tests) || nrow(tests) == 0) {
    warning("No tests returned for the requested window; nothing cached.")
    return(list(
      access = access,
      tests = tests,
      forcetime = list(),
      tz = tz,
      output_dir = normalizePath(output_dir)
    ))
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  tests$startTime <- as.POSIXct(tests$startTime, origin = "1970-01-01", tz = tz)
  saveRDS(tests, file.path(output_dir, "tests.rds"))
  message("Saved ", nrow(tests), " tests to ", file.path(output_dir, "tests.rds"))

  forcetime_ids <- head(tests$id, max_forcetime)
  if (length(forcetime_ids) == 0) {
    warning("No force-time IDs available to download.")
    force_curves <- list()
  } else {
    force_curves <- lapply(forcetime_ids, function(id) {
      message("Downloading force-time for test ", id, "...")
      tryCatch(
        hawkinR::get_forcetime(testId = id),
        error = function(e) {
          warning(sprintf("Force-time download failed for %s: %s", id, e$message))
          NULL
        }
      )
    })
    names(force_curves) <- forcetime_ids
  }

  saveRDS(force_curves, file.path(output_dir, "forcetime.rds"))
  message("Saved ", length(Filter(Negate(is.null), force_curves)), " force-time traces to ",
          " ", file.path(output_dir, "forcetime.rds"))

  list(
    access = access,
    tests = tests,
    forcetime = force_curves,
    tz = tz,
    output_dir = normalizePath(output_dir)
  )
}

.validate_hawkin_env <- function() {
  refresh_token <- Sys.getenv("HAWKIN_REFRESH_TOKEN")
  region <- Sys.getenv("HAWKIN_REGION", unset = "Americas")
  org_name <- Sys.getenv("HAWKIN_ORG_NAME", unset = "")

  if (!nzchar(refresh_token)) {
    stop("Set HAWKIN_REFRESH_TOKEN in your environment before running.")
  }

  valid_regions <- c("Americas", "Europe", "Asia/Pacific", "Dev")
  if (!region %in% valid_regions) {
    warning(
      "HAWKIN_REGION not recognized (", region, "); defaulting to 'Americas'.",
      " Valid options: ", paste(valid_regions, collapse = ", ")
    )
    region <- "Americas"
  }

  list(
    refresh_token = refresh_token,
    region = region,
    org_name = org_name
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
