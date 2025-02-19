box::use(
  shiny[tags, NS, moduleServer, uiOutput, renderUI, renderPlot, outputOptions, reactive, icon, actionButton, updateDateRangeInput, updateSelectInput, observe, observeEvent, req, plotOutput, reactiveVal, selectInput, dateRangeInput, tagList],
  dplyr[tbl, rename, bind_rows, collect, pull, distinct, select, arrange],
  shinyjs[toggleClass],
  shinyWidgets[dropMenu, animateOptions, animations]
)

box::use(
  app/logic/plot_model_fit[plot_model_fit]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "dashboard-container",

    # Main Chart / Table Display with Floating Filters Button
    tags$div(
      class = "display_view",

      # Floating DropMenu Filter Button
      dropMenu(
        tag = actionButton(ns("toggle_filters"), "Filters", class = "filter-button"),
        placement = "bottom-start",
        theme = "light-border",
        arrow = FALSE,
        maxWidth = "300px",

        # UI Filters Inside the Dropdown
        uiOutput(ns("control_panel_ui"))
      ),

      # Plot Output
      plotOutput(ns("display_view_plot"), width = "100%")
    )
  )
}

#' @export
server <- function(id, con, generated_filters) {
  moduleServer(id, function(input, output, session) {
    available_options <- reactive({
        actuals <- tbl(con, "ActualFittedResiduals")

        dates <- actuals |> 
            arrange(date) |>
            pull("date") |>
            unique()
        date_range <- c(dates[1], dates[length(dates)])

        model_name <- actuals |>
            select(model_name) |>
            distinct() |>
            pull(model_name)

        list(
            model_name = model_name,
            date_range = date_range
        )
    })

    output$control_panel_ui <- renderUI({
        plot_options <- available_options()
        tagList(
            tags$div(
                class = "model_name_input",
                selectInput(
                    inputId = session$ns("model_name"),
                    label = "Enter Model Name",
                    choices = plot_options$model_name,
                    selected = plot_options$model_name[1],
                    multiple = FALSE
                )
            ),
            tags$div(
                class = "date_range_input",
                dateRangeInput(
                    inputId = session$ns("date_range"),
                    label = "Enter Date Range",
                    start = plot_options$date_range[1],
                    end = plot_options$date_range[length(plot_options$date_range)],
                    min = plot_options$date_range[1],
                    max = plot_options$date_range[length(plot_options$date_range)],
                )
            )
        )
    })
    outputOptions(output, "control_panel_ui", suspendWhenHidden = FALSE)

    selected_options <- reactive({
      list(
        model_name = input$model_name,
        date_range = input$date_range
      )
    })

    observeEvent(generated_filters(), {
      plot_options <- available_options()
      if (!is.null(generated_filters()$model_name)) {
        updateSelectInput(
          session = session, 
          inputId = "model_name",
          label = "Enter Model Name",
          choices = plot_options$model_name,
          selected = generated_filters()$model_name
        )
      }
      if (!is.null(generated_filters()$date_range)) {
        chosen_date_range <- generated_filters()$date_range
        if (generated_filters()$date_range[1] < plot_options$date_range[1]) {
          chosen_date_range[1] <- plot_options$date_range[1]
        }
        if (generated_filters()$date_range[2] > plot_options$date_range[2]) {
          chosen_date_range[2] <- plot_options$date_range[2]
        }

        updateDateRangeInput(
          session = session,
          inputId = "date_range",
          label = "Enter Date Range",
          start = chosen_date_range[1],
          end = chosen_date_range[2],
          min = plot_options$date_range[1],
          max = plot_options$date_range[length(plot_options$date_range)]
        )
      }
    })

    output$display_view_plot <- renderPlot({
        req(!is.null(selected_options()$model_name))
        plot_model_fit(con, selected_options())
    })

    return(available_options)
  })
}