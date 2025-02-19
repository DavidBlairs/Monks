box::use(
  shiny[tags, NS, moduleServer, uiOutput, renderUI, actionButton, outputOptions, renderPlot, reactive,  updateDateRangeInput, updateSelectInput, observe, observeEvent, req, plotOutput, selectInput, dateRangeInput, tagList],
  dplyr[tbl, rename, bind_rows, collect, pull, distinct, select, arrange, left_join, mutate],
  DT[DTOutput, renderDT],
  shinyWidgets[dropMenu],
  lubridate[parse_date_time, days_in_month, mdy]
)

box::use(
  app/logic/get_roi_display[get_roi_display]
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
      DTOutput(ns("display_view_plot"), width = "100%")
    )
  )
}


#' @export
server <- function(id, con, generated_filters) {
  moduleServer(id, function(input, output, session) {
    available_options <- reactive({
        roi <- tbl(con, "ROI")
        spend_data <- tbl(con, "SpendData")
 
        roi <- roi |>
             rename(DateGroup = date_group) |>
             select(-model_name) |>
            left_join(spend_data, by = c("SpendVariable", "DateGroup")) 
        
        roi_date <- roi |> 
            collect() |>
            mutate(DateGroup_parsed = parse_date_time(DateGroup, orders = "my")) |>
            arrange(DateGroup_parsed) |>
            select(-DateGroup_parsed) |>
            pull(DateGroup) |>
            unique() 

        date_range <- c(roi_date[1], roi_date[length(roi_date)])

        start_date <- as.Date(paste0("01 ", date_range[1]), format = "%d %B %Y")
        end_date <- as.Date(paste0(days_in_month(mdy(paste0("01 ", date_range[2]))), " ", date_range[2]), format =  "%d %B %Y")

        date_range <- c(start_date, end_date)

        spend_variable <- roi |> 
            select(SpendVariable) |>
            distinct() |>
            pull("SpendVariable")

        list(
            date_range = date_range,
            spend_variable = spend_variable
        )
    })
    output$control_panel_ui <- renderUI({
        plot_options <- available_options()
        tagList(
            tags$div(
                class = "spend_variable_input",
                selectInput(
                    inputId = session$ns("spend_variable"),
                    label = "Enter Spend Variable(s)",
                    choices = plot_options$spend_variable,
                    selected = plot_options$spend_variable,
                    multiple = TRUE
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

    observeEvent(generated_filters(), {
      plot_options <- available_options()
      if (!is.null(generated_filters()$spend_variable)) {
        updateSelectInput(
          session = session, 
          inputId = "spend_variable",
          label = "Enter Spend Variable(s)",
          choices = plot_options$spend_variable,
          selected = generated_filters()$spend_variable
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

    selected_options <- reactive({
      list(
        date_range = input$date_range,
        spend_variable = input$spend_variable
      )
    })

    output$display_view_plot <- renderDT({
        req(!is.null(selected_options()$spend_variable))
        get_roi_display(con, selected_options())
    })

    return(available_options)
  })
}