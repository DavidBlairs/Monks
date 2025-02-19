box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, updateActionButton, observe, icon, reactive, uiOutput, fluidPage, column, fluidRow, textInput, actionButton, reactiveVal, observeEvent],
  shinyjs[runjs, useShinyjs]
)

box::use(
  app/view/marketing_contributions,
  app/view/waterfall_chart,
  app/view/roi_display,
  app/view/model_fit,
  app/logic/get_con[get_con]
)

box::use(
  app/logic/determine_options[determine_options]
)

con <- get_con(is_local=T)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    useShinyjs(),
    tags$div(
      class = "input_region",
      fluidRow(
        textInput(
          inputId = ns("chat_message_input"),
          label = NULL,
          width = "100%",
          placeholder = "Question"
        ),
        actionButton(
          inputId = ns("chat_message_confirm"),
          label = "Send",
          width = "100px",
          class = "chat-message-button"
        )
      )
    ),
    fluidRow(
      column(6,
        waterfall_chart$ui(ns("waterfall_chart"))
      ),
      column(6,
        marketing_contributions$ui(ns("marketing_contributions"))
      )
    ),
    fluidRow(
      column(6,
        roi_display$ui(ns("roi_display"))
      ),
      column(6,
        model_fit$ui(ns("model_fit"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    generated_filters <- reactiveVal(NULL)

    model_fit_options <- model_fit$server("model_fit", con, generated_filters)
    roi_display_options <- roi_display$server("roi_display", con, generated_filters)
    waterfall_chart_options <- waterfall_chart$server("waterfall_chart", con, generated_filters)
    marketing_contributions_options <- marketing_contributions$server("marketing_contributions", con, generated_filters)

    observeEvent(input$chat_message_confirm, {
      runjs(paste0("$('#", session$ns("chat_message_confirm"), "').html('Loading...');"))

      message <- input$chat_message_input
      generated_filters(determine_options(message))

      runjs(paste0("$('#", session$ns("chat_message_confirm"), "').html('Send');"))
    })

    observeEvent(generated_filters(), {
      print(generated_filters())
    })

    all_available_options <- reactive({
      list(
        model_fit = model_fit_options(),
        roi_display = roi_display_options(),
        waterfall_chart = waterfall_chart_options(),
        marketing_contributions = marketing_contributions_options()
      )
    })
  })
}
