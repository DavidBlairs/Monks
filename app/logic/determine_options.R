box::use(
  ellmer[type_object, chat_openai, type_boolean, type_string, type_enum, type_array, token_usage]
)

determine_options <- function(chat_message) {
  chat <- chat_openai(model = "gpt-4o-mini")

  type_filters <- type_object(
    "Which filters will be applicable to the users query.",
    model_name = type_boolean("Whether the user specifies a specific model (from Brand, Generic or Sales)."),
    date_range = type_boolean("Whether the user specifies a range of dates."),
    spend_variable = type_boolean("Whether the user specific any type of media spending categories."),
    categories = type_boolean("Whether the user specifies any type of categories that might effect sales or our KPIs.")
  )

  filters_to_apply <- chat$extract_data(chat_message, type = type_filters)
  type_model_name <- type_object(
    "Which model to use.",
    model_name = type_enum("The name of the model", c("Brand", "Generic", "Sales"))
  )

  type_date_range <- type_object(
    "Range of dates to use, inclusive of the start and end periods indicated.",
    start_date = type_string("The start date indicated, inclusive of the period described. This MUST be in the YYYY-MM-DD format."),
    end_date = type_string("The end date indicated, inclusive of the period described. This MUST be in the YYY-MM-DD format.")
  )

  spend_variable <- c(
        "Media - Brand-Total-Spend",
        "Media - BRTV-Total-Spend",
        "Media - Direct Mail-Total-Spend",
        "Media - Display Online-Total-Spend",
        "Media - Generic-Total-Spend",
        "Media - Inserts-Total-Spend",
        "Media - Press Radio & Doordrops-Total-Spend",
        "Media - Retargeting-Criteo-Spend",
        "Media - Social-Facebook-Spend",
        "Media - TV-Total-Spend"
      )

  categories <- c(
        "Competitors", "Display Online", "TV", "Press Radio & Doordrops",
        "Generic", "Price", "Brand", "Economy", "Retargeting", "Base",
        "Social", "BRTV", "Climate", "Event", "Covid", "Promo",
        "LT - TV", "Direct Mail", "Inserts", "Seasonality", "Events"
      )

  type_spend_variable <- type_object(
    "A unique list of inferred spend variables relating to the users query.",
    spend_variables = type_array(items = type_enum("The spend variable mentioned or implied.", spend_variable))
  )

  type_categories <- type_object(
    "A unique list of inferred influence categories to our KPIs.",
    categories = type_array(items = type_enum("The KPI influence categories mentioned or implied.", categories))
  )

  output_list <- list(
    categories = NULL,
    spend_variable = NULL,
    date_range = NULL,
    model_name = NULL
  )

  if (filters_to_apply$categories) {
    output_list$categories <- chat$extract_data(chat_message, type = type_categories)$categories |>
      as.vector()
  }
  if (filters_to_apply$spend_variable) {
    output_list$spend_variable <- chat$extract_data(chat_message, type = type_spend_variable)$spend_variables |>
      as.vector()
  }
  if (filters_to_apply$date_range) {
    date_range <- chat$extract_data(chat_message, type = type_date_range) |>
      unlist() |>
      as.vector()
    output_list$date_range <- as.Date(date_range, format = "%Y-%m-%d")
  }
  if (filters_to_apply$model_name) {
    output_list$model_name <- chat$extract_data(chat_message, type = type_model_name)$model_name
  }
  print(token_usage())
  output_list
}

#chat_message <- "I want to understand how covid effected our KPIs."
#x <- determine_options(chat_message)
