box::use(
  ggplot2[theme, element_text],
  dplyr[filter, mutate, left_join, group_by, summarise, collect, arrange, tbl, lag, ungroup, select, rename],
  lubridate[parse_date_time],
  waterfalls[waterfall]
)

box::use(
    app/logic/get_con[get_con]
)

#' Generate Waterfall Chart for Marketing Contributions
#' 
#' This function generates a Waterfall Chart showing category-wise contributions over time.
#' It aggregates values from the `Decomp` table and maps them to `DateGroup` using `DateList`.
#' 
#' @param con Database connection
#' @param options A named list of options (model_name, date_range, categories)
#' @export
plot_waterfall_chart <- function(con, options) {
  mod_name <- options$model_name
  start_date <- options$date_range[1]
  end_date <- options$date_range[2]
  
  decomp <- tbl(con, "Decomp")
  date_mapping <- tbl(con, "DateList") 
  
  # filter
  decomp <- decomp |> 
    left_join(date_mapping, by = "Date") |> 
    filter(Date >= start_date & Date <= end_date) |> 
    filter(category %in% options$categories) |> 
    filter(model_name %in% mod_name) |>
    select(-c(model_name, Date, category)) 

  decomp <- decomp |>
    collect() |>
    group_by(DateGroup) |>
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") |>
    mutate(DateGroup_parsed = parse_date_time(DateGroup, orders = "my")) |>
    arrange(DateGroup_parsed) |>
    select(-DateGroup_parsed) |>
    rename(Label = DateGroup) |>
    mutate(
        lagged_total = lag(total_value, n = 1, default = 0),
        Value = total_value - lagged_total
    ) |>
    select(-c(total_value, lagged_total))


  plot <- waterfall(decomp, rect_text_labels = rep("", nrow(decomp))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  plot
}

#con <- get_con()

# Example Call
#plot_waterfall_chart(con, list(
#  model_name = "Brand",
#  date_range = as.Date(c("2016-10-31", "2018-08-24")),
#  categories = c("TV", "Display Online")
#))
