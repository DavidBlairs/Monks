box::use(
  ggplot2[ggplot, aes, geom_line, labs, scale_color_manual, theme_minimal, scale_color_brewer],
  dplyr[filter, mutate, collect, distinct, pull, tbl],
  tidyr[pivot_wider],
  grDevices[colorRampPalette],
  RColorBrewer[brewer.pal],
  stats[setNames]
)

box::use(
    app/logic/get_con[get_con]
)

#' Generate Time Series Graph
#' 
#' This function generates a time series plot for marketing contributions.
#' It overlays actual KPI values and allows filtering by date and category.
#' 
#' @param con Database connection
#' @param options A named list of options (model_name, date_range, categories)
#' @export
plot_marketing_contributions <- function(con, options = list()) { 
  mod_name <- options$model_name
  start_date <- options$date_range[1]
  end_date <- options$date_range[2]

  decomp <- tbl(con, "Decomp") |> 
    filter(model_name == mod_name) |> 
    filter(Date >= start_date, Date <= end_date) |> 
    filter(category %in% options$categories) |>
    collect()

  # Fetch and filter Actual KPI data
  actuals <- tbl(con, "ActualFittedResiduals") |> 
    filter(model_name == mod_name) |>
    filter(date >= start_date, date <= end_date) |>
    collect()

  all_categories <- tbl(con, "Decomp") |> 
    distinct(category) |> 
    pull(category)

  category_palette <- setNames(
    colorRampPalette(brewer.pal(8, "Dark2"))(length(all_categories)),
    all_categories
  )

  plot <- ggplot() +
    geom_line(data = decomp, aes(x = Date, y = value, color = category), alpha = 0.8, linewidth = 1) +
    geom_line(data = actuals, aes(x = date, y = actual, color = "Actual KPI"), linewidth = 1.5, linetype = "solid") +
    scale_color_manual(values = c(category_palette, "Actual KPI" = "black")) +
    labs(title = paste("Decomposition of KPI for Model:", mod_name),
         x = "Date", 
         y = "Value", 
         color = "Category") +
    theme_minimal()

  plot
}

#con <- get_con()

# Example Call
#plot_marketing_contributions(con, list(
#  model_name = "Brand",
#  date_range = as.Date(c("2016-10-31", "2017-01-02")),
#  categories = c("TV", "Display Online")
#))
