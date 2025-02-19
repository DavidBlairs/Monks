box::use(
    dplyr[tbl, filter],
    ggplot2[ggplot, geom_line, aes, labs, scale_color_manual, theme_minimal]
)

box::use(
    app/logic/get_con[get_con]
)

plot_model_fit <- function(con, options) {
  start_date <- options$date_range[1]
  end_date <- options$date_range[2]
  mod_name <- options$model_name

  actuals <- tbl(con, "ActualFittedResiduals")

  actuals <- actuals |>
    filter(model_name == mod_name) |>
    filter(date >= start_date & date <= end_date) 

  plot <- ggplot(actuals, aes(x = date)) +
    geom_line(aes(y = actual, color = "Actual"), size = 1) +  # Line for actual values
    geom_line(aes(y = fitted, color = "Fitted"), size = 1, linetype = "dashed") +  # Line for fitted values
    labs(title = "Actual vs. Fitted Values Over Time",
         x = "Date",
         y = "Value",
         color = "Legend") +  # Legend title
    scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red")) +  # Custom colors
    theme_minimal()

  plot
}

#plot_model_fit(con, list(
#    model_name = "Brand",
#    date_range = as.Date(c("2016-11-05", "2018-03-24"))
#))
