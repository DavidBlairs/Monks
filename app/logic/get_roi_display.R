box::use(
    dplyr[tbl, left_join, rename, mutate, arrange, across, select, slice, n, collect, filter],
    lubridate[parse_date_time],
    DT[datatable]
)

box::use(
    app/logic/get_con[get_con]
)

get_roi_display <- function(con, options = list()) {
  start_date <- options$date_range[1]
  end_date <- options$date_range[2]
  
  roi <- tbl(con, "ROI")
  spend_data <- tbl(con, "SpendData")
 
  roi <- roi |>
    rename(DateGroup = date_group) |>
    select(-model_name) |>
    left_join(spend_data, by = c("SpendVariable", "DateGroup")) |>
    filter(SpendVariable %in% options$spend_variable)

  start_date_month <- format(start_date, "%B %Y")
  end_date_month <- format(end_date, "%B %Y")

  roi <- roi |> 
    collect() |>
    mutate(DateGroup_parsed = parse_date_time(DateGroup, orders = "my")) |>
    arrange(DateGroup_parsed) |>
    select(-DateGroup_parsed) 

  if (start_date_month %in% roi$DateGroup) {
    roi <- roi |>
      slice(which(roi$DateGroup == start_date_month)[1]:n())
  }
  if (end_date_month %in% roi$DateGroup) {
    roi <- roi |>
      slice(1:which(roi$DateGroup == end_date_month)[1])
  }

  roi <- roi |> 
    mutate(ROI = ifelse(Spend == 0, 0, Profit / Spend)) |>
    mutate(across(where(is.numeric), round, 3))

  datatable(
    roi
  )
}

#get_roi_display(con, list(
#    date_range = as.Date(c("2016-11-05", "2017-03-24")),
#    spend_variable = c()
#))
