box::use(
  DBI[dbConnect],
  odbc[odbc],
  dplyr[tbl]
)

#' Setup a database connection
#'
#' @return a con object
#' @export
get_con <- function(is_local=T) {
  uid <- Sys.getenv("DB_UID")
  pwd <- Sys.getenv("DB_PWD")
  
  if (is_local) {
    driver <- "ODBC Driver 18 for SQL Server"
    server <- "tcp:monk-submission.database.windows.net,1433"
    database <- "monk_data"

    connection_string <- sprintf(
      "Driver={%s};Server=%s;Database=%s;Uid=%s;Pwd=%s;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;",
      driver, server, database, uid, pwd
    )

    return(dbConnect(odbc(), .connection_string = connection_string))
  } else {
    server <- "monk-submission.database.windows.net"
    database <- "monk_data"

    con <- dbConnect(
      odbc::odbc(),
      Driver = "FreeTDS",
      Server = server,
      database = database,
      UID = uid,
      PWD = pwd,
      Port = 1433,
      TDS_Version = 7.4
    )
    return(con)
  }
}
