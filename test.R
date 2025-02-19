# Load required libraries
box::use(
  DBI[dbConnect],
  odbc[odbc],
  dplyr[tbl],
  glue[glue]
)

# Function to fetch table
fetch_table <- function(tbl_name) {
  # Database connection details
  driver <- "ODBC Driver 18 for SQL Server"
  server <- "tcp:monk-submission.database.windows.net,1433"
  database <- "monk_data"
  uid <- "dave-admin"
  pwd <- "Password1234"  # Replace with your actual password
  
  # Create a connection string
  connection_string <- glue(
    "Driver={{driver}};Server={{server}};Database={{database}};",
    "Uid={{uid}};Pwd={{pwd}};Encrypt=yes;TrustServerCertificate=no;",
    "Connection Timeout=30;"
  )
  
  # Establish the connection
  con <- dbConnect(odbc(), .connection_string = connection_string)
  
  # Fetch and return the table
  tbl(con, tbl_name)
}
