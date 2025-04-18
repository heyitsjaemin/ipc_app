library(DBI)
library(RPostgres)
library(readr)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("PG_DBNAME"),
  host = Sys.getenv("PG_HOST"),
  port = as.integer(Sys.getenv("PG_PORT")),
  user = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASSWORD")
)

# Read files
# overdose_state <- read_delim("data/ipcapp_030_overdose_by_state_2018_2023.txt", delim = "\t")
# overdose_county <- read_delim("data/ipcapp_031_overdose_by_county_2018_2023.txt", delim = "\t")
injury_state <- read_csv("data/ipcapp_042_state_drug_suicide_homicide_firearm.csv")
injury_county <- read_csv("data/ipcapp_041_county_drug_suicide_homicide_firearm.csv")


# Upload again
# dbWriteTable(con, "overdose_by_state", overdose_state, overwrite = TRUE, row.names = FALSE)
# dbWriteTable(con, "overdose_by_county", overdose_county, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "injury_state", injury_state, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "injury_county", injury_county, overwrite = TRUE, row.names = FALSE)

# Confirm
print(dbListTables(con))
dbDisconnect(con)
