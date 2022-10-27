
table = "visitors"
my_db_file <- "visitor_log.sqlite"
test <- as.data.frame(cbind("Name" = "Person, Test", "Phone" = "604-555-5555", "Email"= "test.person@notanemail.com", "Reason4Visit" = "Other", "EntryDate" = as.character(today()), "EntryTime" = as.character(now()), "ExitTime" = NA))
# Create database

saveData <- function(data) {
  require(dplyr)
  require(RSQLite)
  require(dbplyr)
  # Connect to the database
  db <- dbConnect(RSQLite::SQLite(), my_db_file)
  src_dbi(db)
  # Create data table
  # dbCreateTable(conn = db, name = "visitors", fields = as.data.frame(visitor) , row.names = NULL, temporary = FALSE)
  
  # Submit the update query and disconnect
  dbWriteTable(conn = db, 
               name = "visitors", 
               value = data, 
               overwrite = FALSE,
               append = TRUE, 
               temporary = FALSE)
  #copy_to(db, data, temporary = FALSE)
  #dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function() {
  require(dplyr)
  require(RSQLite)
  require(dbplyr)
  require(lubridate)
  # Connect to the database
  db <- dbConnect(RSQLite::SQLite(), my_db_file)
  src_dbi(db)
  # Construct the fetching query
  visitors <- tbl(db, table) %>%
    filter(EntryDate == as.character(today())) %>%
    filter(is.na(ExitTime)) %>%
    collect()
  #query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  #data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return(visitors)
}

updateData <- function(name){
  require(lubridate)
  update_query <- paste0("UPDATE visitors ",
                        "SET ExitTime='", as.character(now()),
                        "' WHERE Name='", name, 
                        "' AND EntryDate='", as.character(today()),
                        "'")
  db <- dbConnect(RSQLite::SQLite(), my_db_file)
  src_dbi(db)
  dbExecute(db, update_query)
  dbDisconnect(db)
}

## Now setup a scheduler so we don't have to remember to remove old entries from the database
shiny.auto.delete <- function(table = table){ # 24 hours 60 minutes 60 seconds
  require(lubridate)
  
  db <- dbConnect(RSQLite::SQLite(), my_db_file)
  src_dbi(db)
  
  time <- today() - 30
  
  visitors <- tbl(db, table) %>%
    filter(as.Date(EntryDate) > time) %>%
  collect()
  
  
  delete_query <- paste0(
    "DELETE FROM ", table,
    " WHERE EntryDate > ", time
  )
  
  if (nrow(visitors) != 0){
    dbExecute(db, delete_query)
  }
  
  dbDisconnect(db)
}
  
