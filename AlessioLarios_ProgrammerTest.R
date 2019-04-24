
# Alessio Larios
# Cherre Exercise

# Load necessary libraries to work with testdb.db
if(!require(DBI)){
  install.packages("DBI")
  library(DBI)
}

if(!require(RSQLite)){
  install.packages("RSQLite")
  library(RSQLite)
}

# Setting my working directory to the folder containing relevant files
setwd("~/Desktop/ProgrammerTest")

# Connect to testdb.db
con = dbConnect(SQLite(), dbname="testdb.db")

query <- dbSendQuery(con, "SELECT personId FROM visits")

visits <- dbFetch(query, n = -1)

dbClearResult(myQuery)

# Problem 1

# Manipulate the data to find the ten people who have visited the most sites
# Import dplyr
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

# Count number of visits each person made
visits %>%
  count(personId) -> visitCount

# Problem 2

# Order the dataframe in descending order of the count variable
visitCount <- visitCount[order(-visitCount$n),] 

# Subset the dataframe to only include the top ten people
visitCountSub <- visitCount[1:10,]

# Rename columns
colnames(visitCountSub) <- c("person_id","num_sites_visited")

# Write results back into db
dbWriteTable(conn = con, name = "frequent_browsers",
             value =  visitCountSub, 
             row.names = pkgconfig::get_config("RSQLite::row.names.table",FALSE),
             append = TRUE)


