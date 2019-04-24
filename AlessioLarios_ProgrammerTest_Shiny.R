
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

# Connect to testdb.db to import visits, people, and sites tables
con = dbConnect(SQLite(), dbname="testdb.db")

query1 <- dbSendQuery(con, "SELECT personId,siteId FROM visits")

visits <- dbFetch(query1, n = -1)

dbClearResult(query1)

query2 <- dbSendQuery(con, "SELECT id,first_name,last_name FROM people")

people <- dbFetch(query2, n = -1)

dbClearResult(query2)

query3 <- dbSendQuery(con, "SELECT id,url FROM sites")

sites <- dbFetch(query3, n = -1)

dbClearResult(query3)

rm(list = c("query1","query2","query3"))

# Create full name field for people table
people$full_name <- NA
people$full_name <- paste(people$first_name,people$last_name,sep = " ")

# Rename id in people table
colnames(people)[1] <- "personId"

# Manipulate the data to find how many times people visited sites
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

visits %>%
  count(personId) -> visitCount

# Merge people and visitCount
visitCount <- merge(visitCount,people,by="personId")

# Remove unnecessary columns
visitCount <- visitCount[,c(1,2,5)]

# Rearrange the dataset
visitCount <- as.data.frame(cbind(visitCount$personId,
                                  cbind(visitCount$full_name,
                                        visitCount$n)))

# Rename column names
colnames(visitCount) <- c("ID","Name","Number of Sites Visited")

# Sort by descending order 'Number of Sites Visited' 
visitCount$'Number of Sites Visited' <- as.numeric(visitCount$'Number of Sites Visited')
visitCount <- visitCount[order(-visitCount$'Number of Sites Visited'),] 

# Create table showing people's favorite sites
visits %>%
  group_by(personId) %>%
  count(siteId) %>%
  filter(n==max(n))-> mostVisited

# Get the names of the people
mostVisited <- merge(mostVisited,people,by="personId" ,all = TRUE)

# In the visits table, there is someone with id 0 and not someone with id 29
# In the people table, there is someone with id 29 and not someone with id 0
# I'm wondering if they're the same person?

mostVisited <- mostVisited[!(mostVisited$personId %in% c(0,29)), ]

# Rename id in people table
colnames(sites)[1] <- "siteId"

# Get the names of the sites
mostVisitedSites <- merge(mostVisited,sites,by="siteId" ,all = TRUE)

# There isn't a site with id 0
mostVisitedSites <- mostVisitedSites[!(mostVisitedSites$siteId==0), ]

# Remove sites that were not the most visited for anyone
mostVisitedSites <- mostVisitedSites[!(is.na(mostVisitedSites$personId)), ]

# Order the dataframe in descending order of the count variable
mostVisitedSites <- mostVisitedSites[order(-mostVisitedSites$n),] 

colnames(mostVisitedSites)[3] <- "most_visits"

mostVisitedSites <- mostVisitedSites[,c(3,6,7)]

mostVisitedSites <- as.data.frame(cbind(mostVisitedSites$full_name,
                                        cbind(mostVisitedSites$url,
                                              mostVisitedSites$most_visits)))

colnames(mostVisitedSites) <- c("Name","Most Visited Site","Visits")

# Now let's see how popular each site is
visits %>%
  count(siteId) -> siteCount

# Get the names of the sites
namedSiteCount <- merge(siteCount,sites,by="siteId" ,all = TRUE)

namedSiteCount <- namedSiteCount[!(namedSiteCount$siteId==0), ]
namedSiteCount$n[is.na(namedSiteCount$n)] <- 0

namedSiteCount <- as.data.frame(cbind(namedSiteCount$siteId,
                                      cbind(namedSiteCount$url,namedSiteCount$n)))

colnames(namedSiteCount) <- c("ID","Website","Number of Visits")

# Sort by descending order `Number of Visits`
namedSiteCount$`Number of Visits` <- as.numeric(namedSiteCount$`Number of Visits`)
namedSiteCount <- namedSiteCount[order(-namedSiteCount$'Number of Visits'),]

# Clear environment of unnecessary items
rm(list=c("con","mostVisited","people","siteCount","sites","visits"))

# Run the Shiny App
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

if(!require(DT)){
  install.packages("DT")
  library(DT)
}

ui <- fluidPage(
  titlePanel("Cherre Programming Test"),
  "By: Alessio Larios",
  sidebarLayout(
    sidebarPanel(
      radioButtons("choice", label = h4("Choose Table:"),
                   choices = list("Number of Sites Visited by Each Person",
                                  "Individuals' Most Visited Sites",
                                  "Visit Counts of Each Site"))),
    mainPanel(
      DT::dataTableOutput("mytable")
      )
    ))

server <- function(input, output,session) {
  # Depending on the user's choice of radio button in the sidebar, the dataset being used will change
  datasetInput <- reactive({
    switch(input$choice,
           "Number of Sites Visited by Each Person" = visitCount,
           "Individuals' Most Visited Sites" = mostVisitedSites,
           "Visit Counts of Each Site" = namedSiteCount)
  })
  
  output$mytable = DT::renderDataTable({
    datasetInput()
})

}

shinyApp(ui, server)
