#
#

rm(list=ls())
library(shiny)
library(tidyverse)
library(zipcode)
library(devtools)
library(daymetr) # install_github("khufkens/daymetr")
data(zipcode)


# changed the download function to take a dataframe instead of a csv.
# change is on github, need to figure out how to load modified package with library

batch.download.daymet <- function(df,
                                  start_yr=1980,
                                  end_yr=as.numeric(format(Sys.time(), "%Y"))-1,
                                  internal="assign"){
  
  # loop over all lines in the file
  for (i in 1:nrow(df)){
    site = as.character(df[i,1])
    lat = as.numeric(df[i,2])
    lon = as.numeric(df[i,3])
    try(download.daymet(site=site,lat=lat,lon=lon,start_yr=start_yr,end_yr=end_yr,internal=internal),silent=FALSE)
  }
}



# sites <- read.csv(file= "test.csv", colClasses = "character") %>% as_data_frame() %>% mutate(zip = as.character(zip))
# 
# daymetrfood <- left_join(sites, zipcode, by = "zip") %>% select(location, latitude, longitude) %>% 
#                 mutate(location = paste0("x",location))
# 
# 
# batch.download.daymet(df=daymetrfood,start_yr=2009,end_yr=2010,internal=TRUE)
# 
# dat <- data.frame()
# for (i in daymetrfood[,1])
#   dat <- do.call(rbind, get(i))





# ------------------------- User Interface Code -----------------------------

# Goal: User inputs a date range and uploads a csv with one column for site id,
# and either one column for zipcode, or two columns for lat/long
# Then a user can click a download button to retrieve a csv with weather data

ui <- fluidPage(
   
   # Application title
   titlePanel("Collect Daymet Data"),
   
   sidebarLayout(
      sidebarPanel(
        
        dateRangeInput("dates", label = h5(strong("Enter a Date range"))
        ),
        
        
        
        selectInput("id", label = h5(strong("How are locations identified?")), 
                    choices = list("Latitude/Longitude" = 1, "Zip-code" = 2), 
                    selected = 1),
        
        checkboxInput('header', 'Column Headers', TRUE),
        
        fileInput('file', 'Choose file to upload',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )
        )
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        downloadLink("downloadData", "Download")
      )
   )
)



# --------------------- Server Code ------------------------------
# need to modify to input latitude and longitude

server <- function(input, output){
  
  
    data <- reactive({
      # input$file will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inFile <- input$file
      
      if (is.null(inFile))
        return(NULL)
      
       sites <- read.csv(inFile$datapath, header = input$header, colClasses = "character")
       
       daymetrfood <- left_join(sites, zipcode, by = "zip") %>% select(get(names(sites)[1]),
                                                                       latitude,
                                                                       longitude)

       batch.download.daymet(df=daymetrfood, start_yr = 2009, end_yr = 2010)

       dat.ls <- NULL
       
       for (i in 1:nrow(daymetrfood))
         
         dat.ls[[i]] <-  get(daymetrfood[i,1])$data %>%
         mutate(site = as.character(daymetrfood[i,1]))
         #mutate(site = strsplit(as.character(daymetrfood[1,1]), split = "")[[1]][2])

       dat <- data.frame()
       dat <- do.call(rbind,dat.ls)
       return(dat)
    })
    
      

    output$downloadData <- downloadHandler(
      
      filename = function() { 
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      
      content = function(file) {
        
        write.csv(data(), file)
        
      })
}
    
   



# Run the application 
shinyApp(ui = ui, server = server)

