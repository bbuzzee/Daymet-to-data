#
#
#


rm(list=ls())
library(shiny)
library(tidyverse)
library(zipcode)
library(devtools)
library(daymetr) # install_github("khufkens/daymetr")
library(shinythemes)
library(plotly)
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



# ------------------------- User Interface Code -----------------------------

# Goal: User inputs a date range and uploads a csv with one column for site id,
# and either one column for zipcode, or two columns for lat/long
# Then a user can click a download button to retrieve a csv with weather data

ui <- fluidPage(
  
   theme =  shinytheme("spacelab"),

   titlePanel(img(src = "daymet_web_banner_NA.jpg")),
   
   sidebarLayout(
     
      sidebarPanel(
                 
          dateRangeInput("dates", label = h5(strong("Enter a Date Range")),
                         start = "2011-01-01", end = "2012-01-01"
          ),
          
          selectInput("id", label = h5(strong("How are locations identified?")), 
                      choices = list("Zip Code" = 1, "Latitude/Longitude" = 2), 
                      selected = 1
          ),
          
          checkboxInput('header', 'Column Headers', TRUE
          ),
          
          helpText("Required upload format:"
          ),
          
          imageOutput("image", width = 100, height = 125
          ),
          
          fileInput('file1', 'Choose file to upload',
                    accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      '.csv'
                    )
          ),
          
          width = 3
      ),

      
      mainPanel(
        column(3, uiOutput("selectyr")),
        column(3, uiOutput("selectsite")),
        column(3, uiOutput("selectmetric")),
        br(),
        br(),
        br(),
        plotlyOutput("plot"),
        downloadButton("downloadData", "Download Daymet Data")
      )
   )
)




# --------------------- Server Code ------------------------------
# need to modify to input latitude and longitude

server <- function(input, output){

    # read in and modify the data from daymetr
  
    data <- reactive({
      # input$file will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inFile <- input$file1
      
      if (is.null(inFile)){
        return(NULL)
      }
      
       sites <- read.csv(inFile$datapath, header = input$header, colClasses = "character")
       
       if (input$id == 1) {
         
       daymetrfood <- left_join(sites, zipcode, by = "zip") %>%
                      select(get(names(sites)[1]), latitude, longitude)
       }
       
       else { 
         daymetrfood <- sites
       }
       
       batch.download.daymet(df=daymetrfood, start_yr = as.numeric(format(as.Date(input$dates[1]), "%Y")),
                             end_yr = as.numeric(format(input$dates[2], "%Y")))

       
       # possibly simplify the following loop?
       
       dat.ls <- NULL
       
       for (i in 1:nrow(daymetrfood))
         
         dat.ls[[i]] <-  get(daymetrfood[i,1])$data %>%
         mutate(site = as.character(daymetrfood[i,1]))

       dat <- data.frame()
       dat <- do.call(rbind,dat.ls)
       
       names(dat) <- c("year", "yday","dayls", "prcp_mm", "srad_wm2", "swe_kgm2",
                       "tmax_c", "tmin_c", "vpr_pa", "site")
       
       # create cumulative growing degree day calculations for each site and for each
       # year
       
       dat <-  dat %>% group_by(year, site) %>% 
               mutate(gdd_day = ifelse((tmax_c+tmin_c)/2 > 10,(tmax_c + tmin_c)/2 - 10, 0)) %>% 
               mutate(cumul_gdd = cumsum(gdd_day))
       
       return(dat)
    })
    
    # prompt user for year to graph
    
    output$selectyr <- renderUI({
      dat <- data()
      dat <- unique(dat$year)
      selectInput(inputId = "yr",label = "Choose a year to graph", dat)
    })
    
    # prompt user for site
    
    output$selectsite <- renderUI({
      dat <- data()
      dat <- unique(dat$site)
      selectInput(inputId = "loc",label = "Choose a site", dat)
    })
    
    output$selectmetric <- renderUI({
      dat <- data()
      dat <- c("cumul_gdd", "prcp_mm", "srad_wm2", "swe_kgm2",
               "tmax_c", "tmin_c", "vpr_pa")
      selectInput(inputId = "metric", label = "Choose a metric", dat)
    })

    # filter and plot the data
    
    output$plot <- renderPlotly({
      
      if(is.null(data())){
        return(NULL)
      }       

      # m <- list(l = 50,r = 50, b = 100, t = 100,pad = 4)
      yaxis <- list(title = input$metric)
      dat <- data()
      met <- input$metric
      dat %>% filter(year == input$yr, site == input$loc) %>%
              plot_ly(x = ~yday, y = ~get(input$metric)) %>% 
              layout(yaxis = yaxis)
      
    })
    
    
    # create download button 
    
    output$downloadData <- downloadHandler(
      
      filename = function() { 
        paste0(Sys.Date(),"-daymet-data", ".csv", sep="")
      },
      
      content = function(file) {
        write.csv(data(), file, row.names = F)
      })

    
    # render appropriate format image based on input
    
    output$image <- renderImage({
      if(input$header & input$id == 1)
        return(list(src = "www/header_zip.png",
                    filetype = "image/png",
                    alt = "string"))
      else if (!input$header & input$id == 1)
        return(list(src = "www/noheader_zip.png",
                    filetype = "image/png",
                    alt = "string"))
      else if (input$header & input$id == 2)
        return(list(src = "www/header_lat.png",
                    filetype = "image/png",
                    alt = "string"))
      else (!input$header & input$id == 2)
        return(list(src = "www/noheader_lat.png",
                    filetype = "image/png",
                    alt = "string"))
    },
    deleteFile = F)
}
    
   

# Run the application 
shinyApp(ui = ui, server = server)

