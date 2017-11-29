
library(shiny)
library(tidyverse)
library(zipcode)
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

   titlePanel(img(src = "daymet_web_banner_NA.jpg"),
              windowTitle = "Daymet R Shiny"),
   
   sidebarLayout(
     
      sidebarPanel(
               
        dateRangeInput(inputId = "dates",
                       label = h5(strong("Enter a Date Range")),
                       start = "2011-01-01",
                       end = "2012-01-01"
                       ),
        
        selectInput(inputId = "id",
                    label = h5(strong("How are locations identified?")), 
                    choices = list("Zip Code" = 1, "Latitude/Longitude" = 2), 
                    selected = 1
                    ),
        
        checkboxInput(inputId = 'header',
                      label = 'Column Headers',
                      value = TRUE
                      ),
        
        helpText("Required upload format:"
                 ),
        
        imageOutput(outputId = "image",
                    width = 100,
                    height = 125
                    ),
        
        fileInput(inputId = 'file1',
                  label = 'Choose a file to upload',
                  accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            '.csv'
                            )
                  ),
        width = 3
        ),

      
      mainPanel(
        column(3, uiOutput("selectyr")
        ),
        column(3, uiOutput("selectsite")
        ),
        column(3, selectInput(inputId = "metric",
                              label = "Metrics",
                              choices =  c("dayl_s",
                                           "gdd_cumul",
                                           "prcp_mm",
                                           "prcp_cumul",
                                           "srad_wm2",
                                           "srad_cumul",
                                           "swe_kgm2",
                                           "tmax_c",
                                           "tmin_c",
                                           "vpr_pa"
                                           )
                              )
               ),
        br(),
        br(),
        br(),
        plotlyOutput("plot"),
        column(4, helpText("Learn more at the",
                           a("Daymet Homepage",
                            href = "https://daymet.ornl.gov/overview.html",
                            target = "_blank"
                            )
                           )
               ),
        column(3, "" ),
        column(3, downloadButton(outputId = "downloadData",
                                 label = "Download Daymet Data"
                                 )
               )
        )
      )
   )




# --------------------- Server Code ------------------------------


server <- function(input, output){

    # DaymetR requires site, lat, long format, so if a user uploads zip identifiers
    # it needs to be joined with zipcode database and reduced to site, lat, long via select
    # returns a single datafame containing all daymet data
  
    data <- reactive({
      
      # input$file will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
     inFile <- input$file1
      
     # inFile <- NULL
     if (is.null(inFile)){
      sites <- data.frame(location = c('Yellowstone', 'Death Valley'), zip = c('59730', '92328'))
     }else{
      sites <- read.csv(file = inFile$datapath,
                       header = input$header,
                       colClasses = "character"
                       )
     }
     
     
     # any time a two column file is uploaded, rename cols and join with zipcode database
     if (ncol(sites) == 2){
       
      names(sites) <- c("location", "zip")
      daymetrfood <- left_join(sites, zipcode, by = "zip") %>%
                    select(get(names(sites)[1]), latitude, longitude)
     }else{
       # if lat/long are uploaded, the file can be fed straight to daymetR
       daymetrfood <- sites
     }
     
     

     
     # DaymetR function to download data
     
     batch.download.daymet(df = daymetrfood,
                           start_yr = as.numeric(format(as.Date(input$dates[1]), "%Y")),
                           end_yr = as.numeric(format(input$dates[2], "%Y")
                                               )
                           )

     
     # compile data from download into one file
     # possibly simplify?
     nrow(daymetrfood)
     dat.ls <- NULL
     
     for (i in 1:nrow(daymetrfood)){
       
       dat.ls[[i]] <-  get(as.character(daymetrfood[i,1]))$data %>%
       mutate(site = as.character(daymetrfood[i,1])
              )
       
     }
     
     dat <- data.frame()
     dat <- do.call(rbind, dat.ls)
     
     names(dat) <- c("year",
                     "yday",
                     "dayl_s",
                     "prcp_mm",
                     "srad_wm2",
                     "swe_kgm2",
                     "tmax_c",
                     "tmin_c",
                     "vpr_pa",
                     "site"
                     )
     
     # create cumulative growing degree day calculations for each site and for each
     # year
     
     dat <-  dat %>% group_by(year, site) %>% 
             mutate(gdd_day = ifelse((tmax_c+tmin_c)/2 > 10, (tmax_c + tmin_c)/2 - 10, 0)) %>% 
             mutate(gdd_cumul = cumsum(gdd_day),
                    srad_cumul = cumsum(srad_wm2),
                    prcp_cumul = cumsum(prcp_mm))
     
     # move site column to column 1 for easy key-value identification
     dat <- dat %>% select(site, everything())
     
     return(dat)
   })
    
    # prompt user for year to graph
    
  output$selectyr <- renderUI({
    dat <- data()
    selectInput(inputId = "yr",
                label = "Choose a year",
                choices = unique(dat$year))
  })
    
    # prompt user for site
    
  output$selectsite <- renderUI({
    dat <- data()
    selectInput(inputId = "loc",
                label = "Choose a site",
                choices = unique(dat$site))
  })


    # --------------- Create Plot ---------------
    
  output$plot <- renderPlotly({
      
    # if(is.null(data())){
    #   return(NULL)
    # }       

    m <- list(l = 70,
              r = 70,
              b = 100,
              t = 25,
              pad = 1
              )
    
    yaxis <- list(title = input$metric)
    
    data() %>% filter(year %in% input$yr, site %in% input$loc) %>%
                plot_ly(x = ~yday, y = ~get(input$metric)) %>% 
                config(displayModeBar = FALSE) %>% 
                layout(yaxis = yaxis, autosize = T, margin = m)
              
      
  })
    
    
  output$downloadData <- downloadHandler(
      
    filename = function() { 
      paste0(Sys.Date(),"-daymet-data", ".csv", sep="")
      },
    
    content = function(file) {
      write.csv(data(), file, row.names = F)
      }
    )

    
    # render appropriate format image based on input, four possibilities
    
  output$image <- renderImage({
    
    if (input$header & input$id == 1){
      return(list(src = "www/header_zip.PNG", # changed this!
                  filetype = "image/png",
                  alt = "string"
                  )
             )
    } else if (!input$header & input$id == 1){
      return(list(src = "www/noheader_zip.PNG",
                  filetype = "image/png",
                  alt = "string"
                  )
             )
    } else if (input$header & input$id == 2){
      return(list(src = "www/header_lat.PNG",
                  filetype = "image/png",
                  alt = "string",
                  width = 200
                  )
             )
    } else {
      return(list(src = "www/noheader_lat.PNG",
                  filetype = "image/png",
                  alt = "string",
                  width = 200
                  )
             )
      }
      
    },
  deleteFile = F)
}
    
   

# Run the application 
shinyApp(ui = ui, server = server)

