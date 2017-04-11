#
#
#
#
#

library(shiny)

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
         tableOutput("collectedinfo")
      )
   )
)



# --------------------- Server Code ------------------------------


server <- function(input, output){
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
    })
    
    output$collectedinfo <- renderTable({data.frame(input$header, input$id, input$dates)})
  
   
}


# Run the application 
shinyApp(ui = ui, server = server)

