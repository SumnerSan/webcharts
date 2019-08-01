# Description - Describe what the app does (e.g. visualizes births data)
# User interface - how your app looks and elements users can interact with

shinyUI(fluidPage(
  title = "Runchart Builder",
                  
  fluidRow(
    br(),
    column(2, "Build your chart",
    column(12,
      pickerInput(
        inputId = "datatype",
        label = "Select measure",
        choices = as.character(unique(measures$measure)),
        width = "fit"
      ),
      pickerInput(
        inputId = "hb",
        label = "Select board",
        choices = as.character(unique(measures$board))
      )
    )
    ),
  
  # Show a plot of the generated distribution
    
    column(8, offset = 0.9,
           plotOutput("runchart")
    )
    
  ),
                      
  br(),
                      
  # Show table
          
  #fluidRow(column(7, dataTableOutput("rundata"))
  #),
                    
  fluidRow(
    #column(2, downloadButton("downloaddata", "Download data")
    #),
                    
    column(10, downloadButton("downloadchart", "Download chart")
    )
  ))
)
        