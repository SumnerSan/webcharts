# Description - Describe what the app does (e.g. visualizes births data)
# User interface - how your app looks and elements users can interact with

shinyUI(fluidPage(
  
  tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
              type="text/javascript"),

  title = "Runchart Builder",
  
  # Show a plot of the generated distribution
  
  br(),
  
  fluidRow(column(12,
                  plotOutput("runchart")
  )),
  
  br(),
  
  hr(),
  
  fluidRow(
    
    column(2, pickerInput(
      inputId = "datatype",
      label = "Select measure",
      choices = as.character(unique(measures$measure)),
      width = "fit"
    )),
    column(2, offset = 1, pickerInput(
      inputId = "hb",
      label = "Select board",
      choices = as.character(unique(measures$board))
    )),
    
    column(2, offset = 1, style = "margin-top: 25px;", 
           downloadButton("downloaddata", "Download data")),
    
    column(2, style = "margin-top: 25px;",
           downloadButton("downloadchart", "Download chart"))),

HTML('<div data-iframe-height></div>'))) 


#br()

# Show table

#fluidRow(column(7, dataTableOutput("rundata"))
#)
