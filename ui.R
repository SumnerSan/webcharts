# Description - Describe what the app does (e.g. visualizes births data)
# User interface - how your app looks and elements users can interact with

shinyUI(fluidPage(
  
  title = "Runchart Builder",
  
  fluidRow(
    column(2, "Build your chart",
           selectInput("datatype", "Select measure", c("CAMHS seen", 
                                                       "CAMHS waiting",
                                                       "PT seen",
                                                       "PT waiting"), selected = NULL, multiple = FALSE, selectize = FALSE),
           selectInput("hb", "Select geography", c("NHS Ayrshire and Arran",
                                                   "NHS Borders",
                                                   "NHS Dumfries and Galloway",
                                                   "NHS Fife",
                                                   "NHS Forth Valley",
                                                   "NHS Grampian",
                                                   "NHS Greater Glasgow and Clyde", 
                                                   "NHS Highland", 
                                                   "NHS Lanarkshire", 
                                                   "NHS Lothian", 
                                                   "NHS Orkney", 
                                                   "NHS Shetland", 
                                                   "NHS Tayside", 
                                                   "NHS Western Isles",
                                                   "NHS Scotland"), selected = NULL, multiple = FALSE, selectize = FALSE)),
    
    
    # Show a plot of the generated distribution
    column(10,
           plotOutput("runchart"))

  ),
  
  
  fluidRow(column(3,downloadButton("pullchart", "Download chart"))
  )))