
# This is the server script, it takes a file upload (myData) and performs automated runchart data wrangling.
# It then plots the resulting dataframe into a chart and outputs the chart to the UI script. It also takes
# a number of user inputs (e.g. annotations) to customise the output. It closes with a download tool so that
# an image of the chart (named with the upload filename) can be downloaded.

shinyServer <- function(input, output) {

  myData <- reactive ({
    
    myData <- switch(input$datatype,"CAMHS seen" = camhs.adjusted.patients.seen, 
                     "CAMHS waiting" = camhs.adjusted.patients.waiting,
                     "PT seen" = pt.adjusted.patients.seen,
                     "PT waiting" = pt.adjusted.patients.waiting)
    
    HB<- reactive({
      input$hb
    })
    
    myData <- myData%>%
      left_join(HBlook, by = c("HBT2014" = "HB2014"))
    
    myData <- myData%>%
      filter(HB2014Name == HB())%>%
      mutate(date = ymd(paste0(str_sub(as.character(Month), 1,4), "-", str_sub(as.character(Month), 5,6), "-01")),
             performance = switch(input$datatype,"CAMHS seen" = (NumberOfPatientsSeen0To18Weeks/TotalPatientsSeen)*100, 
                                  "CAMHS waiting" = (NumberOfPatientsWaiting0To18Weeks/TotalPatientsWaiting)*100,
                                  "PT seen" = (NumberOfPatientsSeen0To18Weeks/TotalPatientsSeen)*100,
                                  "PT waiting" = (NumberOfPatientsWaiting0To18Weeks/TotalPatientsWaiting)*100))%>%
      arrange(date)
      
  })

yaxis <- reactive({
  
  yaxis <- switch(input$datatype,"CAMHS seen" = "% of CAMHS list seen within 18 weeks", 
                  "CAMHS waiting" = "% of CAMHS list currently waiting under 18 weeks",
                  "PT seen" = "% of PT list seen within 18 weeks",
                  "PT waiting" = "% of PT list currently waiting under 18 weeks")
  
})

heading <- reactive({
  
  heading <- switch(input$datatype,"CAMHS seen" = "% of CAMHS list seen within 18 weeks", 
                  "CAMHS waiting" = "% of CAMHS list currently waiting under 18 weeks",
                  "PT seen" = "% of PT list seen within 18 weeks",
                  "PT waiting" = "% of PT list currently waiting under 18 weeks")
  
})
  
  rundata <- reactive({
    
    if (is.null(myData()))
    {return()}
    
    else {
      
      RunChart(myData()$performance, myData()$date, shiftsens = "none")
      
    }
  })
  
  runplot <- reactive ({
    
    rundata <- rundata()
    
  ggplot(rundata) +
    geom_hline(yintercept = 90, linetype = "dotted", colour = "black", alpha = 0.3) +
    geom_line(aes(x = subgroup, y=measure, group = 1), colour = "#00a2e5", size = 1) + 
    geom_point(aes(x = subgroup, y=measure, group = 1), colour = "#00a2e5", size = 2) +  
    geom_line(aes(x = subgroup, y=median, group = base_n), linetype = "longdash", colour = "#ffcd04") +
    geom_line(aes(x = subgroup, y=baselines, group = base_n), linetype = "solid", colour = "#ffcd04", size = 1) +
    geom_point(aes(x = subgroup, y=as.numeric(highlight), group = 1), colour = "#ffcd04") +
    geom_point(aes(x = subgroup, y=as.numeric(trendind), group = 1), shape = 1, size = 5, colour = "#004785") +
    geom_text(aes(x = subgroup, y = median, group = base_n, label = base_label), vjust = 1, hjust = 0) +
    theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
    geom_text(x = min(rundata$subgroup), label = "90% target", y = 90, vjust = 1, hjust = 0)+
    #geom_vline(xintercept = event1(), linetype = "dashed")+
    #geom_text(x = event1(), label = stringr::str_wrap(anno1(),30), y = max(as.numeric(rundata$measure))*0.1, vjust = 1)+
    scale_y_continuous(limits=c(0, 110), expand = c(0, 0)) +
    #scale_x_continuous(breaks=pretty(subgroup, n=30)) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
    xlab("Month of return") + ylab(yaxis()) +
    labs(title = paste0("Runchart of ", heading()),
         subtitle = input$hb)+
    theme_classic()+
    theme(plot.title = element_text(family = "Arial", size = 14, face = "bold"),
          axis.title.x = element_text(family = "Arial", size = 11, face = "bold"),
          axis.title.y = element_text(family = "Arial", size = 11, face = "bold"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  }) # ggplot chart
  
  output$runchart <- renderPlot({runplot()})
  
  output$rundata <- renderDataTable({rundata()})
  
  output$downloaddata <- downloadHandler(
    filename = function(){
      paste("Runchart data for ", input$hb, " ", input$datatype, ".csv", sep = "")}, 
    content = function(file){
      write.csv(rundata(),
                file,
                row.names = FALSE)
    }
  )
  
  output$downloadchart <- downloadHandler(
    filename = function() {
      paste0("Runchart for ", input$hb, " ", input$datatype, ".png", sep = "")},
    content = function(file) {
      ggsave(file, plot = runplot(), device = "png")
    }
  )
  
}



## END
