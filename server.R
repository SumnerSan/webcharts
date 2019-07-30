
# This is the server script, it takes a file upload (myData) and performs automated runchart data wrangling.
# It then plots the resulting dataframe into a chart and outputs the chart to the UI script. It also takes
# a number of user inputs (e.g. annotations) to customise the output. It closes with a download tool so that
# an image of the chart (named with the upload filename) can be downloaded.

shinyServer <- function(input, output, session) {
  
  observeEvent(input$hb, {
    measures2 <- measures[measures$board == input$hb, ]
    
    updatePickerInput(session = session, inputId = "datatype",
                      choices = as.character(unique(measures2$measure)))
    
  }, ignoreInit = TRUE)

  myData <- reactive ({
    
    myData <- switch(input$datatype,"CAMHS seen" = camhs.adjusted.patients.seen,
                     "CAMHS median" = camhs.adjusted.patients.seen,
                     "CAMHS 90th" = camhs.adjusted.patients.seen,
                     "CAMHS waiting" = camhs.adjusted.patients.waiting,
                     "CAMHS referrals" = camhs.referrals,
                     "CAMHS accepted" = camhs.referrals,
                     "CAMHS DNAs" = camhs.DNAs,
                     "CAMHS open" = camhs.open,
                     "PT seen" = pt.adjusted.patients.seen,
                     "PT median" = pt.adjusted.patients.seen,
                     "PT 90th" = pt.adjusted.patients.seen,
                     "PT waiting" = pt.adjusted.patients.waiting,
                     "PT referrals" = pt.referrals,
                     "PT accepted" = pt.referrals) %>%
      mutate(measure_name = input$datatype)
    
    HB<- reactive({
      input$hb
    })
    
    myData <- myData%>%
      left_join(HBlook, by = c("HBT2014" = "HB2014"))
    
    myData <- myData %>%
      left_join(matrix, by = c("measure_name" = "Measure"))
    
    myData <- myData%>%
      filter(HB2014Name == HB())%>%
      mutate(date = ymd(paste0(str_sub(as.character(Month), 1,4), "-", str_sub(as.character(Month), 5,6), "-01")),
             performance = round(switch(input$datatype,"CAMHS seen" = (NumberOfPatientsSeen0To18Weeks/TotalPatientsSeen)*100, 
                                        "CAMHS median" = (MedianWeeksPatientsSeen),
                                        "CAMHS 90th" = (X90thPercentileWeeksPatientsSeen),
                                        "CAMHS waiting" = (NumberOfPatientsWaiting0To18Weeks/TotalPatientsWaiting)*100,
                                        "CAMHS DNAs" = (DidNotAttends/TotalAppointments)*100,
                                        "CAMHS referrals" = (ReferralsRecieved),
                                        "CAMHS accepted" = (ReferralsAccepted/ReferralsRecieved)*100,
                                        "CAMHS open" = (OpenCases),
                                        "PT seen" = (NumberOfPatientsSeen0To18Weeks/TotalPatientsSeen)*100,
                                        "PT median" = (MedianWeeksPatientsSeen),
                                        "PT 90th" = (X90thPercentileWeeksPatientsSeen),
                                        "PT waiting" = (NumberOfPatientsWaiting0To18Weeks/TotalPatientsWaiting)*100,
                                        "PT referrals" = (ReferralsRecieved),
                                        "PT accepted" = (ReferralsAccepted/ReferralsRecieved)*100),0)) %>%
      arrange(date)
      
  })

yaxis <- reactive({
  
  yaxis <- switch(input$datatype,"CAMHS seen" = "percent",
                  "CAMHS median" = "weeks",
                  "CAMHS 90th" = "weeks",
                  "CAMHS percent" = "weeks",
                  "CAMHS waiting" = "percent",
                  "CAMHS DNAs" = "percent",
                  "CAMHS referrals" = "number",
                  "CAMHS accepted" = "percent",
                  "CAMHS open" = "number",
                  "PT seen" = "percent",
                  "PT median" = "weeks",
                  "PT 90th" = "weeks",
                  "PT percent" = "weeks",
                  "PT waiting" = "percents",
                  "PT referrals" = "number",
                  "PT accepted" = "percent")
  
})

heading <- reactive({
  
  heading <- switch(input$datatype,"CAMHS seen" = "percentage of CAMHS patients seen within 18 weeks",
                    "CAMHS median" = "median experienced wait for CAMHS patients",
                    "CAMHS 90th" = "90th percentile experienced wait for CAMHS patients",  
                    "CAMHS waiting" = "percentage of CAMHS patients waiting under 18 weeks",
                    "CAMHS DNAs" = "percentage of CAMHS appointments where patient did not attend",
                    "CAMHS referrals" = "number of CAMHS referrals received",
                    "CAMHS accepted" = "percentage CAMHS referrals accepted",
                    "CAMHS open" = "number of patients on caseload",
                    "PT seen" = "percentage of PT patients seen within 18 weeks",
                    "PT median" = "median experienced wait for PT patients",
                    "PT 90th" = "90th percentile experienced wait for PT patients", 
                    "PT waiting" = "percentage of PT patients waiting under 18 weeks",
                    "PT referrals" = "number of PT referrals received",
                    "PT accepted" = "percentage PT referrals accepted")
  
})
  
  rundata <- reactive({
    
    if (is.null(myData()))
    {return()}
    
    else {
      
      RunChart(myData()$performance, myData()$Chart_type, myData()$date, shiftsens = "none")
      
    }
  })
  
  runplot <- reactive ({
    
    rundata <- rundata()
    
    #print(myData()$Target)
    
    if (unique(myData()$Target) == "yes") {
    
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
      labs(title = paste0(input$hb, ": ", heading())) +
           #subtitle = input$hb)+
      theme_classic()+
      theme(plot.title = element_text(family = "Arial", size = 14, face = "bold"),
            axis.title.x = element_text(family = "Arial", size = 11, face = "bold"),
            axis.title.y = element_text(family = "Arial", size = 11, face = "bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
    } else {
      
    ggplot(rundata) +
      #geom_hline(yintercept = 90, linetype = "dotted", colour = "black", alpha = 0.3) +
      geom_line(aes(x = subgroup, y=measure, group = 1), colour = "#00a2e5", size = 1) + 
      geom_point(aes(x = subgroup, y=measure, group = 1), colour = "#00a2e5", size = 2) +  
      geom_line(aes(x = subgroup, y=median, group = base_n), linetype = "longdash", colour = "#ffcd04") +
      geom_line(aes(x = subgroup, y=baselines, group = base_n), linetype = "solid", colour = "#ffcd04", size = 1) +
      geom_point(aes(x = subgroup, y=as.numeric(highlight), group = 1), colour = "#ffcd04") +
      geom_point(aes(x = subgroup, y=as.numeric(trendind), group = 1), shape = 1, size = 5, colour = "#004785") +
      geom_text(aes(x = subgroup, y = median, group = base_n, label = base_label), vjust = 1, hjust = 0) +
      theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
      #geom_text(x = min(rundata$subgroup), label = "90% target", y = 90, vjust = 1, hjust = 0)+
      #geom_vline(xintercept = event1(), linetype = "dashed")+
      #geom_text(x = event1(), label = stringr::str_wrap(anno1(),30), y = max(as.numeric(rundata$measure))*0.1, vjust = 1)+
      scale_y_continuous(limits=c(0, 1.1*max(myData()$performance)), expand = c(0, 0)) +
      #scale_x_continuous(breaks=pretty(subgroup, n=30)) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
      xlab("Month of return") + ylab(yaxis()) +
      labs(title = paste0(input$hb, ": ", heading())) +
          #subtitle = input$hb)+
      theme_classic()+
      theme(plot.title = element_text(family = "Arial", size = 14, face = "bold"),
            axis.title.x = element_text(family = "Arial", size = 11, face = "bold"),
            axis.title.y = element_text(family = "Arial", size = 11, face = "bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) }
      
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
