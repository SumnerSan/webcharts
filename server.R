# This is the server script, it takes a file upload (myData) and performs automated runchart data wrangling.
# It then plots the resulting dataframe into a chart and outputs the chart to the UI script. It also takes
# a number of user inputs (e.g. annotations) to customise the output. It closes with a download tool so that
# an image of the chart (named with the upload filename) can be downloaded.

shinyServer <- function(input, output, session) {
  
  HB <- reactive({
    input$hb
  })
  
  observeEvent(input$hb, {
    measures2 <- measures[measures$board == input$hb, ]
    
    updatePickerInput(session = session, inputId = "datatype",
                      choices = as.character(unique(measures2$measure)),
                      selected = ifelse(input$hb == "NHS 24", "PT seen", input$datatype))
    
  }, ignoreInit = TRUE)
  
  myData <- reactive ({
    
    myData <- switch(input$datatype,"CAMHS seen" = camhs.adjusted.patients.seen,
                     "CAMHS %seen" = camhs.adjusted.patients.seen,
                     "CAMHS median" = camhs.adjusted.patients.seen,
                     "CAMHS 90th" = camhs.adjusted.patients.seen,
                     "CAMHS waiting" = camhs.adjusted.patients.waiting,
                     "CAMHS %waiting" = camhs.adjusted.patients.waiting,
                     "CAMHS referrals" = camhs.referrals,
                     "CAMHS accepted" = camhs.referrals,
                     "CAMHS %accepted" = camhs.referrals,
                     "CAMHS rejected" = camhs.referrals,
                     "CAMHS %rejected" = camhs.referrals,
                     "CAMHS referral rate" = camhs.referrals,
                     "CAMHS accepted rate" = camhs.referrals,
                     "CAMHS %DNAs" = camhs.DNAs,
                     "CAMHS open" = camhs.open,
                     "PT seen" = pt.adjusted.patients.seen,
                     "PT %seen" = pt.adjusted.patients.seen,
                     "PT median" = pt.adjusted.patients.seen,
                     "PT 90th" = pt.adjusted.patients.seen,
                     "PT waiting" = pt.adjusted.patients.waiting,
                     "PT %waiting" = pt.adjusted.patients.waiting,
                     "PT referrals" = pt.referrals,
                     "PT accepted" = pt.referrals,
                     "PT %accepted" = pt.referrals,
                     "PT rejected" = pt.referrals,
                     "PT %rejected" = pt.referrals,
                     "PT referral rate" = pt.referrals,
                     "PT accepted rate" = pt.referrals) %>%
      mutate(measure_name = input$datatype)
    
    #myData <- myData%>%
    #left_join(HBlook, by = c("HBT2014" = "HB2014"))
    
    myData <- myData %>%
      left_join(matrix, by = c("measure_name" = "Measure"))
    
    myData <- myData%>%
      filter(HB2014Name == HB())%>%
      mutate(date = ymd(paste0(str_sub(as.character(Month), 1,4), "-", str_sub(as.character(Month), 5,6), "-01")),
             performance = switch(input$datatype, "CAMHS seen" = TotalPatientsSeen,
                                  "CAMHS %seen" = (NumberOfPatientsSeen0To18Weeks/TotalPatientsSeen)*100, 
                                  "CAMHS median" = MedianWeeksPatientsSeen,
                                  "CAMHS 90th" = X90thPercentileWeeksPatientsSeen,
                                  "CAMHS waiting" = TotalPatientsWaiting,
                                  "CAMHS %waiting" = (NumberOfPatientsWaiting0To18Weeks/TotalPatientsWaiting)*100,
                                  "CAMHS %DNAs" = (DidNotAttends/TotalAppointments)*100,
                                  "CAMHS referrals" = ReferralsReceived,
                                  "CAMHS accepted" = ReferralsAccepted,                                  
                                  "CAMHS %accepted" = (ReferralsAccepted/ReferralsReceived)*100,
                                  "CAMHS rejected" = (ReferralsReceived - ReferralsAccepted),
                                  "CAMHS %rejected" = ((ReferralsReceived - ReferralsAccepted)/ReferralsReceived)*100,
                                  "CAMHS referral rate" = (ReferralsReceived/Pop0.17)*1000,
                                  "CAMHS accepted rate" = (ReferralsAccepted/Pop0.17)*1000,
                                  "CAMHS open" = OpenCases,
                                  "PT seen" = TotalPatientsSeen,
                                  "PT %seen" = (NumberOfPatientsSeen0To18Weeks/TotalPatientsSeen)*100,
                                  "PT median" = MedianWeeksPatientsSeen,
                                  "PT 90th" = X90thPercentileWeeksPatientsSeen,
                                  "PT waiting" = TotalPatientsWaiting,
                                  "PT %waiting" = (NumberOfPatientsWaiting0To18Weeks/TotalPatientsWaiting)*100,
                                  "PT referrals" = ReferralsReceived,
                                  "PT accepted" = ReferralsAccepted,
                                  "PT %accepted" = (ReferralsAccepted/ReferralsReceived)*100,
                                  "PT rejected" = (ReferralsReceived - ReferralsAccepted),
                                  "PT %rejected" = ((ReferralsReceived - ReferralsAccepted)/ReferralsReceived)*100,
                                  "PT referral rate" = (ReferralsReceived/Pop.All.ages)*1000,
                                  "PT accepted rate" = (ReferralsAccepted/Pop.All.ages)*1000)) %>%
      arrange(date)
    
  })
  
  yaxis <- reactive({
    
    yaxis <- switch(input$datatype,"CAMHS seen" = "number",
                    "CAMHS %seen" = "percent",
                    "CAMHS median" = "weeks",
                    "CAMHS 90th" = "weeks",
                    "CAMHS waiting" = "number",
                    "CAMHS %waiting" = "percent",
                    "CAMHS %DNAs" = "percent",
                    "CAMHS referrals" = "number",
                    "CAMHS accepted" = "number",
                    "CAMHS %accepted" = "percent",
                    "CAMHS rejected" = "number",
                    "CAMHS %rejected" = "percent",
                    "CAMHS referral rate" = "per 1000",
                    "CAMHS accepted rate" = "per 1000",
                    "CAMHS open" = "number",
                    "PT seen" = "number",
                    "PT %seen" = "percent",
                    "PT median" = "weeks",
                    "PT 90th" = "weeks",
                    "PT waiting" = "number",
                    "PT %waiting" = "percent",
                    "PT referrals" = "number",
                    "PT accepted" = "number",
                    "PT %accepted" = "percent",
                    "PT rejected" = "number",
                    "PT %rejected" = "percent",
                    "PT referral rate" = "per 1000",
                    "PT accepted rate" = "per 1000")
    
  })
  
  heading <- reactive({
    
    heading <- switch(input$datatype,"CAMHS seen" = "total number of CAMHS patients seen",
                      "CAMHS %seen" = "percentage of CAMHS patients who were seen within 18 weeks",
                      "CAMHS median" = "median wait for CAMHS patients: half of patients waited less/more than this",
                      "CAMHS 90th" = "90th percentile wait for CAMHS patients: 90% of patients waited up to this long",
                      "CAMHS waiting" = "total number of CAMHS patients still waiting",
                      "CAMHS %waiting" = "percentage of CAMHS patients who were waiting 0-18 weeks",
                      "CAMHS %DNAs" = "percentage of CAMHS first contact appointments where the patient did not attend",
                      "CAMHS referrals" = "number of CAMHS referrals received",
                      "CAMHS accepted" = "number of CAMHS referrals accepted",
                      "CAMHS %accepted" = "percentage of CAMHS referrals accepted",
                      "CAMHS rejected" = "number of CAMHS referrals rejected",
                      "CAMHS %rejected" = "percentage of CAMHS referrals rejected",
                      "CAMHS referral rate" = "referral rate per 1000 people under 18",
                      "CAMHS accepted rate" = "accepted referral rate per 1000 people under 18",
                      "CAMHS open" = "number of patients on caseload",
                      "PT seen" = "total number of PT patients seen",
                      "PT %seen" = "percentage of PT patients who were seen within 18 weeks",
                      "PT median" = "median wait for PT patients: half of patients waited less/more than this",
                      "PT 90th" = "90th percentile wait for PT patients: 90% of patients waited up to this long", 
                      "PT waiting" = "total number of PT patients still waiting",
                      "PT %waiting" = "percentage of PT patients who were waiting 0-18 weeks",
                      "PT referrals" = "number of PT referrals received",
                      "PT accepted" = "number of PT referrals accepted",
                      "PT %accepted" = "percentage of PT referrals accepted",
                      "PT rejected" = "number of PT referrals rejected",
                      "PT %rejected" = "percentage of PT referrals rejected",
                      "PT referral rate" = "referral rate per 1000 people",
                      "PT accepted rate" = "accepted referral rate per 1000 people")
    
  })
  
  subheading <- reactive({
    
    subheading <- switch(input$datatype,
                         "CAMHS %seen" = "based on the waiting times experienced from referral to treatment",
                         "CAMHS median" = "based on the waiting times experienced from referral to treatment",
                         "CAMHS 90th" = "based on the waiting times experienced from referral to treatment",  
                         "CAMHS %waiting" = "based on the waiting times (from referral) of patients still waiting for treatment",
                         "CAMHS %DNAs" = "the number of first contact appointments where the patient gave no notice of unavailability compared with the total number of first contact appointments offered",
                         "CAMHS referrals" = "the total number of referrals received into the CAMHS service",
                         "CAMHS accepted" = "the number of referrals received into the CAMHS service that were accepted",
                         "CAMHS %accepted" = "the number of accepted referrals compared with the total number of referrals received",
                         "CAMHS rejected" = "the number of referrals received into the CAMHS service that were rejected",
                         "CAMHS %rejected" = "the number of rejected referrals compared with the total number of referrals received",
                         "CAMHS referral rate" = "based on the number of referrals received into the CAMHS service and the 2018 mid-year population 0-17",
                         "CAMHS accepted rate" = "based on the number of accepted CAMHS referrals and the 2018 mid-year population 0-17",
                         "CAMHS open" = "the total number of individuals that the CAMHS services have on their systems as current patients",
                         "PT %seen" = "based on the waiting times experienced from referral to treatment",
                         "PT median" = "based on the waiting times experienced from referral to treatment",
                         "PT 90th" = "based on the waiting times experienced from referral to treatment", 
                         "PT %waiting" = "based on the waiting times (from referral) of patients still waiting for treatment",
                         "PT referrals" = "the total number of referrals received into the PT service",
                         "PT accepted" = "the number of referrals received into the PT service that were accepted",
                         "PT %accepted" = "the number of accepted referrals compared with the total number of referrals received",
                         "PT rejected" = "the number of referrals received into the PT service that were rejected",
                         "PT %rejected" = "the number of rejected referrals compared with the total number of referrals received",
                         "PT referral rate" = "based on the number of referrals received into the PT service and the 2018 mid-year population",
                         "PT accepted rate" = "based on the number of accepted PT referrals and the 2018 mid-year population")
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
      
      ggplot(rundata, aes(x = subgroup)) +
        geom_hline(yintercept = 90, linetype = "dotted", colour = "black", alpha = 0.3) +
        geom_line(aes(y=measure, group = 1), colour = "#00a2e5", size = 1) + 
        geom_point(aes(y=measure, group = 1), colour = "#00a2e5", size = 2) +  
        geom_line(aes(y=median, group = base_n), linetype = "longdash", colour = "#ffcd04") +
        geom_line(aes(y=baselines, group = base_n), linetype = "solid", colour = "#ffcd04", size = 1) +
        geom_point(aes(y=as.numeric(highlight), group = 1), colour = "#ffcd04") +
        geom_point(aes(y=as.numeric(trendind), group = 1), shape = 1, size = 5, colour = "#004785") +
        geom_text(aes(y = median, group = base_n, label = base_label), vjust = 1, hjust = 0) +
        #theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
        geom_text(x = min(rundata$subgroup), label = "90% target", y = 90, vjust = 1, hjust = 0)+
        scale_y_continuous(limits=c(0, 110), expand = c(0, 0)) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
        xlab("Reporting month") + ylab(yaxis()) +
        #labs(title = paste0(input$hb, ": ", heading()), +
        #subtitle = paste(subheading()))+
        labs(title = paste0(input$hb, ": ", heading()),
             subtitle = subheading(),
             caption = paste("Source: ISD",  myData()$Service, "Waiting Times database")) +
        theme_classic()+
        theme(plot.title = element_text(family = "Arial", size = 14, face = "bold"),
              axis.title.x = element_text(family = "Arial", size = 11, face = "bold"),
              axis.title.y = element_text(family = "Arial", size = 11, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
    } else {
      
      ggplot(rundata, aes(x = subgroup)) +
        geom_line(aes(y=measure, group = 1), colour = "#00a2e5", size = 1) + 
        geom_point(aes(y=measure, group = 1), colour = "#00a2e5", size = 2) +  
        geom_line(aes(y=median, group = base_n), linetype = "longdash", colour = "#ffcd04") +
        geom_line(aes(y=baselines, group = base_n), linetype = "solid", colour = "#ffcd04", size = 1) +
        geom_point(aes(y=as.numeric(highlight), group = 1), colour = "#ffcd04") +
        geom_point(aes(y=as.numeric(trendind), group = 1), shape = 1, size = 5, colour = "#004785") +
        geom_text(aes(y = median, group = base_n, label = base_label), vjust = 1, hjust = 0) +
        theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
        scale_y_continuous(limits=c(0, 1.1*max(myData()$performance)), expand = c(0, 0)) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
        xlab("Reporting month") + ylab(yaxis()) +
        #labs(title = paste0(input$hb, ": ", heading()), +
        #ubtitle = paste(subheading()))+
        labs(title = paste0(input$hb, ": ", heading()),
             subtitle = subheading(),
             caption = paste("Source: ISD",  myData()$Service, "Waiting Times database")) +
        theme_classic()+
        theme(plot.title = element_text(family = "Arial", size = 14, face = "bold"),
              axis.title.x = element_text(family = "Arial", size = 11, face = "bold"),
              axis.title.y = element_text(family = "Arial", size = 11, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) }
  }) # ggplot chart
  
  output$runchart <- renderPlot({
    
    shiny::validate(
      shiny::need(try(!is.null(rundata())),
                  "Creating chart")
    )
    runplot()})
  
  #output$footnote <- renderText({footnote()})
  
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
