library(shinyWidgets)
library(shinydashboard)
library(plotrix)
library(gplots)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(reshape)
library(dplyr)
library(hms)
library(shiny)
library(sqldf)
library(ggplot2)
library(r2symbols)
library(leaflet)
library(gridExtra)
library(ggthemes)
library(readr)
library(shinythemes)
list_rnow <- list()
counter = 1
ui <- fluidPage(
    titlePanel(tags$div("SCATS Data Manager", tags$br(),
                        h1(strong("Developed by Abdullah Shabarek"), style = "font-size:15px;"))),dashboardSidebar(
      sidebarMenu(
        menuItem("MX Value Analysis", tabName = "Max_Value_Problems", icon = icon("clock-o")),menuItem("Detector Alarms", tabName = "Detector_Alarms", icon = icon("cog")),menuItem("Degree of Saturation Analysis", tabName = "Degree_Saturation", icon = icon("battery-full")),menuItem("Degree of Saturation over Phase Timing Analysis", tabName = "Degree_Saturation_Phase", icon = icon("list-alt")),menuItem("Resources and Instructions", tabName = "Resources", icon = icon("flag-checkered"))
      )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Max_Value_Problems",
                fluidPage(
                  titlePanel("MX Value Analysis"),
                  sidebarLayout(
                    sidebarPanel(
                      bootstrapPage(
                        div(style="display:inline-block",textInput(inputId="lowertime", label="Enter the lower time bound", placeholder = "06:00:00", width=120, value = "06:00:00")),div(style="display:inline-block",textInput(inputId="uppertime", label="Enter the upper time bound", placeholder = "21:00:00", width=120, value = "21:00:00")),div(style="display:inline-block",textInput(inputId="max_value_threshold", label="Enter the Max Value Threshold", placeholder = 15, width=120, value = 15))),fileInput("file1",
                  "Choose Event files from a directory",
                  multiple = TRUE,
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        actionButton('previewData','Preview', icon = icon("refresh")),
        downloadButton('downloadData', 'Download')
      ),
      mainPanel(
        plotOutput("Plot"),
        tableOutput('contents')
      )
      
    )
  )
  ),
  tabItem(tabName = "Detector_Alarms",
            fluidPage(
              titlePanel("Detector Alarms"),
              sidebarLayout(
                sidebarPanel(
                  bootstrapPage(
                    ),
                  fileInput("file3",
                            "Choose SCATS Log file from a directory",
                            multiple = FALSE,
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv')),
                  downloadButton('downloadData3', 'Download')
                ),
                mainPanel(
                  tableOutput('contents3')
                )
              )
            )
  ),
  tabItem(tabName = "Degree_Saturation",
          fluidPage(
            titlePanel("Degree of Saturation Analysis"),
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(
                  selectInput("Rt44444", "Route Name:",
                              c("US-1" = "RT1",
                                "NJ-18" = "RT18",
                                "NJ-73" = "RT73",
                                "US-130" = "RT130"))
               ),
                fileInput("file4",
                          "Choose SCATS Strategic Monitor files from a directory",
                          multiple = TRUE,
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                actionButton('previewData4','Preview', icon = icon("refresh")),
                downloadButton('downloadData4', 'Download')
              ),
              mainPanel(plotOutput("Plot4"),
                tableOutput('contents4')
              )
            )
          )
  ),
  tabItem(tabName = "Degree_Saturation_Phase",
          fluidPage(
            titlePanel("Degree of Saturation over Phase Timing Analysis"),
            
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(
                ),selectInput("degree_saturation_analysis_type", "Choose a Type of Analysis:",
                             list(`Type of Analysis` = list("Two Axis","Ratio"))),
                selectInput("degree_saturation_analysis_type2", "Choose Type of Details:",
                            list(`Type of Analysis` = list("Phases"
                                                           ,"Signal Groups"
                                                           )))
                          ,fileInput("file5",
                          "Choose a SCATS Strategic Monitor file from a directory",
                          multiple = FALSE,
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                actionButton('previewData5','Preview', icon = icon("refresh")),
                downloadButton('downloadData5', 'Download')
              ),
              mainPanel(
                fluidRow(plotOutput("Plot5_2",height="300px"),
                         plotOutput("Plot5",height="3000px"),
                        tableOutput('contents5'))
              )
            )
          )
  ),
  tabItem(tabName = "Resources",
          fluidPage(
            titlePanel("Resources and Instructions"),
            sidebarLayout(
              sidebarPanel(
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Detector%20Alarms%20Instructions.pdf", "MX Value Instructions"),tags$br(),tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Degree%20of%20Saturation%20Analysis%20Sample%20Data.zip", "MX Value Sample Data"),tags$br(),tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Detector%20Alarms%20Instructions.pdf", "Detector Alarms Instructions"),tags$br(),tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Detector%20Alarm%20Sample%20Data.zip", "Detector Alarms Sample Data"),tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Degree%20of%20Saturation%20Analysis%20Instructions.pdf", "Degree of Saturation Analysis Instructions"),tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Degree%20of%20Saturation%20Analysis%20Sample%20Data.zip", "Degree of Saturation Analysis Sample Data"),
                tags$br(),tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Degree%20of%20Saturation%20over%20Phase%20Timing%20Analysis%20Instructions.pdf", "Degree of Saturation over Phase Timing Analysis Instructions"),tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Degree%20of%20Saturation%20over%20Phase%20Timing%20Analysis%20Sample%20Data.txt","Degree of Saturation over Phase Timing Analysis Sample Data"),
                tags$br()
              ),
              mainPanel(img(src = "https://www.limsforum.com/wp-content/uploads/uloo0b2c.bmp", height = 200, width = 200))
            )
          )
  )
  
  ),hr(),
  h6(print(r2symbols::symbol("copyright")),print(" Abdullah Shabarek"),
     style = "font-size:10px;")
  )
,theme = shinytheme("sandstone"))



options(shiny.maxRequestSize = 100*1024^2)
server <-  function(input, output) {
  getData <- reactive({
    inFile <- input$file1
    idx <- 0
    if (is.null(inFile)){
      return(NULL)
    }else {  
      files3 = lapply(inFile$datapath, function(y){
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Analyizing the data....", value = 0)
        idx <<- idx + 1
        df = read.csv(y, header = TRUE)
        Site_ID <- as.integer(gsub(".*Site_*|.csv*", "", inFile$name[idx]))
        df2 <- as.data.frame(gsub("\\:.*", "", df$Event))
        df2 = cbind(Site_ID = Site_ID, df2)
        colnames (df2) <- c("Site_ID","Event")
        df2$Info <- gsub(".*:", "", df$Event)
        uniquevalues <- as.data.frame(unique(df2[,"Event"]))
        colnames (uniquevalues) <- "Unique_Values"
        df2$Date <- substr(df$Time, 0, 10)
        df2$Tim <- substr(df$Time, 12, 19)
        df2$Event_Type <- df2$Event
        df_phase_termination <- df2 %>% filter(df2$Event == "Phase termination")
        if( nrow(df_phase_termination) > 1){
        df_phase_termination$Phase <- substr(df_phase_termination$Info, 19,19)
        df4 <- as.data.frame(sub(".*MX=([^.]+)\\, GT.*", "\\1", df_phase_termination$Info))
        colnames (df4) <- "Max_Value"
        df_phase_termination$Max_Value <- df4$Max_Value
        df_phase_termination$ti <- as_hms(df_phase_termination$Tim)
        df_phase_termination <- df_phase_termination %>% filter(df_phase_termination$ti > as_hms(input$lowertime))
        df_phase_termination <- df_phase_termination %>% filter(df_phase_termination$ti < as_hms(input$uppertime))
        df_phase_termination$Max_Value <- as.integer(df_phase_termination$Max_Value)
        for (i in (1:nrow(df_phase_termination))){
          df_phase_termination$Time_Start[i] <- as_hms(as.integer(df_phase_termination$ti[i]/(as.numeric(1)*60))*(as.numeric(1)*60))
        }
        df_phase_termination$Time_Start <- as_hms(df_phase_termination$Time_Start)
        df_phase_termination <- df_phase_termination
        } else {
          df_phase_termination <- NULL
        } 
        df_phase_termination <- df_phase_termination
        df_cycle_length <- df2 %>% filter(df2$Event == "Cycle length")
        if( nrow(df_cycle_length) >1) {
        df_cycle_length$cycle_type <- NULL
        df_cycle_length$cycle_type <- gsub("=.*", "", df_cycle_length$Info)
        df_cycle_length$cycle_length <- NULL
        df_cycle_length$cycle_length <- gsub(".*=", "", df_cycle_length$Info)
        input$lowertime
        input$uppertime
        number_of_seconds <- as.integer(as_hms(input$uppertime) - as_hms(input$lowertime))
        number_of_minutes <- as.integer(number_of_seconds/60)
        df_time <- NULL
        df_time <- data.frame(matrix(, nrow=1440, ncol=1))
        colnames(df_time) <- "tim"
        df_time$tim[1] <- as.integer(as_hms("00:00:00"))
        for (jj in (2:1440-1)){
          df_time$tim[jj+1] <- df_time$tim[jj]+60
        }
        df_time$Time <- NULL
        df_time$Time <- as_hms(df_time$tim)
        df_cycle_length$cycle_type <- trimws(df_cycle_length$cycle_type, which = c("both"))
        df_cycle_length_nominal <- df_cycle_length %>% filter(df_cycle_length$cycle_type == "Nominal")
        df_cycle_length_active <- df_cycle_length %>% filter(df_cycle_length$cycle_type == "Active")
        df_cycle_length_nominal$Time <- NULL
        df_cycle_length_nominal$Time <- as.integer(as.integer(as_hms(df_cycle_length_nominal$Tim))/60)*60
        df_cycle_length_nominal2 <- left_join(df_time, df_cycle_length_nominal, by = c("tim" = "Time"))
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Site_ID)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Event)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Info)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Date)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Event_Type)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(cycle_type)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(cycle_length)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Site_ID, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Event, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Info, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Date, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Event_Type, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(cycle_type, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(cycle_length, .direction = "up")
        df_cycle_length_active$Time <- NULL
        df_cycle_length_active$Time <- as.integer(as.integer(as_hms(df_cycle_length_active$Tim))/60)*60
        df_cycle_length_active2 <- left_join(df_time, df_cycle_length_active, by = c("tim" = "Time"))#, "y" = "y2"))
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Site_ID)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Event)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Info)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Date)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Event_Type)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(cycle_type)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(cycle_length)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Site_ID, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Event, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Info, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Date, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Event_Type, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(cycle_type, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(cycle_length, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2[!duplicated(df_cycle_length_active2$Time),]
        df_cycle_length_nominal2 <- df_cycle_length_nominal2[!duplicated(df_cycle_length_nominal2$Time),]
        df_cycle_length_active3 <- df_cycle_length_active2 %>% select(cycle_length)
        colnames(df_cycle_length_active3) <- c("cycle_length_active")
        df_cycle_length_nominal2
        colnames(df_cycle_length_nominal2) <- c("tim", "Time", "Site_ID", "Event", "Info",
                                                "Date","Tim","Event_Type","cycle_type",
                                                "cycle_length_nominal")
        df_cycle_length2 <- cbind(df_cycle_length_nominal2, df_cycle_length_active3)
        df_cycle_length2$diff_nominal_active <- NULL
        df_cycle_length2$diff_nominal_active <- as.integer(df_cycle_length2$cycle_length_nominal) - as.integer(df_cycle_length2$cycle_length_active)
        df_cycle_length_reduced <- df_cycle_length2 %>% select(Time, cycle_length_active,
                                                               cycle_length_nominal,diff_nominal_active)
        colnames(df_cycle_length_reduced) <- c("Time_Start","cycle_length_active",
                                               "cycle_length_nominal","diff_nominal_active")
        for (k in (1:nrow(df_cycle_length_reduced))){
         if(is.na(df_cycle_length_reduced$cycle_length_nominal[k]) == TRUE){
           df_cycle_length_reduced$cycle_length_nominal [k] <- df_cycle_length_reduced$cycle_length_active [k]
         }
        }
        } else {df_cycle_length_reduced <- NULL
        }
        if (is.null(df_phase_termination) | is.null(df_cycle_length_reduced)){
          df_phase_termination2 <- NULL
        } else {
          df_phase_termination2 <- dplyr::left_join(df_phase_termination, df_cycle_length_reduced, by = c("Time_Start"="Time_Start"))
          df_phase_termination2
        }
        df_phase_termination2
      }
      )
      do.call(rbind, files3)
    }
  })
  getData2 <- reactive({
    inFile2 <- input$file2
    index <- 0
    if (is.null(inFile2)){
      return(NULL)
    }else {
      files4 = lapply(inFile2$datapath, function(yy){
        index <<- index + 1
        dff = read.csv(yy, header = TRUE)
        Site_ID_Phase <- as.integer(gsub(".*Site_*|_Phase.csv*", "", inFile2$name[index]))
        dff = cbind(Site_ID = Site_ID_Phase, dff)
        print(inFile2$name[2])
        dff <- dff %>% dplyr::filter(as.data.frame(dff$Phase) !="Unknown")
        colnames(dff)[3] <- "Duration"
        Number_of_phases <- 0
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="<A>"))>1){
          Number_of_phases = 1
        } else {
          Number_of_phases = 0
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="B"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="C"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="D"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="E"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="F"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="G"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="H"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="I"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="J"))>1){
          Number_of_phases = Number_of_phases+1
        }
        dff <- dff %>%
          mutate(Phasenumber = case_when(
            endsWith(Phase, "<A>") ~ as.integer(1),
            endsWith(Phase, "B") ~ as.integer(2),
            endsWith(Phase, "C") ~ as.integer(3),
            endsWith(Phase, "D") ~ as.integer(4),
            endsWith(Phase, "E") ~ as.integer(5),
            endsWith(Phase, "F") ~ as.integer(6),
            endsWith(Phase, "G") ~ as.integer(7),
            endsWith(Phase, "H") ~ as.integer(8),
            endsWith(Phase, "I") ~ as.integer(9),
            endsWith(Phase, "J") ~ as.integer(10),
            endsWith(Phase, "K") ~ as.integer(11)
          ))
        if (nrow(dff) > 1) {
        for (i in (1:5)){
          if(dff$Phasenumber[1] != 1){
            dff <- dff[2:nrow(dff),]
          }
        }
        dff$Cycle_number <- NULL
        dff <- dff
        for (i in (1:nrow(dff))){
          if (i == 1){
            dff$Cycle_number[i] <- 1
          } else{
            if(as.integer(dff$Phasenumber[i]) == 1 ){
              dff$Cycle_number[i] <- dff$Cycle_number[as.integer(i-1)]+1
            } else {
              dff$Cycle_number[i] <- dff$Cycle_number[as.integer(i-1)]
            }
          }
        }
        dff_grouped <- dff %>% group_by(Cycle_number,Site_ID) %>% summarise(
          Cycle_length = sum(`Duration`))
        dff <- merge(x = dff, y = dff_grouped, by = c("Cycle_number","Site_ID"), all.x = TRUE)
        dff$Phase_skip <- NULL
        dff$Main_Approach_Gapped <- NULL
        coef1 <- as.numeric(input$coef1)
        coef2 <- as.numeric(input$coef2)
        coef3 <- as.numeric(input$coef1) +1
        coef4 <- as.numeric(input$coef2) +1
        coef5 <- as.numeric(input$coef1) +2
        coef6 <- as.numeric(input$coef2) +2
        coef7 <- as.numeric(input$coef1) +3
        coef8 <- as.numeric(input$coef2) +3
        coef9 <- as.numeric(input$coef1) +4
        coef10 <- as.numeric(input$coef2) +4
        max_phase_number <- as.integer(max(dff$Phasenumber))
        for (i in (1:nrow(dff))){
          if( i < max_phase_number+1) {
            if((
              (((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef10))
            )
            {
              dff$Phase_skip[i] <- 1
            } else {
              dff$Phase_skip[i] <- 0
            }
          } else if ( i > nrow(dff)-max_phase_number ){
            if((
              (((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef10))
            ){
              dff$Phase_skip[i] <- 1
            } else {
              dff$Phase_skip[i] <- 0
            }
          } else {if((
            (((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef1 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef2 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef10) |
            (((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef1 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef2 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef10))
          ){
            dff$Phase_skip[i] <- 1
          } else {
            dff$Phase_skip[i] <- 0
          }
          }
        }
        for (i in (1:nrow(dff))){
          if(is.na(dff$Gapped[i]) && dff$Phasenumber[i] == 1){
            dff$Main_Approach_Gapped[i] <- 0
          } else if (dff$Phasenumber[i] != 1){
            dff$Main_Approach_Gapped[i] <- 0
          } else {
            dff$Main_Approach_Gapped[i] <- 1
          }
        }
        dff$Date <- substr(dff$Start, 0, 10)
        dff$Time_Start <- substr(dff$Start, 12, 19)
        dff$Time_End <- substr(dff$End, 12, 19)
        dff$Time_Start <- as_hms(dff$Time_Start)
        dff$Time_End <- as_hms(dff$Time_End)
        for (i in (1:nrow(dff))){
          dff$Time_rounded[i] <- as_hms(as.integer(dff$Time_Start[i]/(as.numeric(input$rounded_every)*60))*(as.numeric(input$rounded_every)*60))
        }
        dff$Time_rounded <- as_hms(dff$Time_rounded)
        dff_grouped2 <- dff %>% group_by(Time_rounded, Site_ID) %>% summarise(
          max_cycle_length = max(`Cycle_length`))
        dff_grouped2 <- dff_grouped2
        } else {
          dff_grouped2 <- NULL
        }
      }
      )
      do.call(rbind, files4)
    }
  })
  output$contents <- renderTable( 
    getData() 
  )
  observeEvent(input$previewData, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Creating a Heatmap....", value = 0)
      Github_Link <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Variation_Routines.csv"
      df_Events <- getData() %>% filter(getData()$Max_Value > as.numeric(input$max_value_threshold))
      Sites_vr <- readr::read_csv(Github_Link)
      df_final <- dplyr::left_join(df_Events, Sites_vr, by = c("Site_ID" = "SCATS_Site_ID"))
      df_final$Cycle_operating <- NULL
      if (nrow(df_final) > 1) {
      for (i in (1:nrow(df_final))) {
        if(df_final$cycle_length_nominal[i] > df_final$VR57_Misol_cycle_length[i]){
          df_final$Cycle_operating[i] <- "MLink"
        } else {
          df_final$Cycle_operating[i] <- "MIsol"
        }
      }
        df_final <- df_final %>% dplyr::filter(Cycle_operating=="MLink" & Phase != "A")
        df_final$hour_time <- as_hms("00:00:00")
        for (j in (1:nrow(df_final))){
          df_final$hour_time[j] <- as_hms(as.integer(as_hms(df_final$Tim[j])/(as.numeric(3600)))*(as.numeric(3600)))
        }
        df_final$hour_time <- as_hms(df_final$hour_time)
        df_grouped <- df_final %>% group_by(Site_ID, hour_time) %>% count(`Site_ID`) %>% rename (mx_value_count = n)
        df_grouped <-df_grouped[order(df_grouped$hour_time),]
        df_pivoted <- df_grouped %>%
          pivot_wider(names_from = hour_time, values_from = mx_value_count, values_fill = 0)
        
        df_pivoted
      } else {
      }
      my_palette <- colorRampPalette(c("white", "orange", "red"))(n = 100)
      output$Plot <- renderPlot({
        print(heatmap.2(as.matrix(df_pivoted[1:nrow(df_pivoted),2:ncol(df_pivoted)]),
        col=my_palette,
        Colv = NA, Rowv = NA, scale="none",
        cexRow=0.8,cexCol =0.8, labRow=paste("site#", unlist(df_pivoted[,1]),sep="")))})
      graphics.off()
    })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("data-", Sys.time(), ".csv", sep="")
    },
    content = function(file) { 
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Analyizing the data....", value = 0)
      Github_Link <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Variation_Routines.csv"
      df_Events <- getData() %>% filter(getData()$Max_Value > as.numeric(input$max_value_threshold))
      Sites_vr <- readr::read_csv(Github_Link)
      df_final <- dplyr::left_join(df_Events, Sites_vr, by = c("Site_ID" = "SCATS_Site_ID"))
      df_final$Cycle_operating <- NULL
      if (nrow(df_final) > 1) {
        for (i in (1:nrow(df_final))) {
          if(df_final$cycle_length_nominal[i] > df_final$VR57_Misol_cycle_length[i]){
            df_final$Cycle_operating[i] <- "MLink"
          } else {
            df_final$Cycle_operating[i] <- "MIsol"
          }
        }
        df_final <- df_final %>% dplyr::filter(Cycle_operating=="MLink" & Phase != "A")
        df_final$hour_time <- as_hms("00:00:00")
        for (j in (1:nrow(df_final))){
          df_final$hour_time[j] <- as_hms(as.integer(as_hms(df_final$Tim[j])/(as.numeric(3600)))*(as.numeric(3600)))
        }
        df_final$hour_time <- as_hms(df_final$hour_time)
        df_grouped <- df_final %>% group_by(Site_ID, hour_time) %>% count(`Site_ID`) %>% rename (mx_value_count = n)
        df_grouped <-df_grouped[order(df_grouped$hour_time),]
        df_pivoted <- df_grouped %>%
          pivot_wider(names_from = hour_time, values_from = mx_value_count, values_fill = 0)
        df_pivoted
      } else {
      }
      write.csv(df_pivoted, file, row.names=FALSE)
    })
  output$downloadData3 <- downloadHandler(
    filename = function() { 
      paste("DetectorAlarms-", Sys.time(), ".csv", sep="")
    },
    content = function(file) { 
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Analyizing the data....", value = 0)
      Detectors_list <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Detectors_List.csv"
      df <- readr::read_csv(input$file3$datapath)
      df_detector1 <- df %>% dplyr::filter(df$Event == "DA" & df$`+/-` != "-",!grepl("New: PB\\s*(.*?)\\s*", df$Details)) 
      df_detector1$first_detector <- NULL
      df_detector1$second_detector <- NULL
      df_detector1_add <- data.frame(matrix(, nrow=nrow(df_detector1), ncol=2))
      colnames(df_detector1_add) <- c("first_detector","second_detector")
      df_detector1 <- cbind(df_detector1,df_detector1_add)
      for (i in 1:nrow(df_detector1)){
        text1 <- gsub(".*DA*-*", "", df_detector1$Details[i])
        if(grepl("-", substr(text1, 1, 2), fixed = TRUE) == TRUE){
          df_detector1$first_detector[i] <- as.integer(substr(text1, 1, 1))
        } else if (grepl(",", substr(text1, 1, 3), fixed = TRUE) == TRUE) {
          if(is.na(as.integer(substr(text1, 1, 2))) == FALSE){
            df_detector1$first_detector[i] <- as.integer(substr(text1, 1, 2))
          } else if (is.na(as.integer(substr(text1, 1, 2))) == TRUE){
            df_detector1$first_detector[i] <- as.integer(substr(text1, 1, 1))
          }
        } else if (grepl("-", substr(text1, 1, 2), fixed = TRUE) == FALSE) {
          df_detector1$first_detector[i] <- as.integer(substr(text1, 1, 2))
        }
      }
      s <- strsplit(stringr::str_match(df_detector1$Details, "DA\\s*(.*?)\\s* ")[,2], split = ",")
      df_detector1 <- data.frame(No. = rep(df_detector1$No., sapply(s, length)),
                                 Record = rep(df_detector1$Record, sapply(s, length)) ,
                                 Date = rep(df_detector1$Date, sapply(s, length)) ,
                                 Time = rep(df_detector1$Time, sapply(s, length)),
                                 Source = rep(df_detector1$Source, sapply(s, length)),
                                 Site = rep(df_detector1$Site, sapply(s, length)),
                                 Type = rep(df_detector1$"+/-", sapply(s, length)),
                                 Event = rep(df_detector1$Event, sapply(s, length)),
                                 Mode = rep(df_detector1$Mode, sapply(s, length)),
                                 User = rep(df_detector1$User, sapply(s, length)),
                                 Details = rep(df_detector1$Details, sapply(s, length)), 
                                 Detectors = unlist(s))
      ss <- strsplit(df_detector1$Detectors, split = "-")
      unlist(ss)[[1]]
      df_detector1 <- data.frame(df_detector1,gsub("-.*", "", df_detector1$Detectors),
                                 df_detector1 <- gsub(".*-", "", df_detector1$Detectors))
      colnames(df_detector1[11]) <- "First_detector"
      df_detector1$second_detector <- NULL
      for (i in 1:nrow(df_detector1)){
        text2 <- gsub(".*-", "", df_detector1$Details[i])
        if(grepl(")", substr(text2, 1, 2), fixed = TRUE) == TRUE){
          df_detector1$second_detector[i] <- as.integer(substr(text2, 1, 1))
        } else if (grepl(")", substr(text2, 1, 2), fixed = TRUE) == FALSE) {
          df_detector1$second_detector[i] <- as.integer(substr(text2, 1, 2))
        }
      }
      names(df_detector1)[names(df_detector1)=="gsub............df_detector1.Detectors."] <- "First"
      names(df_detector1)[names(df_detector1)=="df_detector1....gsub............df_detector1.Detectors."] <- "Last"
      df_detector1$rep <- NULL
      df_detector1$rep <- as.integer(as.integer(df_detector1$Last) - as.integer(df_detector1$First) +1)
      df_detector1$unique_col <- order(df_detector1$Date, df_detector1$Time, df_detector1$First)
      v <- do.call("c", (mapply(rep, c(df_detector1$No., df_detector1$Record, df_detector1$Date, df_detector1$Time, df_detector1$Source, df_detector1$Site, df_detector1$Type, df_detector1$Event, df_detector1$Mode, df_detector1$User, df_detector1$Details, df_detector1$Detectors, df_detector1$First, df_detector1$Last, df_detector1$second_detector, df_detector1$rep, df_detector1$unique_col), df_detector1$rep)))
      v <- t(matrix(v, ncol(df_detector1), byrow=T))
      v <- as.data.frame(v)
      v <- setNames(v, names(df_detector1))
      df_detector1 <- v
      df_detector1$detector <- 0
      df_detector1$detector[1] <- as.integer(df_detector1$First[1])
      for (i in (2:nrow(df_detector1))){
        if(df_detector1$unique_col[i-1] != df_detector1$unique_col[i]){
          df_detector1$detector[i] <- as.integer(df_detector1$First[i])
        } else if (df_detector1$unique_col[i-1] == df_detector1$unique_col[i]) {
          df_detector1$detector[i] <- as.integer(df_detector1$detector[i-1]) + 1
        }
      }
      df_detector2 <- df %>% dplyr::filter(df$Event == "DA" & df$`+/-` == "-",!grepl("Cleared: PB\\s*(.*?)\\s*", df$Details))
      df_detector2$first_detector <- NULL
      df_detector2$second_detector <- NULL
      df_detector2_add <- data.frame(matrix(, nrow=nrow(df_detector2), ncol=2))
      colnames(df_detector2_add) <- c("first_detector","second_detector")
      df_detector2 <- cbind(df_detector2,df_detector2_add)
      for (i in 1:nrow(df_detector2)){
        text1 <- gsub(".*Cleared: DA*-*", "", df_detector2$Details[i])
        if(grepl("-", substr(text1, 1, 2), fixed = TRUE) == TRUE){
          df_detector2$first_detector[i] <- as.integer(substr(text1, 1, 1))
        } else if (grepl(",", substr(text1, 1, 3), fixed = TRUE) == TRUE) {
          if(is.na(as.integer(substr(text1, 1, 2))) == FALSE){
            df_detector2$first_detector[i] <- as.integer(substr(text1, 1, 2))
          } else if (is.na(as.integer(substr(text1, 1, 2))) == TRUE){
            df_detector2$first_detector[i] <- as.integer(substr(text1, 1, 1))
          }
        } else if (grepl("-", substr(text1, 1, 2), fixed = TRUE) == FALSE) {
          df_detector2$first_detector[i] <- as.integer(substr(text1, 1, 2))
        }
      }
      df_detector2$Details = substr(df_detector2$Details,1,nchar(df_detector2$Details)-1)
      s <- strsplit(sub(".*Cleared: DA", "", df_detector2$Details), split = ",")
      df_detector2 <- data.frame(No. = rep(df_detector2$No., sapply(s, length)),
                                 Record = rep(df_detector2$Record, sapply(s, length)) ,
                                 Date = rep(df_detector2$Date, sapply(s, length)) ,
                                 Time = rep(df_detector2$Time, sapply(s, length)),
                                 Source = rep(df_detector2$Source, sapply(s, length)),
                                 Site = rep(df_detector2$Site, sapply(s, length)),
                                 Type = rep(df_detector2$"+/-", sapply(s, length)),
                                 Event = rep(df_detector2$Event, sapply(s, length)),
                                 Mode = rep(df_detector2$Mode, sapply(s, length)),
                                 User = rep(df_detector2$User, sapply(s, length)),
                                 Details = rep(df_detector2$Details, sapply(s, length)), 
                                 Detectors = unlist(s))
      ss <- strsplit(df_detector2$Detectors, split = "-")
      unlist(ss)[[1]]
      df_detector2 <- data.frame(df_detector2,gsub("-.*", "", df_detector2$Detectors),
                                 df_detector2 <- gsub(".*-", "", df_detector2$Detectors))
      colnames(df_detector2[11]) <- "First_detector"
      df_detector2$second_detector <- NULL
      for (i in 1:nrow(df_detector2)){
        text2 <- gsub(".*-", "", df_detector2$Details[i])
        if(grepl(")", substr(text2, 1, 2), fixed = TRUE) == TRUE){
          df_detector2$second_detector[i] <- as.integer(substr(text2, 1, 1))
        } else if (grepl(")", substr(text2, 1, 2), fixed = TRUE) == FALSE) {
          df_detector2$second_detector[i] <- as.integer(substr(text2, 1, 2))
        }
      }
      names(df_detector2)[names(df_detector2)=="gsub............df_detector2.Detectors."] <- "First"
      names(df_detector2)[names(df_detector2)=="df_detector2....gsub............df_detector2.Detectors."] <- "Last"
      df_detector2$rep <- NULL
      df_detector2$rep <- as.integer(as.integer(df_detector2$Last) - as.integer(df_detector2$First) +1)
      df_detector2$unique_col <- order(df_detector2$Date, df_detector2$Time, df_detector2$First)
      v <- do.call("c", (mapply(rep, c(df_detector2$No., df_detector2$Record, df_detector2$Date, df_detector2$Time, df_detector2$Source, df_detector2$Site, df_detector2$Type, df_detector2$Event, df_detector2$Mode, df_detector2$User, df_detector2$Details, df_detector2$Detectors, df_detector2$First, df_detector2$Last, df_detector2$second_detector, df_detector2$rep, df_detector2$unique_col), df_detector2$rep)))
      v <- t(matrix(v, ncol(df_detector2), byrow=T))
      v <- as.data.frame(v)
      v <- setNames(v, names(df_detector2))
      df_detector2 <- v
      df_detector2$detector <- 0
      df_detector2$detector[1] <- as.integer(df_detector2$First[1])
      for (i in (2:nrow(df_detector2))){
        if(df_detector2$unique_col[i-1] != df_detector2$unique_col[i]){
          df_detector2$detector[i] <- as.integer(df_detector2$First[i])
        } else if (df_detector2$unique_col[i-1] == df_detector2$unique_col[i]) {
          df_detector2$detector[i] <- as.integer(df_detector2$detector[i-1]) + 1
        }
      }
      df_detector_result <- rbind(df_detector1,df_detector2)
      df_detector_result$Date <- as.Date(df_detector_result$Date, format =  "%d/%m/%Y")
      df_detector_result$Time <- hms::as_hms(as.integer(df_detector_result$Time))
      df_detector_result <- df_detector_result %>% mutate(ID = group_indices(df_detector_result, .dots=c("Site", "detector"))) 
      df_detector_result <- df_detector_result[with(df_detector_result, order(ID, Date, Time)),]
      df_detector_result$end_time <- 0
      df_detector_result$end_date <- 0
      for (kk in 1:nrow(df_detector_result)-1){
        print(kk)
        if((df_detector_result$Type[kk] == "+" | df_detector_result$Type[kk] == "+=")&&(df_detector_result$Type[kk+1] == "-")&&(df_detector_result$ID[kk]==df_detector_result$ID[kk+1])){
          df_detector_result$end_date[kk] = as.character(df_detector_result$Date[kk+1])
          df_detector_result$end_time[kk] = hms::as_hms(as.integer(df_detector_result$Time[kk+1]))
        }
      }
      df_detector_result$end_date <- as.Date(df_detector_result$end_date, format =  "%Y-%m-%d")
      df_detector_result$end_time <- hms::as_hms(as.integer(df_detector_result$end_time))
      max_date <- max(df_detector_result$Date)
      df_detector_result <- df_detector_result[ !duplicated(df_detector_result[, c("Site","Type","end_date","end_time","detector")], fromLast=T),]
      df_detector_result$end_time[is.na(df_detector_result$end_date)] <- hms::as_hms(as.integer(86399))
      df_detector_result$end_date[is.na(df_detector_result$end_date)] <- max_date
      df_detector_result$end_time <- hms::as_hms(as.integer(df_detector_result$end_time))
      df_detector_result <- df_detector_result %>% filter(Type != "-")
      df_detector_result <- df_detector_result[with(df_detector_result, order(ID, Date, Time)),]
      df_detector_result$duration <- 0
      df_detector_result$duration <- as.POSIXct(paste0(df_detector_result$end_date," ",hms::as_hms(df_detector_result$end_time)),tz="EST") - as.POSIXct(paste0(df_detector_result$Date," ",hms::as_hms(df_detector_result$Time)),tz="EST")
      detectorlists <- readr::read_csv(Detectors_list)
      detectorlists$Site_ID <- as.integer(detectorlists$Site_ID)
      df_detector_result$Site <- as.integer(df_detector_result$Site)
      df_detector_result$duration <- as.double(df_detector_result$duration)
      df_detector_result2 <- dplyr::left_join(df_detector_result, detectorlists, by = c("Site" = "Site_ID"))
      write.csv(df_detector_result2, file, row.names=FALSE)
     })
  output$downloadData4 <- downloadHandler(
    filename = function() { 
      paste("SM-", Sys.time(), ".csv", sep="")
    },
    content = function(file44) { 
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Analyizing the data....", value = 0)
      Route <- input$Rt44444
      if(Route == "RT1"){RT_code <- "100"}
      if(Route == "RT73"){RT_code <- "73"}
      if(Route == "RT130"){RT_code <- "130"}
      if(Route == "RT18"){RT_code <- "18"}
      df_final_heat <- NULL
      site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
      phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
      phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
      for (kk in (1:length(input$file4$datapath))){
        file <- input$file4$datapath[kk]
        df <- read.delim2(file,header = FALSE)
        df_final <- NULL
        df_tmp1 <- df
        for (i in (1:nrow(df_tmp1))){
          print(paste0(i,"row"))
          if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
            df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
            colnames(df_tmp11) <- c("V1")
            df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
            colnames(df_tmp2) <- c("V1")
            df_tmp3 <- NULL
            df_tmp3$V1 <- NULL
            for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
              print(k)
              if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
              if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
              tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
              colnames(tmp3) <- c("V1")
              df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
              colnames(df_tmp3) <- c("V1")
            }
            df_final <- as.data.frame(rbind(df_final,df_tmp3))
            colnames(df_final) <- c("V1")
          } else {
            df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
            colnames(df_final) <- c("V1")
          }
        }
        df <- df_final
        stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
        gsub("^.*?_","_","ATGAS_1121")
        Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
        Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
        subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
        RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
        substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
        substr(df[950,],18,19)
        substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
        if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
          as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
        }
        substr(df$V1[2],1,6)
        Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
        Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
        Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
        Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
        line_leng <- nchar(df[2,])
        df$Degree_Saturation <- ""
        df$Cycle_Length <- ""
        df$Required_Cycle_Length <- ""
        df$time <- ""
        df$Progression <- ""
        df$rotation <- ""
        df$Married <- ""
        df$type <- ""
        df$subsite <- ""
        df$strategic_approach <- ""
        df$notation <- ""
        df$phase <- ""
        df$phase_time <- ""
        df$Avg_DS_Phase <-""
        df$phase_A_GT <- ""
        df$phase_B_GT <- ""
        df$phase_C_GT <- ""
        df$phase_D_GT <- ""
        df$phase_E_GT <- ""
        df$phase_F_GT <- ""
        df$phase_G_GT <- ""
        df$phase_H_GT <- ""
        df$phase_I_GT <- ""
        for (i in 1:nrow(df)){
          if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
            print(paste0(i,":",nchar(df[i,])))
            df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
            df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
            df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
            df$time[i] <- as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
            df$Progression[i] <- as.double(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3+6,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4+7))[1]
            df$rotation[i] <- as.integer(substr(df[i,],line_leng-22,line_leng-20))[1]
            df$Married[i] <- as.character(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4)[1])
          } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
            type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
            df$type[i] <- type
            df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
            strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
            df$strategic_approach[i] <- strategic_approach
            
            stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
            if(type == "SA"){
              end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
            } else if (type == "LK"){
              end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
            }
            df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
            end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
            df$phase[i] <- as.character(substr(df$V1[i],17,19))
            df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
            df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
          } else if (substr(df$V1[i],1,3)=="A=<"){
            df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
            df$phase_B_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
              }
            df$phase_C_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
              }
            df$phase_D_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
              }
            df$phase_E_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
              }
            df$phase_F_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
              }
            df$phase_G_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
              }
            df$phase_H_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
              }
            df$phase_I_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
              }
          }
        }
        df$Day <- Day_name
        df$Date <- Date
        df$Date_name <- Date_name
        df$SS <- subsystem
        df$phase <- gsub('\\s+', '',df$phase)
        df1 <- readr::read_csv(phase_naming_file)
        df1$ss <- NULL
        df1 <- df1 %>% dplyr::filter(Site == Main_site)
        colnames(df1) <- c("site","phase_name","phase_number")
        df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
        df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(rotation = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(Progression = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(Married = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
        df22 <- df22 %>% tidyr::fill(rotation, .direction = "down")
        df22 <- df22 %>% tidyr::fill(Progression, .direction = "down")
        df22 <- df22 %>% tidyr::fill(Cycle_Length, .direction = "down")
        df22 <- df22 %>% tidyr::fill(time, .direction = "down")
        df22 <- df22 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
        df22 <- df22 %>% tidyr::fill(Degree_Saturation, .direction = "down")
        df22 <- df22 %>% tidyr::fill(Married, .direction = "down")
        df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
        df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
        df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
        df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
        df22 <- df22 %>% dplyr::filter(type == "")
        df22$Site <- Main_site
        df_final_heat <- rbind(df22,df_final_heat)
      }
      df_final_heat$time <- hms::as_hms(as.integer(df_final_heat$time))
      df_final_heat$Hour <- as.integer(round(df_final_heat$time/3600,0)*3600)
      df_final_heat$Degree_Saturation <- as.double(df_final_heat$Degree_Saturation)
      df_grouped_part1 <- sqldf::sqldf(paste0("select Site, Date, Hour, avg(Degree_Saturation) as [Avg_DS]
                             from df_final_heat
                             group by Site, Hour, Date"), drv="SQLite")
      df_grouped_part1$Datetime <- as.POSIXct(paste0(as.Date(df_grouped_part1$Date)," ",hms::as_hms(df_grouped_part1$Hour)), format="%Y-%m-%d %H:%M:%S", tz="EST")
      df_grouped <- df_grouped_part1
      lowerbound <- format(as.POSIXct(paste0(as.Date(min(df_grouped$Date))," ",hms::as_hms("00:00:00")), format="%Y-%m-%d %H:%M:%S", tz="EST"),"%Y-%m-%d %H:%M:%S")
      upperbound <- format(as.POSIXct(paste0(as.Date(max(df_grouped$Date))," ",hms::as_hms("23:00:00")), format="%Y-%m-%d %H:%M:%S", tz="EST"),"%Y-%m-%d %H:%M:%S")
      NoOfHours <- as.numeric(ymd_hms(upperbound) - ymd_hms(lowerbound))*24 
      datetime_list <- ymd_hms(lowerbound) + hours(0:NoOfHours)
      df_grouped2 <- as.data.frame(datetime_list)
      colnames(df_grouped2) <- c("Datetime")
      unique_sites <- as.data.frame(unique(df_final_heat$Site))
      colnames(unique_sites) <- c("Site")
      df_grouped2 <- tidyr::crossing(df_grouped2,unique_sites)
      df_grouped_part1$Datetime <- as.character(df_grouped_part1$Datetime)
      df_grouped2$Datetime <- as.character(df_grouped2$Datetime)
      df_grouped_part11 <- dplyr::left_join(df_grouped2, df_grouped_part1, by = c("Datetime" = "Datetime", "Site"="Site"))
      df_grouped_part11 <- df_grouped_part11 %>% dplyr::select(Datetime, Site, Avg_DS)
      df_grouped_part11$Avg_DS[is.na(df_grouped_part11$Avg_DS)] <- 0
      dfff <- reshape::cast(df_grouped_part11, Site ~ Datetime, fun.aggregate = mean, value ="Avg_DS")
      dfff <- as.data.frame(dfff)
      dfff <- dfff[,1:25]
      write.csv(dfff, file44, row.names=FALSE)
    })
  observeEvent(input$previewData4, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Creating a Heatmap....", value = 0)  
    Route <- input$Rt44444
      if(Route == "RT1"){RT_code <- "100"}
      if(Route == "RT73"){RT_code <- "73"}
      if(Route == "RT130"){RT_code <- "130"}
      if(Route == "RT18"){RT_code <- "18"}
      df_final_heat <- NULL
      site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
      phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
      phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
      for (kk in (1:length(input$file4$datapath))){
          file <- input$file4$datapath[kk]
          df <- read.delim2(file,header = FALSE)
          df_final <- NULL
          df_tmp1 <- df
          for (i in (1:nrow(df_tmp1))){
            print(paste0(i,"row"))
            if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
              df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
              colnames(df_tmp11) <- c("V1")
              df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
              colnames(df_tmp2) <- c("V1")
              df_tmp3 <- NULL
              df_tmp3$V1 <- NULL
              for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
                print(k)
                if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
                if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
                tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
                colnames(tmp3) <- c("V1")
                df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
                colnames(df_tmp3) <- c("V1")
              }
              df_final <- as.data.frame(rbind(df_final,df_tmp3))
              colnames(df_final) <- c("V1")
            } else {
              df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
              colnames(df_final) <- c("V1")
            }
          }
          df <- df_final
          stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
          gsub("^.*?_","_","ATGAS_1121")
          Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
          Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
          subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
          RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
          substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
          substr(df[950,],18,19)
          substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
          if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
            as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
          }
          substr(df$V1[2],1,6)
          Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
          Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
          Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
          Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
          line_leng <- nchar(df[2,])
          df$Degree_Saturation <- ""
          df$Cycle_Length <- ""
          df$Required_Cycle_Length <- ""
          df$time <- ""
          df$Progression <- ""
          df$rotation <- ""
          df$Married <- ""
          df$type <- ""
          df$subsite <- ""
          df$strategic_approach <- ""
          df$notation <- ""
          df$phase <- ""
          df$phase_time <- ""
          df$Avg_DS_Phase <-""
          df$phase_A_GT <- ""
          df$phase_B_GT <- ""
          df$phase_C_GT <- ""
          df$phase_D_GT <- ""
          df$phase_E_GT <- ""
          df$phase_F_GT <- ""
          df$phase_G_GT <- ""
          df$phase_H_GT <- ""
          df$phase_I_GT <- ""
          for (i in 1:nrow(df)){
            if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
              print(paste0(i,":",nchar(df[i,])))
              df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
              df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
              df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
              df$time[i] <- as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
              df$Progression[i] <- as.double(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3+6,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4+7))[1]
              df$rotation[i] <- as.integer(substr(df[i,],line_leng-22,line_leng-20))[1]
              df$Married[i] <- as.character(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4)[1])
            } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
              type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
              df$type[i] <- type
              df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
              strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
              df$strategic_approach[i] <- strategic_approach
              stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
              if(type == "SA"){
                end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
              } else if (type == "LK"){
                end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
              }
              df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
              end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
              df$phase[i] <- as.character(substr(df$V1[i],17,19))
              df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
              df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
            } else if (substr(df$V1[i],1,3)=="A=<"){
              df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
              df$phase_B_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
                }
              df$phase_C_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
                }
              df$phase_D_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
                }
              df$phase_E_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
                }
              df$phase_F_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
                }
              df$phase_G_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
                }
              df$phase_H_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
                }
              df$phase_I_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
                }
            }
          }
          
          df$Day <- Day_name
          df$Date <- Date
          df$Date_name <- Date_name
          df$SS <- subsystem
          df$phase <- gsub('\\s+', '',df$phase)
          df1 <- readr::read_csv(phase_naming_file)
          df1$ss <- NULL
          df1 <- df1 %>% dplyr::filter(Site == Main_site)
          colnames(df1) <- c("site","phase_name","phase_number")
          df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
          df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(rotation = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(Progression = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(Married = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
          df22 <- df22 %>% tidyr::fill(rotation, .direction = "down")
          df22 <- df22 %>% tidyr::fill(Progression, .direction = "down")
          df22 <- df22 %>% tidyr::fill(Cycle_Length, .direction = "down")
          df22 <- df22 %>% tidyr::fill(time, .direction = "down")
          df22 <- df22 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
          df22 <- df22 %>% tidyr::fill(Degree_Saturation, .direction = "down")
          df22 <- df22 %>% tidyr::fill(Married, .direction = "down")
          df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
          df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
          df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
          df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
          df22 <- df22 %>% dplyr::filter(type == "")
          df22$Site <- Main_site
          df_final_heat <- rbind(df22,df_final_heat)
        }
      df_final_heat$time <- hms::as_hms(as.integer(df_final_heat$time))
      df_final_heat$Hour <- as.integer(round(df_final_heat$time/3600,0)*3600)
      df_final_heat$Degree_Saturation <- as.double(df_final_heat$Degree_Saturation)
      df_grouped_part1 <- sqldf::sqldf(paste0("select Site, Date, Hour, avg(Degree_Saturation) as [Avg_DS]
                             from df_final_heat
                             group by Site, Hour, Date"), drv="SQLite")
      df_grouped_part1$Datetime <- as.POSIXct(paste0(as.Date(df_grouped_part1$Date)," ",hms::as_hms(df_grouped_part1$Hour)), format="%Y-%m-%d %H:%M:%S", tz="EST")
      df_grouped <- df_grouped_part1
      lowerbound <- format(as.POSIXct(paste0(as.Date(min(df_grouped$Date))," ",hms::as_hms("00:00:00")), format="%Y-%m-%d %H:%M:%S", tz="EST"),"%Y-%m-%d %H:%M:%S")
      upperbound <- format(as.POSIXct(paste0(as.Date(max(df_grouped$Date))," ",hms::as_hms("23:00:00")), format="%Y-%m-%d %H:%M:%S", tz="EST"),"%Y-%m-%d %H:%M:%S")
      NoOfHours <- as.numeric(ymd_hms(upperbound) - ymd_hms(lowerbound))*24 
      datetime_list <- ymd_hms(lowerbound) + hours(0:NoOfHours)
      df_grouped2 <- as.data.frame(datetime_list)
      colnames(df_grouped2) <- c("Datetime")
      unique_sites <- as.data.frame(unique(df_final_heat$Site))
      colnames(unique_sites) <- c("Site")
      df_grouped2 <- tidyr::crossing(df_grouped2,unique_sites)
      df_grouped_part1$Datetime <- as.character(df_grouped_part1$Datetime)
      df_grouped2$Datetime <- as.character(df_grouped2$Datetime)
      df_grouped_part11 <- dplyr::left_join(df_grouped2, df_grouped_part1, by = c("Datetime" = "Datetime", "Site"="Site"))
      df_grouped_part11 <- df_grouped_part11 %>% dplyr::select(Datetime, Site, Avg_DS)
      df_grouped_part11$Avg_DS[is.na(df_grouped_part11$Avg_DS)] <- 0
      dfff <- reshape::cast(df_grouped_part11, Site ~ Datetime, fun.aggregate = mean, value ="Avg_DS")
      dfff <- as.data.frame(dfff)
      dfff <- dfff[,1:25]
  my_palette <- colorRampPalette(c("white", "orange", "red"))(n = 100)
  output$Plot4 <- renderPlot({
    print(heatmap.2(as.matrix(dfff[1:nrow(dfff),2:ncol(dfff)]),
                    col=my_palette,
                    Colv = NA, Rowv = NA, scale="none",
                    cexRow=0.8,cexCol =0.8, labRow=paste("site#", unlist(dfff[,1]),sep="")))})
  graphics.off()
  })
  output$downloadData5 <- downloadHandler(
    filename = function() { 
      paste("SM_Phase_", Sys.time(), ".csv", sep="")
    },
    content = function(file55) { 
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Analyizing the data....", value = 0)
      df_final_heat <- NULL
      site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
      phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
      phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
      file <- input$file5$datapath[1]
      df <- read.delim2(file,header = FALSE)
      df_final <- NULL
      df_tmp1 <- df
      for (i in (1:nrow(df_tmp1))){
        print(paste0(i,"row"))
        if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
          df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
          colnames(df_tmp11) <- c("V1")
          df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
          colnames(df_tmp2) <- c("V1")
          df_tmp3 <- NULL
          df_tmp3$V1 <- NULL
          for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
            print(k)
            if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
            if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
            tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
            colnames(tmp3) <- c("V1")
            df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
            colnames(df_tmp3) <- c("V1")
          }
          df_final <- as.data.frame(rbind(df_final,df_tmp3))
          colnames(df_final) <- c("V1")
        } else {
          df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
          colnames(df_final) <- c("V1")
        }
      }
      df <- df_final
      stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
      gsub("^.*?_","_","ATGAS_1121")
      Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
      Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
      subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
      RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
      substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
      substr(df[950,],18,19)
      substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
      if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
        as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
      }
      substr(df$V1[2],1,6)
      Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
      Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
      Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
      Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
      line_leng <- nchar(df[2,])
      df$Degree_Saturation <- ""
      df$Cycle_Length <- ""
      df$Required_Cycle_Length <- ""
      df$time <- ""
      df$Progression <- ""
      df$rotation <- ""
      df$Married <- ""
      df$type <- ""
      df$subsite <- ""
      df$strategic_approach <- ""
      df$notation <- ""
      df$phase <- ""
      df$phase_time <- ""
      df$Avg_DS_Phase <-""
      df$phase_A_GT <- ""
      df$phase_B_GT <- ""
      df$phase_C_GT <- ""
      df$phase_D_GT <- ""
      df$phase_E_GT <- ""
      df$phase_F_GT <- ""
      df$phase_G_GT <- ""
      df$phase_H_GT <- ""
      df$phase_I_GT <- ""
      for (i in 1:nrow(df)){
        if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
          print(paste0(i,":",nchar(df[i,])))
          df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
          df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
          df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
          df$time[i] <- as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
          df$Progression[i] <- as.double(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3+6,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4+7))[1]
          df$rotation[i] <- as.integer(substr(df[i,],line_leng-22,line_leng-20))[1]
          df$Married[i] <- as.character(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4)[1])
        } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
          type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
          df$type[i] <- type
          df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
          strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
          df$strategic_approach[i] <- strategic_approach
          stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
          if(type == "SA"){
            end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
          } else if (type == "LK"){
            end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
          }
          df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
          end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
          df$phase[i] <- as.character(substr(df$V1[i],17,19))
          df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
          df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
        } else if (substr(df$V1[i],1,3)=="A=<"){
          df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
          df$phase_B_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
            }
          df$phase_C_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
            }
          df$phase_D_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
            }
          df$phase_E_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
            }
          df$phase_F_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
            }
          df$phase_G_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
            }
          df$phase_H_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
            }
          df$phase_I_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
            }
        }
      }
      df$Day <- Day_name
      df$Date <- Date
      df$Date_name <- Date_name
      df$SS <- subsystem
      df$phase <- gsub('\\s+', '',df$phase)
      df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
      df1 <- readr::read_csv(phase_naming_file)
      df1$ss <- NULL
      df1 <- df1 %>% dplyr::filter(Site == Main_site)
      colnames(df1) <- c("site","phase_name","phase_number")
      df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
      df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(rotation = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(Progression = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(Married = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
      df22 <- df22 %>% tidyr::fill(rotation, .direction = "down")
      df22 <- df22 %>% tidyr::fill(Progression, .direction = "down")
      df22 <- df22 %>% tidyr::fill(Cycle_Length, .direction = "down")
      df22 <- df22 %>% tidyr::fill(time, .direction = "down")
      df22 <- df22 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
      df22 <- df22 %>% tidyr::fill(Degree_Saturation, .direction = "down")
      df22 <- df22 %>% tidyr::fill(Married, .direction = "down")
      df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
      df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
      df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
      df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
      df_phase <- df22 %>% dplyr::filter(type == "SA" & subsite == Main_site)
      df_phase$time <- hms::as_hms(as.POSIXct(paste0(sprintf("%02d",as.integer(as.integer(df_phase$time)/3600)),":",sprintf("%02d",as.integer(as.integer((as.integer(df_phase$time))%%3600)/60))),format="%H:%M"))
      df_phase$time <- as.character(df_phase$time)
      df_phase$Avg_DS_Phase <- as.double(df_phase$Avg_DS_Phase)
      df_phase <- sqldf::sqldf("select a.time, a.Phase_name, 
Avg(a.Avg_DS_Phase) as [Avg_DS_by_Phase], phase_A_GT, phase_B_GT, phase_C_GT, phase_D_GT, 
phase_E_GT, phase_F_GT, phase_G_GT, phase_H_GT, phase_I_GT
from df_phase as a 
group by a.time, a.Phase_name", drv="SQLite")
      df_phase <- sqldf::sqldf("select a.time, a.Phase_name, a.Avg_DS_by_Phase, 
case when (a.Phase_name= 'A') then a.phase_A_GT
when (a.Phase_name= 'B') then a.phase_B_GT
when (a.Phase_name= 'C') then a.phase_C_GT
when (a.Phase_name= 'D') then a.phase_D_GT
when (a.Phase_name= 'E') then a.phase_E_GT
when (a.Phase_name= 'F') then a.phase_F_GT
when (a.Phase_name= 'G') then a.phase_G_GT
when (a.Phase_name= 'H') then a.phase_H_GT
when (a.Phase_name= 'I') then a.phase_I_GT
End as Phase_time
from df_phase as a", drv="SQLite")
      df_phase$DS_Green_Ratio <- as.double(as.double(df_phase$Avg_DS_by_Phase)/as.double(df_phase$Phase_time))
      phases_unique <- as.data.frame(unique(df_phase$Phase_name))
      colnames(phases_unique) <- "unique_phases"
      plot_label <- paste0("Site ",Main_site," on ",Day_name," ",Date_name)
      if(nrow(phases_unique)==2){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        df_plot <- data.frame(xdata, y1, y2)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue"))
        g <- g + ylab("Avg DS") + xlab("Time")
        g
      }
      if(nrow(phases_unique)==3){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        df_plot <- data.frame(xdata, y1, y2, y3)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen"))
        g <- g + ylab("Avg DS") + xlab("Time")
        g
      }
      if(nrow(phases_unique)==4){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        y4 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[4]]
        df_plot <- data.frame(xdata, y1, y2, y3, y4)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange"))
        g <- g + ylab("Avg DS") + xlab("Time")
        g
      }
      if(nrow(phases_unique)==5){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        y4 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[4]]
        y5 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[5]]
        df_plot <- data.frame(xdata, y1, y2, y3, y4, y5)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
        g <- g + geom_line(aes(y=y5, color = paste0("Phase ",phases_unique$unique_phases[5])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange","cyan"))
        g <- g + ylab("Avg DS") + xlab("Time")
        g
      }
      write.csv(df_phase, file55, row.names=FALSE)
    })
  observeEvent(input$previewData5, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Creating a Plot....", value = 0)  
    df_final_heat <- NULL
    site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
    phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
    phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
    file <- input$file5$datapath[1]
    df <- read.delim2(file,header = FALSE)
    df_final <- NULL
    df_tmp1 <- df
    for (i in (1:nrow(df_tmp1))){
      print(paste0(i,"row"))
      if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
        df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        colnames(df_tmp11) <- c("V1")
        df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
        colnames(df_tmp2) <- c("V1")
        df_tmp3 <- NULL
        df_tmp3$V1 <- NULL
        for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
          print(k)
          if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
          if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
          tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
          colnames(tmp3) <- c("V1")
          df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
          colnames(df_tmp3) <- c("V1")
        }
        df_final <- as.data.frame(rbind(df_final,df_tmp3))
        colnames(df_final) <- c("V1")
      } else {
        df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
        colnames(df_final) <- c("V1")
      }
    }
    df <- df_final
    stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
    gsub("^.*?_","_","ATGAS_1121")
    Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
    Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
    subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
    RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    substr(df[950,],18,19)
    substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
    if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
      as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
    }
    substr(df$V1[2],1,6)
    Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
    Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
    Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
    Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
    line_leng <- nchar(df[2,])
    df$Degree_Saturation <- ""
    df$Cycle_Length <- ""
    df$Required_Cycle_Length <- ""
    df$time <- ""
    df$Progression <- ""
    df$rotation <- ""
    df$Married <- ""
    df$type <- ""
    df$subsite <- ""
    df$strategic_approach <- ""
    df$notation <- ""
    df$phase <- ""
    df$phase_time <- ""
    df$Avg_DS_Phase <-""
    df$phase_A_GT <- ""
    df$phase_B_GT <- ""
    df$phase_C_GT <- ""
    df$phase_D_GT <- ""
    df$phase_E_GT <- ""
    df$phase_F_GT <- ""
    df$phase_G_GT <- ""
    df$phase_H_GT <- ""
    df$phase_I_GT <- ""
    for (i in 1:nrow(df)){
      if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
        print(paste0(i,":",nchar(df[i,])))
        df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
        df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
        df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
        df$time[i] <- as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
        df$Progression[i] <- as.double(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3+6,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4+7))[1]
        df$rotation[i] <- as.integer(substr(df[i,],line_leng-22,line_leng-20))[1]
        df$Married[i] <- as.character(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4)[1])
      } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
        type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
        df$type[i] <- type
        df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
        strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
        df$strategic_approach[i] <- strategic_approach
        stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
        if(type == "SA"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
        } else if (type == "LK"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
        }
        df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
        end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
        df$phase[i] <- as.character(substr(df$V1[i],17,19))
        df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
        df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
      } else if (substr(df$V1[i],1,3)=="A=<"){
        df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
        df$phase_B_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
          }
        df$phase_C_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
          }
        df$phase_D_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
          }
        df$phase_E_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
          }
        df$phase_F_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
          }
        df$phase_G_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
          }
        df$phase_H_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
          }
        df$phase_I_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
          }
      }
    }
    df$Day <- Day_name
    df$Date <- Date
    df$Date_name <- Date_name
    df$SS <- subsystem
    df$phase <- gsub('\\s+', '',df$phase)
    df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
    df1 <- readr::read_csv(phase_naming_file)
    df1$ss <- NULL
    df1 <- df1 %>% dplyr::filter(Site == Main_site)
    colnames(df1) <- c("site","phase_name","phase_number")
    df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
    df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(rotation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Progression = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Married = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
    df22 <- df22 %>% tidyr::fill(rotation, .direction = "down")
    df22 <- df22 %>% tidyr::fill(Progression, .direction = "down")
    df22 <- df22 %>% tidyr::fill(Cycle_Length, .direction = "down")
    df22 <- df22 %>% tidyr::fill(time, .direction = "down")
    df22 <- df22 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
    df22 <- df22 %>% tidyr::fill(Degree_Saturation, .direction = "down")
    df22 <- df22 %>% tidyr::fill(Married, .direction = "down")
    df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
    df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
    df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
    df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
    df_phase <- df22 %>% dplyr::filter(type == "SA" & subsite == Main_site)
    df_phase$time <- hms::as_hms(as.POSIXct(paste0(sprintf("%02d",as.integer(as.integer(df_phase$time)/3600)),":",sprintf("%02d",as.integer(as.integer((as.integer(df_phase$time))%%3600)/60))),format="%H:%M"))
    df_phase$time <- as.character(df_phase$time)
    df_phase$Avg_DS_Phase <- as.double(df_phase$Avg_DS_Phase)
    sites_direction <- readr::read_csv("https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv")
    sites_direction <- sites_direction %>%
      tidyr::pivot_longer(!`Signal ID`, names_to = "direction", values_to = "SG")
    colnames(sites_direction) <- c("site","direction","SG")
    sites_direction <- na.omit(sites_direction)
    sites_direction_onesite <- sites_direction %>% dplyr::filter(site == Main_site)
        df_sg <- sqldf::sqldf("select a.time, a.Phase_name, a.phase,
Avg(a.Avg_DS_Phase) as [Avg_DS_by_SG], phase_A_GT, phase_B_GT, phase_C_GT, phase_D_GT, 
phase_E_GT, phase_F_GT, phase_G_GT, phase_H_GT, phase_I_GT
from df_phase as a 
group by a.time, a.Phase_name, a.phase", drv="SQLite")
    df_sg <- sqldf::sqldf("select a.time, a.Phase_name, a.phase, a.Avg_DS_by_SG, 
case when (a.Phase_name= 'A') then cast(a.phase_A_GT as int)
when (a.Phase_name= 'B') then cast(a.phase_B_GT as int)
when (a.Phase_name= 'C') then cast(a.phase_C_GT as int)
when (a.Phase_name= 'D') then cast(a.phase_D_GT as int)
when (a.Phase_name= 'E') then cast(a.phase_E_GT as int)
when (a.Phase_name= 'F') then cast(a.phase_F_GT as int)
when (a.Phase_name= 'G') then cast(a.phase_G_GT as int)
when (a.Phase_name= 'H') then cast(a.phase_H_GT as int)
when (a.Phase_name= 'I') then cast(a.phase_I_GT as int)
End as Phase_time
from df_sg as a", drv="SQLite")
    sg_unique <- as.data.frame(unique(df_sg$phase))
    colnames(sg_unique) <- "unique_sg"
    sites_direction_onesite <- aggregate(data=sites_direction_onesite, direction~site+SG,FUN=paste,collapse = ",")
    df_phase <- sqldf::sqldf("select a.time, a.Phase_name, 
Avg(a.Avg_DS_Phase) as [Avg_DS_by_Phase], phase_A_GT, phase_B_GT, phase_C_GT, phase_D_GT, 
phase_E_GT, phase_F_GT, phase_G_GT, phase_H_GT, phase_I_GT
from df_phase as a 
group by a.time, a.Phase_name", drv="SQLite")
    df_phase <- sqldf::sqldf("select a.time, a.Phase_name, a.Avg_DS_by_Phase, 
case when (a.Phase_name= 'A') then cast(a.phase_A_GT as int)
when (a.Phase_name= 'B') then cast(a.phase_B_GT as int)
when (a.Phase_name= 'C') then cast(a.phase_C_GT as int)
when (a.Phase_name= 'D') then cast(a.phase_D_GT as int)
when (a.Phase_name= 'E') then cast(a.phase_E_GT as int)
when (a.Phase_name= 'F') then cast(a.phase_F_GT as int)
when (a.Phase_name= 'G') then cast(a.phase_G_GT as int)
when (a.Phase_name= 'H') then cast(a.phase_H_GT as int)
when (a.Phase_name= 'I') then cast(a.phase_I_GT as int)
End as Phase_time
from df_phase as a", drv="SQLite")
    phases_unique <- as.data.frame(unique(df_phase$Phase_name))
    colnames(phases_unique) <- "unique_phases"
    df_sg$phase <- as.integer(df_sg$phase)
     df_sg$DS_Green_Ratio <- as.double(df_sg$Avg_DS_by_SG/df_sg$Phase_time)
    if (input$degree_saturation_analysis_type == "Two Axis"){
      if(input$degree_saturation_analysis_type2 == "Signal Groups"){
        if (1 %in% sg_unique$unique_sg){
          SG_number <- 1
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name1 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          y11 <- df_sg3$Phase_time
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name1))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g1 <- g
        } else { g1 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)} #optional, but safer in case another theme is applied later}
        if (2 %in% sg_unique$unique_sg){
          SG_number <- 2
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          y11 <- df_sg3$Phase_time
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g2 <- g
        } else { g2 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (3 %in% sg_unique$unique_sg){
          SG_number <- 3
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name3 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          y11 <- df_sg3$Phase_time
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name3))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g3 <- g
        } else { g3 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (4 %in% sg_unique$unique_sg){
          SG_number <- 4
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name4 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          y11 <- df_sg3$Phase_time
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name4))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g4 <- g
        } else { g4 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (5 %in% sg_unique$unique_sg){
          SG_number <- 5
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name5 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          y11 <- df_sg3$Phase_time
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name5))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g5 <- g
        } else { g5 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (6 %in% sg_unique$unique_sg){
          SG_number <- 6
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name6 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          y11 <- df_sg3$Phase_time
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name6))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g6 <- g
        } else { g6 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (7 %in% sg_unique$unique_sg){
          SG_number <- 7
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name7 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          y11 <- df_sg3$Phase_time
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name7))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g7 <- g
        } else { g7 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (8 %in% sg_unique$unique_sg){
          SG_number <- 8
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name8 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          y11 <- df_sg3$Phase_time
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name8))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g8 <- g
        } else { g8 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (9 %in% sg_unique$unique_sg){
          SG_number <- 9
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name9 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          y11 <- df_sg3$Phase_time
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name9))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g9 <- g
        } else { g9 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        output$Plot5 <- renderPlot({
          p1 <- g1  
          p2 <- g2
          p3 <- g3
          p4 <- g4
          p5 <- g5
          p6 <- g6
          p7 <- g7
          p8 <- g8
          p9 <- g9
          grid.arrange(grobs = list(p1,p2,p3,p4,p5,p6,p7,p8,p9), ncol=1, top = paste0("Site ",Main_site," on ",Day_name," ",Date_name), nrow = 9, byrow = TRUE)})
      } else if(input$degree_saturation_analysis_type2 == "Phases"){
        plot_label <- paste0("Site ",Main_site," on ",Day_name," ",Date_name)
        if(nrow(phases_unique)==2){
          xdata <- hms::as_hms(unique(df_phase$time))
          y1 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          df_plot <- data.frame(xdata, y1, y2)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label)
          g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
          g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue"))
          g <- g + ylab("Avg DS") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g
          output$Plot5_2 <- renderPlot({ g })
        }
        if(nrow(phases_unique)==3){
          xdata <- hms::as_hms(unique(df_phase$time))
          y1 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y3 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[3]]
          y11 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y33 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[3]]
          df_plot <- data.frame(xdata, y1, y2, y3)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label)
          g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
          g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y33),linetype = "dotted", size=1, color="darkgreen") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen"))
          g <- g + ylab("Average Degree of Saturation %") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g
          output$Plot5_2 <- renderPlot({ g})
        }
        if(nrow(phases_unique)==4){
          xdata <- hms::as_hms(unique(df_phase$time))
          y1 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y3 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[3]]
          y4 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[4]]
          y11 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y33 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[3]]
          y44 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[4]]
          df_plot <- data.frame(xdata, y1, y2, y3, y4)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label)
          g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
          g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
          g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y33),linetype = "dotted", size=1, color="darkgreen") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y44),linetype = "dotted", size=1, color="orange") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange"))
          g <- g + ylab("Avg DS") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g
          output$Plot5_2 <- renderPlot({ g})
        }
        if(nrow(phases_unique)==5){
          xdata <- hms::as_hms(unique(df_phase$time))
          y1 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y3 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[3]]
          y4 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[4]]
          y5 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[5]]
          y11 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y33 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[3]]
          y44 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[4]]
          y55 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[5]]
          df_plot <- data.frame(xdata, y1, y2, y3, y4, y5)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label)
          g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
          g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
          g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
          g <- g + geom_line(aes(y=y5, color = paste0("Phase ",phases_unique$unique_phases[5])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y33),linetype = "dotted", size=1, color="darkgreen") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y44),linetype = "dotted", size=1, color="orange") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y55),linetype = "dotted", size=1, color="cyan") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange","cyan"))
          g <- g + ylab("Avg DS") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g
          output$Plot5_2 <- renderPlot({ g})
        }
      }
    }
    else if (input$degree_saturation_analysis_type == "Ratio"){
      if(input$degree_saturation_analysis_type2 == "Signal Groups"){
        if (1 %in% sg_unique$unique_sg){
          SG_number <- 1
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g1 <- g
        } else { g1 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (2 %in% sg_unique$unique_sg){
          SG_number <- 2
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g2 <- g
        } else { g2 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (3 %in% sg_unique$unique_sg){
          SG_number <- 3
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g3 <- g
        } else { g3 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (4 %in% sg_unique$unique_sg){
          SG_number <- 4
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g4 <- g
        } else { g4 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (5 %in% sg_unique$unique_sg){
          SG_number <- 5
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g5 <- g
        } else { g5 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)} 
        if (6 %in% sg_unique$unique_sg){
          SG_number <- 6
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g6 <- g
        } else { g6 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (7 %in% sg_unique$unique_sg){
          SG_number <- 7
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g7 <- g
        } else { g7 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (8 %in% sg_unique$unique_sg){
          SG_number <- 8
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g8 <- g
        } else { g8 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (9 %in% sg_unique$unique_sg){
          SG_number <- 9
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g9 <- g
        } else { g9 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        output$Plot5 <- renderPlot({
          p1 <- g1  
          p2 <- g2
          p3 <- g3
          p4 <- g4
          p5 <- g5
          p6 <- g6
          p7 <- g7
          p8 <- g8
          p9 <- g9
          grid.arrange(grobs = list(p1,p2,p3,p4,p5,p6,p7,p8,p9), ncol=1, top = paste0("Site ",Main_site," on ",Day_name," ",Date_name), nrow = 9, byrow = TRUE)})
      } else if(input$degree_saturation_analysis_type2 == "Phases"){
      df_phase$DS_Green_Ratio <- as.double(as.double(df_phase$Avg_DS_by_Phase)/as.double(df_phase$Phase_time))
      plot_label <- paste0("Site ",Main_site," on ",Day_name," ",Date_name)
      if(nrow(phases_unique)==2){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        df_plot <- data.frame(xdata, y1, y2)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue"))
        g <- g + ylab("Avg DS") + xlab("Time")
        g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
        g <- g + theme_calc()
        g
        output$Plot5_2 <- renderPlot({ g})
      }
      if(nrow(phases_unique)==3){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        df_plot <- data.frame(xdata, y1, y2, y3)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen"))
        g <- g + ylab("Avg DS") + xlab("Time")
        g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
        g <- g + theme_calc()
        g
        output$Plot5_2 <- renderPlot({ g})
      }
      if(nrow(phases_unique)==4){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        y4 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[4]]
        df_plot <- data.frame(xdata, y1, y2, y3, y4)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange"))
        g <- g + ylab("Avg DS") + xlab("Time")
        g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
        g <- g + theme_calc()
        g
        output$Plot5_2 <- renderPlot({ g})
      }
      if(nrow(phases_unique)==5){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        y4 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[4]]
        y5 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[5]]
        df_plot <- data.frame(xdata, y1, y2, y3, y4, y5)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
        g <- g + geom_line(aes(y=y5, color = paste0("Phase ",phases_unique$unique_phases[5])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange","cyan"))
        g <- g + ylab("Avg DS") + xlab("Time")
        g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
        g <- g + theme_calc()
        g
        output$Plot5_2 <- renderPlot({ g})
      }
      }
    }
  })

 
}

shinyApp(ui = ui, server = server)
