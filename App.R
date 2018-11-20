library(RColorBrewer)
library(Cairo)
library(rhandsontable)
library(stringr)
library(gtools)
library(plyr)
library(shiny)
library(ggplot2)
library(shinyBS)
library(scales)
library(shinythemes)
library(DT)

ui <- fluidPage(theme = shinytheme("flatly"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Home",
        fluidRow(
          column(12, align="center", span(htmlOutput(outputId = "homeTitle"), style="font-size: 100px"))
        ),
        fluidRow(
          column(12, align="center",
              actionButton("goToCreatePatient", "Create New Patient", icon("plus"),
              style="color: #fff; background-color: #900C3F; border-color: #900C3F; height: 80px;
                        width: 320px; font-size:120%")
          )
        ),
        br(),
        fluidRow(
          column(12, align="center",
              actionButton("goToUploadPatient", "Upload Patient CSV", icon("upload"),
              style="color: #fff; background-color: #1E45B0; border-color: #1E45B0; height: 80px;
                        width: 320px; font-size:120%"))
        ),
        br(),
        fluidRow(
          column(12, align="center",
            actionButton("goToHelp", "Help / FAQ", icon("question"),
              style="color: #fff; background-color: MediumSeaGreen; border-color: MediumSeaGreen; height: 80px;
                        width: 320px; font-size:120%"))
        ),
        br(),
        fluidRow(
          column(12, align="center",
            actionButton("goToSettings", "Application Settings", icon("cogs"),
              style="color: #fff; background-color: SlateBlue; border-color: SlateBlue; height: 80px;
                        width: 320px; font-size:120%"))
        )
      ),
    navbarMenu("Upload / Create Patient",
        tabPanel("Create New Patient",
          headerPanel("Create New Patient"),
          rHandsontableOutput("HOTNewPatient"),
          span(htmlOutput(outputId = "infusionNote"), style="font-size: 10px"),
          column(4, actionButton("submitNewPatient", "Submit Patient", icon("user"),
          style="color: #fff; background-color: #16A085; border-color: #16A085; height: 60px;
                    width: 240px; font-size:120%"))
        ),
        tabPanel("Upload Patient CSV",
            headerPanel("Upload Patient CSV"),
            fluidRow(
              column(6,
                tags$div(title="Upload existing patient file",
                    fileInput(inputId = "patientFileCSV", label = "Load Existing CSV File", accept = ".csv", placeholder = "No file selected.")
                  )
                )
              )
          )
        ),
    navbarMenu("Edit Patient Data",
      tabPanel("Add Point",
          #tags$head(tags$script(src = "message-handler.js")),
          headerPanel("Add Point"),
          fluidRow(
            column(9, offset = 1, span(uiOutput(outputId = "addPointText"), style="font-size: 22px; color:black;"))
            ),
          br(),
          fluidRow(
            column(2, offset = 1, numericInput(inputId = "addPointMTXLevel", "MTX Level (uM):", value = 0, min = 0, max = NA, step = NA, width = NA)),
            column(2, dateInput(inputId = "addPointDate", label = "Date:")),
            column(2, textInput(inputId = "addPointTime", label = "Time(0:00-24:00):", value = format(Sys.time(), format="%H:%M"))),
            column(2, numericInput(inputId = "addPointCycleToAdd", label = "Cycle Num:", value = 1, min = 1)),
            column(2, checkboxInput(inputId = "addPointShowCycle", label = "Show Cycle?", value = TRUE))

            ),
            fluidRow(
              column(12, plotOutput("plotsingle_plot"))
            ),
            fluidRow(
              column(6, offset = 1, actionButton("addPointButton", "Add Point To Cycle 1", icon("plus-circle"),
              style="color: #fff; background-color: #7D3C98; border-color: #7D3C98; height: 60px;
                      width: 270px; font-size:120%"))
                    ),
            br(),
            fluidRow(
              column(2, offset = 1, selectInput(inputId = "longsliderinput", label = "Max Hours Shown", c(120,160,200,240), selected = 120)),
              column(2, selectInput(inputId = "colorType", label = "Color Scheme:",
                        c("Black" = "varBLACK",
                          "Basic RGB" = "varA",
                          "Blue" = "varB",
                          "Red" = "varC",
                          "Purple" = "varD",
                          "Green" = "varE"
                        ),
                        selected = "varA",
                        multiple = FALSE)),
              column(3, checkboxInput(inputId = "showInfusionTimeAddPoint", label = "Show Infusion Point?", value = F))
            )#,
            #bsModal("addPointConfirmation", "Add Point Confirmation:", "", size = "large",
            #  span(htmlOutput(outputId = "addPointInfusionData"), style="font-size: 18px"),
            #  span(htmlOutput(outputId = "addPointNewHeader"), style="font-size: 18px"),
            #  tableOutput("addPointTableConfirmation"),
            #  actionButton("confirmAddPoint", "Yes", icon("check"), class = "confirm"),
            #  actionButton("cancelAddPoint", "No", icon("times"), class = "cancel"))
      ),
      tabPanel("Delete Point",
        headerPanel("Delete Point"),
        column(3, numericInput("deleteCycleChoice", "Cycle Number:", value = 1, min = 1)),
        column(12, dataTableOutput("deleteCycleDataTable")),
        column(3, actionButton('deletePoints', 'Delete Selected Point(s)', icon("trash"), class = "delete"))#,
        #bsModal("deletePointConfirmation", "Delete Point(s) Confirmation:", "", size = "large",
        #  tableOutput("deletePointSelected"),
        #  actionButton("confirmDeletePoint", "Yes", icon("check"), class = "confirm"),
        #  actionButton("cancelDeletePoint", "No", icon("times"), class = "cancel"))
      ),
      tabPanel("Add Cycle",
        headerPanel("Add Cycle"),
        fluidRow(
          column(9, offset = 1, span(uiOutput(outputId = "addCycleText"), style="font-size: 22px; color:black;"))
          ),
        br(),
        fluidRow(
          column(3, offset = 1, numericInput(inputId = "addCycleMTXLevel", "Infusion MTX Level (uM):", value = 0, min = 0, max = NA, step = NA, width = NA)),
          column(3, dateInput(inputId = "addCycleDate", label = "Infusion Date:")),
          column(3, textInput(inputId = "addCycleTime", label = "Infusion Time(0:00-24:00):", value = format(Sys.time(), format="%H:%M")))
        ),
        fluidRow(
          column(3, offset = 1, textInput(inputId = "addCycleDay", "Day:")),
          column(3, textInput(inputId = "addCycleMTXMg", label = "MTX (mg/m2):")),
          column(3, textInput(inputId = "addCycleMTXTotal", label = "Total MTX:"))
        ),
        fluidRow(
          column(6, offset = 1, actionButton("addCycleButton", "Add Cycle", icon("plus-circle"),
          style="color: #fff; background-color: #199F19; border-color: #199F19; height: 60px;
                  width: 270px; font-size:120%"))
        )#,
        #bsModal("addCycleConfirmation", "Add Cycle Confirmation:", "", size = "large",
        #  span(htmlOutput(outputId = "addCycleInfusionData"), style="font-size: 18px"),
        #  actionButton("confirmAddCycle", "Yes", icon("check"), class = "confirm"),
        #  actionButton("cancelAddCycle", "No", icon("times"), class = "cancel"))
      ),
      tabPanel("Delete Cycle",
        headerPanel("Delete Cycle"),
        column(12, dataTableOutput("deleteCycleDT")),
        column(3, actionButton('deleteCycle', 'Delete Selected Cycle(s)', icon("trash"), class = "delete"))#,
        #bsModal("deleteCycleConfirmation", "Delete Cycle(s) Confirmation:", "", size = "large",
        #  span(htmlOutput(outputId = "deleteCycleInformation"), style="font-size: 18px"),
        #  tags$hr(),
        #  actionButton("confirmDeleteCycle", "Yes", icon("check"), class = "confirm"),
        #  actionButton("cancelDeleteCycle", "No", icon("times"), class = "cancel"))
      )
      ),
    navbarMenu("View / Download Data",
      tabPanel("View Patient Nomogram",
            headerPanel("View Patient Nomogram"),
              tabsetPanel(
                tabPanel("Show Individual Cycles",
                  fluidRow(
                    column(9, offset = 1, span(textOutput(outputId = "singleCycleInstructionHeader"), style="font-size: 22px; color:black;"))
                    ),
                  fluidRow(
                    column(9, offset = 1, span(htmlOutput(outputId = "singleCycleInstructionBody"), style="font-size: 14px"))
                  ),
                  br(),
                  fluidRow(
                    column(2, offset = 1, numericInput(inputId ="cycleChoiceSingle", label = "Cycle To View:", value = 1, min = 1)),
                    column(2, checkboxInput("numberPoints", "Show Point Numbers:", value = T)),
                    column(2, checkboxInput("showInfusionTimeSingleCycle", "Show Infusion Point:", value = F)),
                    column(2, checkboxInput("highlightSingle", "Highlight Point Numbers:", value = F))
                  ),
                    column(12, plotOutput("singlecycle_graph")),
                    column(12, dataTableOutput("cycleDataTable")),
                    fluidRow(
                      column(3, selectInput(inputId = "colorPickSingle", label = "Color Scheme:",
                                c("Basic" = "basic",
                                  "Extreme" = "extreme",
                                  "Shaded" = "shaded"
                                ),
                                selected = "basic",
                                multiple = FALSE))
                      )
                ),
                tabPanel("Show All Cycles",
                  column(12, plotOutput("csvtab_graph")),
                  column(12, uiOutput("cycleCheckboxes")),
                  fluidRow(
                    column(3, selectInput(inputId = "colorType2", label = "Color Scheme:",
                              c("Black" = "varBLACK",
                                "Basic RGB" = "varA",
                                "Blue" = "varB",
                                "Red" = "varC",
                                "Purple" = "varD",
                                "Green" = "varE"
                              ),
                              selected = "varA",
                              multiple = FALSE)),
                    column(2, checkboxInput("showInfusionTimeMultipleCycles", "Show Infusion Point:", value = F))
                    )
                )
              ),
            fluidRow(
              column(3, sliderInput(inputId = "longsliderinput2", label = "Max Hours Shown", min = 120, max = 240, value = 120, step = 40))
            )
          ),
      tabPanel("View Patient Summary",
          headerPanel("View Patient Summary"),
          span(htmlOutput(outputId = "patientDataNote"), style="font-size: 15px"),
          tags$hr(),
          fluidRow(
            tableOutput("testTable")
          )
      ),
      tabPanel("Download Data",
          headerPanel("Download Patient Data"),
          fluidRow(
            column(3, actionButton('checkDownloadData', 'Download as CSV', icon("arrow-down"), class = "butt"),
              tags$head(tags$style(".butt{height: 45px; width: 270px; font-size:120%;} .butt{background-color:#F5B041; border-color:#F5B041} .butt{color: #fff;}")),
              tags$head(tags$style(".download{height: 45px; width: 270px; font-size:120%;} .download{background-color:#19CC66;} .download{color: #fff;}")),
              tags$head(tags$style(".confirm{height: 45px; width: 270px; font-size:120%;} .confirm{background-color:#19CC66;} .confirm{color: #fff;}")),
              tags$head(tags$style(".delete{height: 45px; width: 270px; font-size:120%;} .delete{background-color:#FF5F00; border-color: #FF5F00;} .delete{color: #fff;}")),
              tags$head(tags$style(".cancel{height: 45px; width: 270px; font-size:120%;} .cancel{background-color:#F54E41;} .cancel{color: #fff;}")))
          ),
          bsModal("modalDownload", "Confirm Download:", "", size = "large",
            span(htmlOutput(outputId = "patientInfoText"), style="font-size: 18px"),
            span(htmlOutput(outputId = "cycleInfoText"), style="font-size: 16px"),
            downloadButton("downloadData", "Yes", class = "download"),
            actionButton("cancelDownload", "No", icon("times"), class = "cancel")
          )#,
          #bsModal("modalError", "Error:", "", size = "large",
          #  span(htmlOutput(outputId = "downloadError"), style="font-size: 18px"))
      )
      ),
    navbarMenu("Settings / Help",
        tabPanel("Settings",
          headerPanel("App Settings")
        ),
        tabPanel("Help / FAQ",
          headerPanel("Help / FAQ"),
          span(htmlOutput(outputId = "helpNote"), style="font-size: 18px")
        )
    )
  )
) #end_ui

server <- function(session, input, output) {
  #Values:
  values <- reactiveValues()

  #Reactive Inputs:
  output$cycleCheckboxes <- renderUI({
    shiny::validate(
      need(!is.null(values$firstName), message=FALSE)
      )
    numIndividuals <- as.integer(values$numCycles)
    lapply(1:numIndividuals, function(i) {
      checkboxInput(inputId = paste0("showCycle", i), label = paste("Cycle ", i, " | Infusion: ", getCycleInfusion(i), sep=""), value=TRUE)
    })
  })

  #Observe / Observe Events:
  observe({ req(input$addPointCycleToAdd)
    if(!is.null(values$numCycles) && 1 <= input$addPointCycleToAdd && input$addPointCycleToAdd <= values$numCycles)
    {
      updateActionButton(session, "addPointButton",
        label = paste0("Add Point To Cycle ", input$addPointCycleToAdd),
        icon("plus-circle"))
    }
    else{
      updateActionButton(session, "addPointButton",
        label = paste0("Invalid Cycle Selection"),
        icon("times"))
    }
    })
  observe({
      updateActionButton(session, "addCycleButton",
        label = paste0("Create New Cycle"),
        icon("plus-circle"))
    })
  observeEvent(input$patientFileCSV, {
        loadOldPatientFromFile()
        updateTabsetPanel(session = session, inputId = "tabs", selected = "View Patient Nomogram")
   })
  observeEvent(input$goToCreatePatient, {
        updateTabsetPanel(session = session, inputId = "tabs", selected = "Create New Patient")
    })
  observeEvent(input$goToUploadPatient, {
        updateTabsetPanel(session = session, inputId = "tabs", selected = "Upload Patient CSV")
    })
  observeEvent(input$goToHelp, {
        updateTabsetPanel(session = session, inputId = "tabs", selected = "Help / FAQ")
   })
  observeEvent(input$goToSettings, {
        updateTabsetPanel(session = session, inputId = "tabs", selected = "Settings")
    })
  observeEvent(input$submitNewPatient, {
    isComplete <- checkNewPatientInput()

    if(isComplete)
    {
      DF <- getNewPatientData()
      DF <- turnNewIntoDF(DF)
      loadFromDF(DF)
      updateTabsetPanel(session = session, inputId = "tabs", selected = "View Patient Nomogram")
    }
    else
    {
      showModal(modalDialog(
        title = "Patient Creation Error:",
        "Please check that ALL fields are filled with valid information and resubmit.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("CLOSE")
        )
      ))
    }

    })
  observeEvent(input$cancelDownload, {
    toggleModal(session, "modalDownload", toggle = "close")
    })
  observeEvent(input$checkDownloadData, {
    if(is.null(values$firstName))
    {
      toggleModal(session, modalId = "modalError", toggle = "open")
    }
    else{
      toggleModal(session, modalId = "modalDownload", toggle = "open")
    }

    })
  observeEvent(input$addPointButton, {
    if(!is.null(values$numCycles) & input$addPointCycleToAdd <= values$numCycles & input$addPointCycleToAdd > 0 & isValidTime(input$addPointTime))
    {
      #Check if unique...
      DF <- getLineToAdd()
      DF[1,5] <- input$addPointCycleToAdd
      if(!checkForDuplicates(DF))
      {
        showModal(modalDialog(
          title = "Duplicate Data Found:",
          "Found existing duplicate data point. Check data for point to add again."))
      }
      else
      {
        toggleModal(session, modalId = "addPointConfirmation", toggle = "open")
      }
    }
    else {
      showModal(modalDialog(
        title = "ERROR:",
        "Please ensure that a patient has been uploaded / created and that the cycle exists. If attempting to add to a new cycle, visit the 'Add Cycle' tab first to add infusion data."))
    }

    })
  observeEvent(input$cancelAddPoint, {
    toggleModal(session, "addPointConfirmation", toggle = "close")
    })
  observeEvent(input$confirmAddPoint, {
    toggleModal(session, "addPointConfirmation", toggle = "close")
    addPoint()
    showNotification(paste0("Point successfully added for patient ", values$firstName, " ", values$lastName, " in cycle ", input$addPointCycleToAdd, "."), duration = 4)
    })
  observeEvent(input$deletePoints, {
    if(!is.null(values$numCycles) & input$deleteCycleChoice <= values$numCycles & input$deleteCycleChoice > 0)
    {
      if(!is.null(getRowsToDelete()))
      {
        toggleModal(session, modalId = "deletePointConfirmation", toggle = "open")
      }
      else {
        showModal(modalDialog(
          title = "ERROR:",
          "Please select at least one row to delete."))
      }
    }
    else {
      showModal(modalDialog(
        title = "ERROR:",
        "Please select a valid cycle number."))
    }
   })
  observeEvent(input$cancelDeletePoint, {
    toggleModal(session, "deletePointConfirmation", toggle = "close")
  })
  observeEvent(input$confirmDeletePoint, {
    toggleModal(session, "deletePointConfirmation", toggle = "close")
    deletePoint()
    showNotification(paste0("Point successfully deleted for patient ", values$firstName, " ", values$lastName, " in cycle ", input$addPointCycleToAdd, "."), duration = 4)
    })
  observeEvent(input$addCycleButton, {
    if(!is.null(values$numCycles) && isValidTime(input$addCycleTime))
    {
      toggleModal(session, modalId = "addCycleConfirmation", toggle = "open")
    }
    else
    {
      showModal(modalDialog(title = "ERROR:",
        "Please create a patient or upload one before attempting to delete."))
    }
  })
  observeEvent(input$confirmAddCycle, {
    toggleModal(session, "addCycleConfirmation", toggle = "close")
    addCycle()
    showNotification(paste0("New cycle added for patient ", values$firstName, " ", values$lastName, "."), duration = 4)
   })
  observeEvent(input$cancelAddCycle, {
    toggleModal(session, "addCycleConfirmation", toggle = "close")
   })
  observeEvent(input$deleteCycle, {
      if(!is.null(getCycleRowsToDelete()))
      {
        toggleModal(session, modalId = "deleteCycleConfirmation", toggle = "open")
      }
      else {
        showModal(modalDialog(
          title = "ERROR:",
          "Please select at least one cycle to delete."))
      }
  })
  observeEvent(input$cancelDeleteCycle, {
    toggleModal(session, "deleteCycleConfirmation", toggle = "close")
  })
  observeEvent(input$confirmDeleteCycle, {
    toggleModal(session, "deleteCycleConfirmation", toggle = "close")
    cyclesToDelete <- getCycleRowsToDelete()
    cat(paste("\nOrig cycles: ", unlist(cyclesToDelete), sep=""))
    cyclesToDelete <- convertCyclesToDelete(cyclesToDelete)
    cat(paste("\nNew cycles: ", unlist(cyclesToDelete), sep=""))
    for(i in 1:length(cyclesToDelete))
    {
      cat(paste("\nDeleting Cyc: ", cyclesToDelete[i], sep=""))
      deleteCycle(cyclesToDelete[i])
    }
    showNotification(paste0("Selected cycle(s) successfully deleted for patient ", values$firstName, " ", values$lastName), duration = 4)
    })


  #Helper Functions:
  calculateHours <- function(startdate, starttime, nowdate, nowtime){
    newt1 <- strptime(starttime, "%H:%M")
    newt2 <- strptime(nowtime, "%H:%M")
    daysdif <- as.Date(nowdate, format="%m/%d/%Y")-as.Date(startdate, format="%m/%d/%Y")
    starthrs <- format(newt1, format="%H")
    donehrs <- format(newt2, format="%H")
    startmin <- format(newt1, format="%M")
    donemin <- format(newt2, format="%M")
    difmin <- as.numeric(donemin) - as.numeric(startmin)
    difhrs <- as.numeric(donehrs) - as.numeric(starthrs)
    totaltime <- (daysdif * 24) + difhrs + (difmin/60)
    return(toString(totaltime));
    }
  getCycleData <- function(cycNum, allPoints) {
    sigRows <- NULL
    for(i in 1:nrow(allPoints))
    {
      if(allPoints[i,5] == cycNum)
      {
        sigRows <- rbind(sigRows, allPoints[i,])
      }
    }
    return(sigRows)
   }
  getCycleInformation <- function(cycNum) {
    cycleRow <- NULL
    for(j in 1:nrow(values$cycleDataDF))
    {
      if(values$cycleDataDF[j,1] == toString(cycNum))
      {
        cycleRow <- values$cycleDataDF[j,]
      }
    }
    return(cycleRow)
    }
  loadOldPatientFromFile <- function(){
    #load the entire csv...
    totalList <- read.csv(input$patientFileCSV$datapath, header = FALSE)
    #read patient values from table...
    totalList <- as.data.frame(totalList)
    loadFromDF(totalList)
   }
  loadFromDF <- function(totalList){
      totalList <- as.data.frame(totalList)
      values$completeFile <- totalList
      values$firstName <- toString(totalList[1,2])
      values$lastName <- toString(totalList[2,2])
      values$mrn <- toString(totalList[3,2])
      values$dob <- toString(totalList[4,2])
      values$diagnosis <- toString(totalList[5,2])
      values$regimen <- toString(totalList[6,2])
      values$goalMTX <- toString(totalList[7,2])

    #rest of table are cycles...
      numCycles <- 0
      cycleLocations <- c()
      for(i in 8:nrow(totalList))
      {
        #will only execute if new cycle
        if(totalList[i,1] == "#NewCycle")
        {
          numCycles <- numCycles + 1
          cycleLocations <- c(cycleLocations, i)
        }
      }
      values$numCycles <- numCycles

      #if only one cyle, pull in single
      if(length(cycleLocations) == 1)
      {
        cycNum <- totalList[cycleLocations[1],2]
        cycDay <- totalList[cycleLocations[1] + 1, 2]
        cycMTX <- totalList[cycleLocations[1] + 2, 2]
        cycTotal <- totalList[cycleLocations[1] + 3, 2]
        rowDF <- data.frame(rep(NA, 1),
                            rep(NA, 1),
                            rep(NA, 1),
                            rep(NA, 1),
                            stringsAsFactors=TRUE)
        rowDF[1,1] <- toString(cycNum)
        rowDF[1,2] <- toString(cycDay)
        rowDF[1,3] <- toString(cycMTX)
        rowDF[1,4] <- toString(cycTotal)
        values$cycleDataDF <- rowDF

        pointsDF <- NULL
        numPoints <- nrow(totalList) - (cycleLocations[1] + 4)
        for(j in 1:numPoints)
        {
          dataRowDF <- data.frame(rep(NA, 1),
                              rep(NA, 1),
                              rep(NA, 1),
                              rep(NA, 1),
                              rep(NA, 1),
                              stringsAsFactors=TRUE)
          dataRowDF[1,1] <- toString(totalList[cycleLocations[1] + 4 + j, 1])
          dataRowDF[1,2] <- toString(totalList[cycleLocations[1] + 4 + j, 2])
          dataRowDF[1,3] <- toString(totalList[cycleLocations[1] + 4 + j, 3])
          dataRowDF[1,4] <- toString(totalList[cycleLocations[1] + 4 + j, 4])
          dataRowDF[1,5] <- toString(cycNum)
          pointsDF <- rbind(pointsDF, dataRowDF)
        }
        values$cycleDataPointsDF <- pointsDF
      }
      #if more than one cycle, pull in multiple
      else
      {
        totalRowsDF <- NULL
        pointsDF <- NULL
        for(i in cycleLocations)
        {
          cycNum <- totalList[i, 2]
          cycDay <- totalList[i + 1, 2]
          cycMTX <- totalList[i + 2, 2]
          cycTotal <- totalList[i + 3, 2]
          rowDF <- data.frame(rep(NA, 1),
                              rep(NA, 1),
                              rep(NA, 1),
                              rep(NA, 1),
                              stringsAsFactors=TRUE)
          rowDF[1,1] <- toString(cycNum)
          rowDF[1,2] <- toString(cycDay)
          rowDF[1,3] <- toString(cycMTX)
          rowDF[1,4] <- toString(cycTotal)
          totalRowsDF <- rbind(totalRowsDF, rowDF)

          currentRow <- match(i,cycleLocations)
          if(currentRow == length(cycleLocations))
          {
            numPoints <- nrow(totalList) - (i + 4)
          }
          else
          {
            numPoints <- cycleLocations[currentRow + 1] - (i + 5)
          }

          for(j in 1:numPoints)
          {
            dataRowDF <- data.frame(rep(NA, 1),
                                rep(NA, 1),
                                rep(NA, 1),
                                rep(NA, 1),
                                rep(NA, 1),
                                stringsAsFactors=TRUE)
            dataRowDF[1,1] <- toString(totalList[i + 4 + j, 1])
            dataRowDF[1,2] <- toString(totalList[i + 4 + j, 2])
            dataRowDF[1,3] <- toString(totalList[i + 4 + j, 3])
            dataRowDF[1,4] <- toString(totalList[i + 4 + j, 4])
            dataRowDF[1,5] <- toString(cycNum)
            pointsDF <- rbind(pointsDF, dataRowDF)
          }
        }
        values$cycleDataDF <- totalRowsDF
        values$cycleDataPointsDF <- pointsDF
      }
    }
  getNewPatientData <- function(){
    DF <- hot_to_r(input$HOTNewPatient)
    return(DF)
    }
  checkNewPatientInput <- function(){
    DF <- getNewPatientData()
    for(i in 1:12)
    {
      if(DF[i,2] == "")
      {
        cat("\nFALSE!")
        return(FALSE)
      }
    }
    if(!isValidTime(DF[12,2]))
    {
      cat("\nTIME FALSE!")
      return(FALSE)
    }
    return(TRUE)
    }
  turnNewIntoDF <- function(DFold){
    #Will turn data from "Create New Patient" tab into four columns
    #Assuming that all information is valid (i.e. not null)
    firstName <- DFold[1,2]
    lastName <- DFold[2,2]
    mrn <- DFold[3,2]
    dob <- DFold[4,2]
    diagnosis <- DFold[5,2]
    regimen <- DFold[6,2]
    goalMTX <- DFold[7,2]
    day <- DFold[8,2]
    mtx <- DFold[9,2]
    totalMTX <- DFold[10,2]
    mtxLevel <- 0
    dateDrawn <- DFold[11,2]
    timeDrawn <- convertToTime(DFold[12,2])
    hours <- 0 #From Start of Infusion

    #decide on number of rows
    N <- 13
    #initialize DataFrame
    DF <- data.frame(rep(NA, N),
                     rep(NA, N),
                     rep(NA, N),
                     rep(NA, N),
                     stringsAsFactors=TRUE)
    #Populate DataFrame
    DF[1, ] <- list("First Name:", firstName, "", "")
    DF[2, ] <- list("Last Name:", lastName, "", "")
    DF[3, ] <- list("MRN:", mrn, "", "")
    DF[4, ] <- list("DOB(MM/DD/YY):", dob, "", "")
    DF[5, ] <- list("Diagnosis:", diagnosis, "", "")
    DF[6, ] <- list("Regimen:", regimen, "", "")
    DF[7, ] <- list("Goal MTX:", goalMTX, "", "")
    DF[8, ] <- list("#NewCycle", "1", "", "")
    DF[9, ] <- list("Day:", day, "", "")
    DF[10, ] <- list("MTX(mg/m2):", mtx, "", "")
    DF[11, ] <- list("Total MTX:", totalMTX, "", "")
    DF[12, ] <- list("Hours After Start:", "MTX Level (uM):", "Date Drawn:", "Time Drawn:")
    DF[13, ] <- list("0", mtxLevel, dateDrawn, timeDrawn)

    return(DF)
    }
  getCycleDataPoints <- function(cycleNumber){
    currentRow <- NULL
    DF <- NULL
    #Search For Cycle:
    for(i in 1:nrow(values$cycleDataPointsDF))
    {
      if(values$cycleDataPointsDF[i,5] == toString(cycleNumber))
      {
        currentRow <- values$cycleDataPointsDF[i,1:4]
        DF <- rbind(DF, currentRow)
      }
    }
    return(DF)
    }
  getCurrentPatientDF <- function(){
    #Will turn all data loaded into the app into a DF of four columns
    #Assuming that all information is valid (i.e. not null)

    #CREATE DF1, not dependent on number of cycles
    DF1 <- data.frame(rep(NA, 7),
                     rep(NA, 7),
                     rep(NA, 7),
                     rep(NA, 7),
                     stringsAsFactors=TRUE)
    #Populate DataFrame
    DF1[1, ] <- list("First Name:", values$firstName, "", "")
    DF1[2, ] <- list("Last Name:", values$lastName, "", "")
    DF1[3, ] <- list("MRN:", values$mrn, "", "")
    DF1[4, ] <- list("DOB(MM/DD/YY):", values$dob, "", "")
    DF1[5, ] <- list("Diagnosis:", values$diagnosis, "", "")
    DF1[6, ] <- list("Regimen:", values$regimen, "", "")
    DF1[7, ] <- list("Goal MTX:", values$goalMTX, "", "")

    totalDF <- NULL
    totalDF <- rbind(totalDF, DF1)

    #CREATE ALL CYCLES:
    for(i in 1:values$numCycles)
    {
      #Cycle Information:
      cycleRow <- NULL
      for(j in 1:nrow(values$cycleDataDF))
      {
        if(values$cycleDataDF[j,1] == toString(i))
        {
          cycleRow <- values$cycleDataDF[j,]
        }
      }
      DF <- data.frame(rep(NA, 5),
                      rep(NA, 5),
                      rep(NA, 5),
                      rep(NA, 5),
                      stringsAsFactors=TRUE)
      DF[1, ] <- list("#NewCycle", toString(i), "", "")
      DF[2, ] <- list("Day:", cycleRow[1,2], "", "")
      DF[3, ] <- list("MTX(mg/m2):", cycleRow[1,3], "", "")
      DF[4, ] <- list("Total MTX:", cycleRow[1,4], "", "")
      DF[5, ] <- list("Hours After Start:", "MTX Level (uM):", "Date Drawn:", "Time Drawn:")
      colnames(DF) <- colnames(totalDF)
      totalDF <- rbind(totalDF, DF)

      #Cycle Data Points:
      cycleRows <- getCycleDataPoints(i)
      colnames(cycleRows) <- colnames(totalDF)
      totalDF <- rbind(totalDF, cycleRows)
    }

    return(totalDF)
    }
  createFileName <- function(){
    patname <- paste(values$lastName, values$firstName, sep=" ")
    patname <- tolower(str_replace_all(patname,"\\s+","_"))
    patmrn <- values$mrn
    filename <- paste(patname, '_', patmrn, sep = '')
    return(filename)
    }
  bold <- function(string){
    return(paste0("<b>",string,"</b>"))
    }
  color <- function(string, color){
      return(paste0("<font color = \"", color, "\">", string, "</font>"))
      }
  generateTextPreview <- function(){
    ln <- "<br/>"
    output <- paste(
      bold("First Name:"), " ", values$firstName, ln,
      bold("Last Name:"), " ", values$lastName, ln,
      bold("MRN:"), " ", values$mrn, ln,
      bold("DOB:"), " ", values$dob, ln,
      bold("Diagnosis:"), " ", values$diagnosis, ln,
      bold("Regimen:"), " ", values$regimen, ln,
      bold("Goal MTX:"), " ", values$goalMTX, ln,
      sep=""
    )
    return(output)
    }
  generateAddPointText <- function(){
    ln <- "<br/>"
    output <- paste(
      bold("Current Patient:"), " ", values$lastName, ", ", values$firstName, ln,
      bold("MRN:"), " ", values$mrn, ln,
      bold("Number of Cycles:"), " ", values$numCycles, ln,
      bold("Start of Infusion:"), " ", getCycleInfusion(input$addPointCycleToAdd),
      sep=""
    )
    return(output)
    }
  generateAddCycleText <- function(){
    ln <- "<br/>"
    cycleData <- ""
    for(i in 1:values$numCycles)
    {
      infusion <- getCycleData(i, values$cycleDataPointsDF)[1,]
      cycleData <- paste(
        cycleData,
        bold(paste("Cycle ", i, " Infusion: ", sep="")),
        infusion[1,3], " | ", infusion[1,4], " | MTX Level: ", infusion[1,2], "uM", ln,
        sep=""
      )
    }
    output <- paste(
      bold("Current Patient:"), " ", values$lastName, ", ", values$firstName, ln,
      bold("MRN:"), " ", values$mrn, ln,
      cycleData,
      sep=""
    )
    return(output)
    }
  generateCyclePreview <- function(){
    ln <- "<br/>"
    output <- ""
    for(i in 1:values$numCycles)
    {
      output <- paste(output, ln, getCycleText(i), sep="")
    }
    return(output)
    }
  getCycleText <- function(cycleNum){
    ln <- "<br/>"
    colorVecStrong <- c("BA3A1A", "117A5D", "440C63", "002C7A", "467A00")
    colorVecWeak <- c("DF2702", "16A085", "7D3C98", "0251DF", "61AA00")
    output <- paste(
      color(bold("Cycle Num: "), colorVecStrong[cycleNum%%5]), color(toString(cycleNum), colorVecWeak[cycleNum%%5]), ln,
      color(bold("Day: "), colorVecStrong[cycleNum%%5]), color(getCycleInformation(cycleNum)[2], colorVecWeak[cycleNum%%5]), ln,
      color(bold("MTX(mg/m2): "), colorVecStrong[cycleNum%%5]), color(getCycleInformation(cycleNum)[3], colorVecWeak[cycleNum%%5]), ln,
      color(bold("Total MTX: "), colorVecStrong[cycleNum%%5]), color(getCycleInformation(cycleNum)[4], colorVecWeak[cycleNum%%5]), ln,
      sep="")
    output <- paste(output, getCyclePointsPreview(cycleNum, colorVecStrong, colorVecWeak))
    return(output)
    }
  getCyclePointsPreview <- function(cycleNum, colorVecStrong, colorVecWeak){
    ln <- "<br/>"
    DF <- getCycleDataPoints(cycleNum)
    allCycles <- ""
    for(i in 1:nrow(DF))
    {
      allCycles <- paste(
        allCycles,
        "<tr>
          <td>", DF[i,1], "</td>
          <td>", DF[i,2], "</td>
          <td>", DF[i,3], "</td>
          <td>", DF[i,4], "</td>
        </tr>",
        sep=""
      )
    }
    output <- paste(color(paste("
      <table style=\"width:100%\">
      <tr>
        <th>Hours:</th>
        <th>MTX Level:</th>
        <th>Date:</th>
        <th>Time:</th>
      </tr>",
      allCycles,
    "</table>", sep=""), colorVecStrong[cycleNum%%5]),
    sep=""
    )
    return(output)
    }
  getColors <- function(levels, hours, colorlist){
      output <- NULL
      col <- NULL
      for(i in 1:length(levels))
      {
        if(levels[i] < .1)
        {
          col <- colorlist[1]
        }
        else if(levels[i] < (24.99 * exp(-1 * 0.0644 * hours[i]) + 0.6168 * exp(-1 * 0.0009494 * hours[i])))
        {
          col <- colorlist[2]
        }
        else if(levels[i] < (254.3 * exp(-1 * 0.09239 * hours[i]) + 6.544 * exp(-1 * 0.001471 * hours[i])))
        {
          col <- colorlist[3]
        }
        else if(levels[i] < (1137 * exp(-1 * 0.09467 * hours[i]) + 52.09 * exp(-1 * -0.000269 * hours[i])))
        {
          col <- colorlist[4]
        }
        else
        {
          col <- colorlist[5]
        }
        output <- c(output, col)
      }
      return(output)
    }
  getColorScheme <- function(){
    colors <- brewer.pal(6,"YlOrRd"<-switch(input$colorPickSingle, #A * (-1 * alpha * input$hrsAfterStart)SQRD + B * (-1 * beta * input$hrsAfterStart)SQRD
          basic = "YlOrRd",
          extreme = "Dark2",
          shaded = "Set1"
          )#endSwitch
          )#endBrewerPal
    #manual editing:
    if(input$colorPickSingle == "basic")
    {
      colors[1] <- "#00ce00"
      colors[2] <- "#e1e401"
      colors[3] <- "#fdaa0f"
      colors[4] <- "#ff8040"
      colors[5] <- "#ff0000"
      colors[6] <- "#000000"
    }
    if(input$colorPickSingle == "shaded")
    {
      colors[1] <- "black"
      colors[2] <- "black"
      colors[3] <- "black"
      colors[4] <- "black"
      colors[5] <- "black"
      colors[6] <- "black"
    }
    return(colors)
    }
  getOrderedPoints <- function(cycNum){
    cyclePoints <- data.frame(getCycleDataPoints(cycNum))
    colnames(cyclePoints) <- c("hrs", "mtx", "date", "time")
    cyclePoints[order("hrs"),]
    }
  getLineToAdd <- function(){
    cyclePoints <- getOrderedPoints(input$addPointCycleToAdd)
    startDate <- cyclePoints[1,3]
    startTime <- cyclePoints[1,4]
    currentDateFormat <- as.character(format(input$addPointDate, "%m/%d/%Y"))
    currentHours <- calculateHours(startDate, startTime, currentDateFormat, convertToTime(input$addPointTime))
    DF <- data.frame(as.character(currentHours), as.character(input$addPointMTXLevel), as.character(format(input$addPointDate, "%m/%d/%Y")), input$addPointTime)
    colnames(DF) <- c("Hours After Start:", "MTX Level(uM):", "Date Drawn:", "Time Drawn:")
    return(DF)
    }
  addPoint <- function(){
    DF <- getLineToAdd()
    DF[1,5] <- input$addPointCycleToAdd
    allPoints <- values$cycleDataPointsDF
    colnames(allPoints) <- c("hrs", "mtx", "date", "time", "cycle")
    colnames(DF) <- colnames(allPoints)
    allPoints <- rbind(allPoints, DF)
    allPointsSorted <- sortPoints(allPoints)
    #allPointsSorted <- allPoints[ order(allPoints[,1], allPoints[,5]), ]
    cat(unlist(allPointsSorted))
    values$cycleDataPointsDF <- allPointsSorted
    }
  sortPoints <- function(allPoints){
    output <- NULL
    colnames(allPoints) <- c("hrs", "mtx", "date", "time", "cycle")
    allPoints <- allPoints[mixedorder(allPoints$hrs),]
    allPoints <- allPoints[order(allPoints[,5]),]
    return(allPoints)
    }
  checkForDuplicates <- function(row_to_find){
    #True if unique, False if duplicate exists
    #Very Messy, Better algorithm in future versons.
    allPoints <- values$cycleDataPointsDF
    for(i in 1:nrow(allPoints))
    {
      if(as.character(row_to_find[1,1]) == as.character(allPoints[i,1]))
      {
        if(as.character(row_to_find[1,2]) == as.character(allPoints[i,2]))
        {
          if(as.character(row_to_find[1,3]) == as.character(allPoints[i,3]))
          {
            if(as.character(row_to_find[1,4]) == as.character(allPoints[i,4]))
            {
              if(as.character(row_to_find[1,5]) == as.character(allPoints[i,5]))
              {
                return(FALSE)
              }
            }
          }
        }
      }
    }
    return(TRUE)
    }
  getDeletionIndex <- function(){
    s <- input$deleteCycleDataTable_rows_selected
    return(s)
    }
  getRowsToDelete <- function(){
    allPoints <- values$cycleDataPointsDF
    colnames(allPoints) <- c("hrs", "mtx", "date", "time", "cycle")
    hoursList <- NULL
    levelList <- NULL
    rowindex <- which(allPoints$cycle == toString(input$deleteCycleChoice))
    cycleRows <- allPoints[rowindex,]
    for(i in 1:nrow(cycleRows))
    {
      hoursList <- c(hoursList, as.numeric(cycleRows[i,1]))
      levelList <- c(levelList, as.numeric(cycleRows[i,2]))
    }

    numPoints <- nrow(cycleRows)
    levels <- levelList
    hrs <- hoursList

    DF <- data.frame(rep(NA, numPoints),
                     rep(NA, numPoints),
                     rep(NA, numPoints),
                     rep(NA, numPoints),
                     rep(NA, numPoints),
                     stringsAsFactors=TRUE)
    colnames(DF) <- c("Hours Since Infusion:", "MTX Level(uM):", "Date Drawn:", "Time Drawn:", "Leucovorin Dose:")
    doseCalculations <- getColors(levels, hrs, c("None", "12 mg/m2 q 6 hr PO", "12 mg/m2 q 3 hr PO", "100 mg/m2 q 6 hr IV", "1000 mg/m2 q 6 hr IV"))
    for(i in 1:numPoints)
    {
      DF[i, ] <- list(hrs[i], levels[i], cycleRows[i,3], cycleRows[i,4], doseCalculations[i])
    }
    return(DF[getDeletionIndex(),])
  }
  getIndicies <- function(row){
    cat(paste0("Here is the Row: ", row[1,1], " ", row[1,2], " ", row[1,3], " ", row[1,4], " ", row[1,5], "\n"))
    for(i in 1:nrow(values$cycleDataPointsDF))
    {
      cat(paste0("checking row ", i, ": ", values$cycleDataPointsDF[i,1], " ", values$cycleDataPointsDF[i,2], " ", values$cycleDataPointsDF[i,3], " ", values$cycleDataPointsDF[i,4], " ", values$cycleDataPointsDF[i,5], "\n"))
      if(as.character(values$cycleDataPointsDF[i,1]) == as.character(row[1,1]))
      {
        if(as.character(values$cycleDataPointsDF[i,2]) == as.character(row[1,2]))
        {
          if(as.character(values$cycleDataPointsDF[i,3]) == as.character(row[1,3]))
          {
            if(as.character(values$cycleDataPointsDF[i,4]) == as.character(row[1,4]))
            {
              if(as.character(values$cycleDataPointsDF[i,5]) == as.character(row[1,5]))
              {
                return(i)
              }
            }
          }
        }
      }
    }
    return(NULL)
    }
  deletePoint <- function(){
    allPoints <- values$cycleDataPointsDF
    colnames(allPoints) <- c("hrs", "mtx", "date", "time", "cycle")
    rowsToDelete <- getRowsToDelete()
    indices <- NULL
    for(i in 1:nrow(rowsToDelete))
    {
      rowsToDelete[i,5] <- input$deleteCycleChoice
      indices <- c(indices, getIndicies(rowsToDelete[i,]))
    }
    values$cycleDataPointsDF <- allPoints[-indices, ]
  }
  deleteCycle <- function(cycNum){
    #change cycledataDF
    cat(paste("\nTrying to delete: ", cycNum, sep=""))
    DF1 <- values$cycleDataDF
    colnames(DF1) <- c("cycle", "day", "mtx", "total")
    dataRows <- which(DF1$cycle == toString(cycNum))
    DF1 <- DF1[-dataRows,]
    #update other values up one:
    changeDown1 <- FALSE
    for(i in 1:(values$numCycles-1))
    {
      if(changeDown1)
      {
        newIndex <- toString(as.numeric(DF1[i,1])-1)
        DF1[i,1] <- newIndex
      }
      else if(DF1[i,1] == toString(cycNum + 1))
      {
        changeDown1 <- TRUE
        DF1[i,1] <- toString(cycNum)
      }
    }
    values$cycleDataDF <- DF1

    #change cycleDataPointsDF
    DF2 <- values$cycleDataPointsDF
    colnames(DF2) <- c("hrs", "mtx", "date", "time", "cycle")
    dataPointRows <- which(DF2$cycle == toString(cycNum))
    DF2 <- DF2[-dataPointRows,]
    changeDown2 <- FALSE
    for(i in 1:nrow(DF2))
    {
      if(changeDown2)
      {
        DF2[i,5] <- toString(as.numeric(DF2[i,5])-1)
      }
      else if(DF2[i,5] == toString(cycNum + 1))
      {
        changeDown2 <- TRUE
        DF2[i,5] <- toString(cycNum)
      }
    }
    values$cycleDataPointsDF <- DF2

    #decrement numCycles
    values$numCycles <- values$numCycles - 1
    }
  checkTimeFormat <- function(time){
    return(isValidTime(time))
    }
  getCycleInfusion <- function(cycle){
    myCycle <- getCycleData(cycle, values$cycleDataPointsDF)
    colnames(myCycle) <- c("hrs", "mtx", "date", "time", "cycle")
    myRow <- NULL
    for(i in 1:nrow(myCycle))
    {
      if(myCycle[i,1] == "0")
      {
        myRow <- myCycle[i,]
      }
    }
    output <- paste0(myRow[1,3], " at ", myRow[1,4])
    return(output)
    }
  getAllColors <- function(doseList){
    #cat("GETTING ALL COLORS:")
    colorPal <- getColorScheme()
    colorVec <- NULL
    doseVec <- c("None", "12 mg/m2 q 6 hr PO", "12 mg/m2 q 3 hr PO", "100 mg/m2 q 6 hr IV", "1000 mg/m2 q 6 hr IV")
    for(i in 1:length(doseList))
    {
      for(j in 1:length(doseVec))
      {
        if(doseList[i] == doseVec[j])
        {
          #cat("\n")
          #cat(doseList[i])
          #cat("|")
          #cat(colorPal[j])
          colorVec <- c(colorVec, colorPal[j])
        }
      }
    }
    return(colorVec)
    }
  addCycle <- function(){

    #Add Day, MTX(mg/m2), and Total mtx to cycleDataDF
    cycleData <- values$cycleDataDF
    newCycleRow <- cycleData[1,]
    colnames(newCycleRow) <- colnames(cycleData)
    newCycleRow[1,1] <- values$numCycles + 1
    newCycleRow[1,2] <- input$addCycleDay
    newCycleRow[1,3] <- input$addCycleMTXMg
    newCycleRow[1,4] <- input$addCycleMTXTotal
    cycleData <- rbind(cycleData, newCycleRow)
    values$cycleDataDF <- cycleData

    #Add Point of Infusion to cycleDataPointsDF
    DF <- values$cycleDataPointsDF
    newRow <- DF[1,]
    colnames(newRow) <- colnames(DF)
    newRow[1,1] <- "0"
    newRow[1,2] <- input$addCycleMTXLevel
    newRow[1,3] <- as.character(format(input$addCycleDate, "%m/%d/%Y"))
    newRow[1,4] <- convertToTime(input$addCycleTime)
    newRow[1,5] <- values$numCycles + 1
    DF <- rbind(DF, newRow)
    values$cycleDataPointsDF <- DF

    #increment cycle numb.
    values$numCycles <- as.numeric(newRow[1,5])
    }
  getCycleRowsToDelete <- function(){
    s <- input$deleteCycleDT_rows_selected
    return(s)
  }
  generateDeleteCycleText <- function(){
    cycleList <- getCycleRowsToDelete()
    if(length(cycleList) > 1)
    {
      cycleString <- cycleList[1]
      for(i in 2:length(cycleList))
      {
        cycleString <- paste(cycleString, ", ", cycleList[i], sep="")
      }
      output <- paste(
        "Cycles to delete:<br/>", cycleString,
        sep=""
      )
    }
    else
    {
      output <- paste(
        "Cycle to delete:<br/>", cycleList[1],
        sep=""
      )
    }
    return(output)
  }
  convertCyclesToDelete <- function(cyclesToDelete){
    #If only one deletion, return index
    if(length(cyclesToDelete) == 1)
    {
      return(cyclesToDelete)
    }
    #If multiple, delete from back first:
    return(rev(cyclesToDelete))
  }
  isValidTime <- function(timeString){
    if(grepl(':', timeString))
    {
      minutes <- substr(timeString, nchar(timeString)-1, nchar(timeString))
      if(grepl(':', minutes))
      {
        return(FALSE)
      }
      if(nchar(timeString) == 4)
      {
        hours <- substr(timeString, 1, 1)
      }
      if(nchar(timeString) == 5)
      {
        hours <- substr(timeString, 1, 2)
      }
      if(grepl(':', hours))
      {
        return(FALSE)
      }
      timeString <- gsub(":","",timeString)
    }
    if(nchar(timeString) < 3 || nchar(timeString) > 4)
    {
      return(FALSE)
    }
    if(nchar(timeString) == 3)
    {
      hours <- substr(timeString, 1, 1)
      if(is.na(as.numeric(hours)))
      {
        return(FALSE)
      }
      minutes <- substr(timeString, 2, 3)
      if(is.na(as.numeric(minutes)) || as.numeric(minutes) < 0 || as.numeric(minutes) > 59)
      {
        return(FALSE)
      }
    }
    if(nchar(timeString) == 4)
    {
      hours <- substr(timeString, 1, 2)
      if(is.na(as.numeric(hours)) || as.numeric(hours) < 0 || as.numeric(hours) > 23)
      {
        return(FALSE)
      }
      minutes <- substr(timeString, 3, 4)
      if(is.na(as.numeric(minutes)) || as.numeric(minutes) < 0 || as.numeric(minutes) > 59)
      {
        return(FALSE)
      }
    }
    cat("\nRETURNING TRUE")
    return(TRUE)
  }
  convertToTime <- function(timeString){
    if(grepl(':', timeString))
    {
      return(timeString)
    }
    if(nchar(timeString) == 3)
    {
      hours <- substr(timeString, 1, 1)
      minutes <- substr(timeString, 2, 3)
      return(paste(hours, ":", minutes, sep=""))
    }
    if(nchar(timeString) == 4)
    {
      hours <- substr(timeString, 1, 2)
      minutes <- substr(timeString, 3, 4)
      return(paste(hours, ":", minutes, sep=""))
    }
  }

  #Home Tab:
  output$homeTitle <- renderUI({
    HTML(bold(color("MTXV", "Tomato")))
  })

  #Create New Patient Tab
  output$infusionNote <- renderText({
    paste("* Time is accepted in military format with or without a colon (i.e. 0000-2359 or 00:00-23:59)")
  })
  output$HOTNewPatient <- renderRHandsontable({
    totalList <- NULL
    totalNum <- NULL
    #Initialize DataFrame
    DF <- data.frame(rep(NA, 12),
                     rep(NA, 12),
                     stringsAsFactors=TRUE)
    #Populate DataFrame
    DF[1, ] <- list("First Name:", "")
    DF[2, ] <- list("Last Name:", "")
    DF[3, ] <- list("MRN:", "")
    DF[4, ] <- list("DOB(MM/DD/YY):", "")
    DF[5, ] <- list("Diagnosis:", "")
    DF[6, ] <- list("Regimen:", "")
    DF[7, ] <- list("Goal MTX:", "")
    DF[8, ] <- list("Day:", "")
    DF[9, ] <- list("MTX(mg/m2):", "")
    DF[10, ] <- list("Total MTX:", "")
    DF[11, ] <- list("Date of MTX Infusion:", format(Sys.time(), format="%m/%d/%Y"))
    DF[12, ] <- list("Start Time of MTX Infusion*:", format(Sys.time(), format="%H:%M"))

    rhandsontable(DF, colHeaders = NULL)%>%
      hot_cols(colWidths = 150) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_rows(rowHeights = 30)
  })

  #View Patient Nomogram Tab
  output$csvtab_graph <- renderPlot({
    shiny::validate(
      need(!is.null(values$firstName), "Please Upload or Create a New Patient to view the Nomogram")
      )
    mainlabel <- ""
    #get colors
     colors <- brewer.pal(5,"Set3"<-switch(input$colorType2, #A * (-1 * alpha * input$hrsAfterStart)SQRD + B * (-1 * beta * input$hrsAfterStart)SQRD
           varA = "Set1",
           varB = "GnBu",
           varC = "Reds",
           varD = "Purples",
           varE = "Greens",
           varBLACK = "Spectral"
           )#endSwitch
           )#endBrewerPal
           if(input$colorType2 == "varB" | input$colorType2 == "varC" | input$colorType2 == "varD" | input$colorType2 == "varE"){colors[1:3] = colors[3:5]}
           if(input$colorType2 == "varBLACK"){colors[1:3] = "black"}
           colors[5] = colors()[180]
           colors[4] = "gray"

    #variables
    colorlist <- colors
    linewidth <- input$curvewidth
    linegridwidth <- .75
    xlimit <- as.numeric(input$longsliderinput2)

    #Set Aspect Ratio
    mar.default <- c(5,4,4,2) + 0.1
    par(mar = mar.default + c(0, 0, -4, 1))
    x <- seq(-10,xlimit + 10,1)

    #Generate Logarithmic Plot
    plot(x, 0*x,
      type = 'l',
      log = 'y',
      xlim = c(0, xlimit),
      ylim = c(.001, 1500), #limits are chosen for best fit
      xlab = NA,
      ylab = NA,
      #xaxt = 'n',
      yaxt = 'n',
      col = colorlist[4],
      lwd = linewidth
      )
      par(new = T)
      box(lwd=2)

    #Generate Axes' Labels:
    axispos <- c(.001, .01, .1, 1, 10, 100, 1000) #to avoid the issue of log(0), I use .001
    axislab <- c(0, .01, .1, 1, 10, 100, 1000)
    axis(4, at = axispos, labels = axislab, tck = 0, las = 2)

    #Grid Lines - Horizontal Main
    abline(h = c(.01,.1,1,10,100,1000), col = colorlist[4], lwd = linegridwidth)
      par(new = T)
    #Grid Lines - Horizontal Smaller
    abline(h = seq(.01,.1,.01), col = colorlist[4], lwd = .2)
      par(new = T)
    abline(h = seq(.1,1,.1), col = colorlist[4], lwd = .2)
      par(new = T)
    abline(h = seq(1,10,1), col = colorlist[4], lwd = .2)
      par(new = T)
    abline(h = seq(10,100,10), col = colorlist[4], lwd = .2)
      par(new = T)
    abline(h = seq(100,1000,100), col = colorlist[4], lwd = .2)
      par(new = T)


    #Grid Lines - Vertical
    abline(v = c(0,20,40,60,80,100,120,140,160,180,200, 220, 240), col = colorlist[4], lwd = linegridwidth)
      par(new = T)

    #MTX Level Lines
    lines(x, 1137 * exp(-1 * 0.09467 * x) + 52.09 * exp(-1 * -0.000269 * x), col = "black", lwd = linewidth)
      par(new = T)
    lines(x, 254.3 * exp(-1 * 0.09239 * x) + 6.544 * exp(-1 * 0.001471 * x), col = "black", lwd = linewidth)
      par(new = T)
    lines(x, 24.99 * exp(-1 * 0.0644 * x) + 0.6168 * exp(-1 * 0.0009494 * x), col = "black", lwd = linewidth)
      par(new = T)
    abline(h = .1, col = "black", lwd = linewidth)
      par(new = T)

    #Dosage Text Labels:
    text(100,.03, "None")
    text(100,.23, "12 mg/m2 q 6 hr PO")
    text(100,2, "12 mg/m2 q 3 hr PO")
    text(100,15, "100 mg/m2 q 6 hr IV")
    text(100,300, "1000 mg/m2 q 6 hr IV")

    #Axis Labels
    mtext("MTX Micromol (uM)", side=2, line=3, cex = 1.25)
    mtext("Hours Since Start of MTX", side=1, line=3, cex = 1.25)

    allPoints <- values$cycleDataPointsDF
    colnames(allPoints) <- c("hrs", "mtx", "date", "time", "cycle")

    for(i in 1:values$numCycles)
    {
      if(input[[paste0("showCycle",i)]])
      {
        cyclePoints <- getCycleData(i, allPoints)
        infusionDate <- cyclePoints[1,3]
        if(!input$showInfusionTimeMultipleCycles && nrow(cyclePoints) > 1)
        {
          cyclePoints <- cyclePoints[-1,]
        }
        hoursList <- cyclePoints[1]
        levelList <- cyclePoints[2]
        levelList[levelList == 0] <- .001
        hoursVec <- unlist(hoursList)
        levelVec <- unlist(levelList)
        if(!input$showInfusionTimeMultipleCycles)
        {
          text(as.numeric(hoursVec[1]) - 10,as.numeric(levelVec[1]), paste0("c", i, ": ", infusionDate), col = colors[i], cex = 1.25)
        }
        else
        {
          text(as.numeric(hoursVec[1]) - 2,as.numeric(levelVec[1]), paste0("c", i), col = colors[i], cex = 1.25)
        }
        points(hoursVec,levelVec,col = colors[i], type = 'l', lwd = 2, pch = 21)
        points(hoursVec,levelVec,col = colors[i], lwd = 6, pch = 16)
      }
    }


  })
  output$singlecycle_graph <- renderPlot({
    #shiny::validate Patient Exists:
    shiny::validate(
      need(!is.null(values$firstName), "Please Upload or Create a New Patient to view the Nomogram")
    )
    #shiny::validate Chosen Cycle Exists:
    shiny::validate(
      need(input$cycleChoiceSingle <= values$numCycles && input$cycleChoiceSingle > 0 && input$cycleChoiceSingle%%1 == 0, paste0("Could not find data for Cycle ", input$cycleChoiceSingle, ". Please choose a cycle between 1 and ", values$numCycles, "."))
    )

    xlimit <- as.numeric(input$longsliderinput2)
    allPoints <- values$cycleDataPointsDF
    colnames(allPoints) <- c("hrs", "mtx", "date", "time", "cycle")
    hoursList <- NULL
    levelList <- NULL

    rowindex <- which(allPoints$cycle == toString(input$cycleChoiceSingle))
    cycleRows <- allPoints[rowindex,]

    for(i in 1:nrow(cycleRows))
    {
      hoursList <- c(hoursList, as.numeric(cycleRows[i,1]))
      levelList <- c(levelList, as.numeric(cycleRows[i,2]))
    }
    levelList[levelList == 0] <- .001

    s <- input$cycleDataTable_rows_selected

    colorList <- getColors(levelList, hoursList, c("None", "12 mg/m2 q 6 hr PO", "12 mg/m2 q 3 hr PO", "100 mg/m2 q 6 hr IV", "1000 mg/m2 q 6 hr IV"))
    colorsToAdd <- getAllColors(colorList)
    DFtest <- data.frame(hoursList, levelList, colorList, seq(0,length(hoursList)-1), colorsToAdd)

    colorPal <- getColorScheme()

    colnames(DFtest) <- c("hours", "level", "colors", "number", "col")
    line0 <- function(x) (x*0 + .1)
    line1 <- function(x) (1137 * exp(-1 * 0.09467 * x) + 52.09 * exp(-1 * -0.000269 * x))
    line2 <- function(x) (254.3 * exp(-1 * 0.09239 * x) + 6.544 * exp(-1 * 0.001471 * x))
    line3 <- function(x) (24.99 * exp(-1 * 0.0644 * x) + 0.6168 * exp(-1 * 0.0009494 * x))

    if(!input$showInfusionTimeSingleCycle && nrow(DFtest) > 1)
    {
      DFtest <- DFtest[-1,]
      if(!is.null(s))
      {
        for(i in seq(1,length(s)))
        {
          s[[i]] <- (s[[i]] - 1)
        }
      }
    }
    ggplot(DFtest, aes(x = hours, y = level, color = colors, c(0,xlimit))) +
      #scale_y_continuous(trans = 'log10', breaks = trans_breaks("log10", function(x) 10^x)) +
      coord_cartesian(
        xlim = c(0, xlimit),
        ylim = c(.001, 1500)
      ) +
      #Horizontal Lines
      geom_hline(yintercept = c(.01, .1, 1, 10, 100, 1000), colour = "#707070") +
      geom_hline(yintercept = seq(.01, .1, .01), colour = "#EAEAEA") +
      geom_hline(yintercept = seq(.1, 1, .1), colour = "#EAEAEA") +
      geom_hline(yintercept = seq(1, 10, 1), colour = "#EAEAEA") +
      geom_hline(yintercept = seq(10, 100, 10), colour = "#EAEAEA") +
      geom_hline(yintercept = seq(100, 1000, 100), colour = "#EAEAEA") +
      #Vertical Lines
      geom_segment(aes(x = 0, y = .001, xend = 0, yend = 1500), colour = "#E0E0E0") +
      geom_segment(aes(x = 20, y = .001, xend = 20, yend = 1500), colour = "#E0E0E0") +
      geom_segment(aes(x = 40, y = .001, xend = 40, yend = 1500), colour = "#E0E0E0") +
      geom_segment(aes(x = 60, y = .001, xend = 60, yend = 1500), colour = "#E0E0E0") +
      geom_segment(aes(x = 80, y = .001, xend = 80, yend = 1500), colour = "#E0E0E0") +
      geom_segment(aes(x = 100, y = .001, xend = 100, yend = 1500), colour = "#E0E0E0") +
      geom_segment(aes(x = 120, y = .001, xend = 120, yend = 1500), colour = "#E0E0E0") +
      {if(xlimit > 120) geom_segment(aes(x = 140, y = .001, xend = 140, yend = 1500), colour = "#E0E0E0")} +
      {if(xlimit > 140) geom_segment(aes(x = 160, y = .001, xend = 160, yend = 1500), colour = "#E0E0E0")} +
      {if(xlimit > 160) geom_segment(aes(x = 180, y = .001, xend = 180, yend = 1500), colour = "#E0E0E0")} +
      {if(xlimit > 180) geom_segment(aes(x = 200, y = .001, xend = 200, yend = 1500), colour = "#E0E0E0")} +
      {if(xlimit > 200) geom_segment(aes(x = 220, y = .001, xend = 220, yend = 1500), colour = "#E0E0E0")} +
      {if(xlimit > 220) geom_segment(aes(x = 240, y = .001, xend = 240, yend = 1500), colour = "#E0E0E0")} +

      #Axes / Nomogram Lines
      geom_hline(yintercept = c(.001, 1500), colour = "black") +
      stat_function(fun = line0, colour = "black") +
      stat_function(fun = line1, colour = "black", size=1) +
      stat_function(fun = line2, colour = "black", size=1) +
      stat_function(fun = line3, colour = "black", size=1) +
      #geom_smooth(aes(group=1), colour = "black") +
      {if(!is.null(s) && input$highlightSingle) geom_point(data=DFtest[s,], mapping=aes(x=hours, y=level), shape = 21, color = "black", fill = "yellow", size = 15)} +
      {if(!is.null(s) && !input$highlightSingle) geom_point(data=DFtest[s,], mapping=aes(x=hours, y=level), shape = 1, color = "black", size = 15)} +
      geom_point(aes(colour = factor(colors), size = 4), shape = 16) +
      #{if(input$numberPoints) geom_point(color = "black", fill = "yellow", shape = 22, size = 6)}+
      {if(input$numberPoints && !input$highlightSingle) geom_text(aes(label = number), color = "black", fontface = "bold", nudge_x = -3, size = 6)}+
      {if(input$numberPoints && input$highlightSingle) geom_label(aes(label = number), color = "black", fill = "yellow", fontface = "bold", nudge_x = -3, size = 6, label.padding = unit(.5, "lines"))}+
      annotate("text", x=100, y=300, label= "1000 mg/m2 q 6 hr IV") +
      annotate("text", x=100, y=15, label= "100 mg/m2 q 6 hr IV") +
      annotate("text", x=100, y=2, label= "12 mg/m2 q 3 hr PO") +
      annotate("text", x=100, y=.23, label= "12 mg/m2 q 6 hr PO") +
      annotate("text", x=100, y=.03, label= "None") +
      coord_trans(y = "log10") +
      scale_color_manual(values = c("None" = colorPal[1], "12 mg/m2 q 6 hr PO" = colorPal[2], "12 mg/m2 q 3 hr PO" = colorPal[3], "100 mg/m2 q 6 hr IV" = colorPal[4], "1000 mg/m2 q 6 hr IV" = colorPal[5])) +
      scale_x_continuous(breaks=c(0,20,40,60,80,100,120), labels = c("0","20","40","60","80","100","120")) +
      scale_y_continuous(breaks=c(.001, .01, .1, 1, 10, 100, 1000), labels = c(0, .01, .1, 1, 10, 100, 1000)) +
      #{if(input$colorPickSingle == "shaded") geom_area(stat = "function", fun = line2, fill = "blue", xlim = c(0, xlimit))} +
      theme(panel.background = element_blank(),
            axis.text.x = element_text(face="bold", size=14),
            axis.text.y = element_text(face="bold", size=14),
            legend.position="none")
  })
  output$cycleDataTable <- renderDataTable({
    shiny::validate(
      need(!is.null(values$firstName), "")
    )
    #shiny::validate Chosen Cycle Exists:
    shiny::validate(
      need(input$cycleChoiceSingle <= values$numCycles && input$cycleChoiceSingle > 0 && input$cycleChoiceSingle%%1 == 0, "")
    )
    allPoints <- values$cycleDataPointsDF
    colnames(allPoints) <- c("hrs", "mtx", "date", "time", "cycle")
    hoursList <- NULL
    levelList <- NULL
    rowindex <- which(allPoints$cycle == toString(input$cycleChoiceSingle))
    cycleRows <- allPoints[rowindex,]
    for(i in 1:nrow(cycleRows))
    {
      hoursList <- c(hoursList, as.numeric(cycleRows[i,1]))
      levelList <- c(levelList, as.numeric(cycleRows[i,2]))
    }

    numPoints <- nrow(cycleRows)
    levels <- levelList
    hrs <- hoursList

    DF <- data.frame(rep(NA, numPoints),
                     rep(NA, numPoints),
                     rep(NA, numPoints),
                     rep(NA, numPoints),
                     rep(NA, numPoints),
                     stringsAsFactors=TRUE)
    colnames(DF) <- c("Hours Since Infusion:", "MTX Level(uM):", "Date Drawn:", "Time Drawn:", "Leucovorin Dose:")
    doseCalculations <- getColors(levels, hrs, c("None", "12 mg/m2 q 6 hr PO", "12 mg/m2 q 3 hr PO", "100 mg/m2 q 6 hr IV", "1000 mg/m2 q 6 hr IV"))
    for(i in 1:numPoints)
    {
      DF[i, ] <- list(hrs[i], levels[i], cycleRows[i,3], cycleRows[i,4], doseCalculations[i])
    }

    colorPal <- getColorScheme()
    colorVec <- c(colorPal[1], colorPal[2], colorPal[3], colorPal[4], colorPal[5])
    doseVec <- c("None", "12 mg/m2 q 6 hr PO", "12 mg/m2 q 3 hr PO", "100 mg/m2 q 6 hr IV", "1000 mg/m2 q 6 hr IV")

    rownames(DF) <- seq(0,nrow(DF)-1)

    DT::datatable(DF, options = list(initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"),
        searchHighlight = TRUE,
        lengthMenu = c(10, 25, 50, 100, nrow(DF)), pageLength = nrow(DF))) %>%
      formatStyle( 'Leucovorin Dose:',
      backgroundColor = styleEqual(doseVec, colorVec))
  })
  output$singleCycleInstructionHeader <- renderText({
      return("Individual Cycle Viewer Instructions:")
      })
  output$singleCycleInstructionBody <- renderUI({
        HTML("    Before attempting to view a specific patient cycle, ensure that a patient has been either created or uploaded <b>(SEE 'PATIENT DATA' TAB)</b><br/>
                  Once a nomogram is showing, update the 'Cycle To View:' input box to the desired cycle number.<br/>
                  To help analyze the data, select individual points by clicking / touching desired rows in the data table below. The data table can be sorted by selecting the individual arrows to the right of each header column.<br/>
                  Hours may be extended from 120 to 240 hours in increments of 40 to best display the data.")
        })

  #View Patient Data Tab
  output$patientDataNote <- renderUI({
    HTML("The following information is a representation of the patient.csv file currently in
     MTXV. If you have an issue with MTXV, send a copy or screenshot of this page along with
     your support request.")
  })
  output$testTable <- renderTable({
    DF <- getCurrentPatientDF()
    colnames(DF) <- NULL
    return(DF)
  })

  #Add Point Tab
  output$addPointText <- renderUI({
    shiny::validate(
      need(!is.null(values$firstName), "")
      )
    shiny::validate(
      need(input$addPointCycleToAdd <= values$numCycles && input$addPointCycleToAdd > 0, "")
      )
      HTML(generateAddPointText())
    })
  output$plotsingle_plot <- renderPlot({
    #shiny::validate:
    shiny::validate(
      need(!is.null(values$firstName), "Please Upload or Create a New Patient to Add Cycle Points / View the Nomogram.")
      )
    shiny::validate(
      need(input$addPointCycleToAdd <= values$numCycles && input$addPointCycleToAdd > 0 && input$addPointCycleToAdd%%1 == 0, paste0("Could not find data for Cycle ", input$addPointCycleToAdd, ". Please choose a cycle between 1 and ", values$numCycles, "."))
      )
    cyclePoints <- getOrderedPoints(input$addPointCycleToAdd)
    startDate <- cyclePoints[1,3]
    startTime <- cyclePoints[1,4]
    currentDateFormat <- as.character(format(input$addPointDate, "%m/%d/%Y"))
    currentHours <- calculateHours(startDate, startTime, currentDateFormat, input$addPointTime)
    shiny::validate(
      need(currentHours >= 0, paste0("Please adjust date and time to be after infusion."))
      )
    shiny::validate(
      need(input$addPointMTXLevel >= 0, paste0("Please adjust MTX Level to be a non-negative value."))
      )
    shiny::validate(
      need(checkTimeFormat(input$addPointTime), paste0("Please adjust time to the following format - HH:MM where HH is 0-23 and MM is 00-59."))
      )
    hoursSinglePoint <- as.numeric(currentHours)
    mtxLevelSinglePoint <- as.numeric(input$addPointMTXLevel)

    #Get Colors:
    colors <- brewer.pal(5,"Set3"<-switch(input$colorType,
           varA = "Set1",
           varB = "GnBu",
           varC = "Reds",
           varD = "Purples",
           varE = "Greens",
           varBLACK = "Spectral"
           )#endSwitch
           )#endBrewerPal
           if(input$colorType == "varB" | input$colorType == "varC" | input$colorType == "varD" | input$colorType == "varE"){colors[1:3] = colors[3:5]}
           if(input$colorType == "varBLACK"){colors[1:3] = "black"}
           colors[5] = colors()[180]
           colors[4] = "gray"
           colors[6] = "red"


     #account for log scale (if mtx level == 0, make .001)
     if(mtxLevelSinglePoint == 0)
     {
       mtxLevelSinglePoint <- .001
     }

     #variables
     colorlist <- colors
     linewidth <- 2.5
     linegridwidth <- .75
     xlimit <- as.numeric(input$longsliderinput)

     #Set Aspect Ratio
     mar.default <- c(5,4,4,2) + 0.1
     par(mar = mar.default + c(0, 0, -4, 1))
     x <- seq(-10, xlimit + 10,1)

     #Generate Logarithmic Plot
     plot(x, 0*x,
       type = 'l',
       log = 'y',
       xlim = c(0, xlimit),
       ylim = c(.001, 1500), #limits are chosen for best fit
       xlab = NA,
       ylab = NA,
       #xaxt = 'n',
       yaxt = 'n',
       col = colorlist[4],
       lwd = linewidth
       )
       par(new = T)
       box(lwd=2)

     #Generate Axes' Labels:
     axispos <- c(.001, .01, .1, 1, 10, 100, 1000) #to avoid the issue of log(0), I use .001
     axislab <- c(0, .01, .1, 1, 10, 100, 1000)
     axis(4, at = axispos, labels = axislab, tck = 0, las = 2)

     #Grid Lines - Horizontal Main
     abline(h = c(.01,.1,1,10,100,1000), col = colorlist[4], lwd = linegridwidth)
       par(new = T)
     #Grid Lines - Horizontal Smaller
     abline(h = seq(.01,.1,.01), col = colorlist[4], lwd = .2)
       par(new = T)
     abline(h = seq(.1,1,.1), col = colorlist[4], lwd = .2)
       par(new = T)
     abline(h = seq(1,10,1), col = colorlist[4], lwd = .2)
       par(new = T)
     abline(h = seq(10,100,10), col = colorlist[4], lwd = .2)
       par(new = T)
     abline(h = seq(100,1000,100), col = colorlist[4], lwd = .2)
       par(new = T)


     #Grid Lines - Vertical
     abline(v = c(0,20,40,60,80,100,120,140,160,180,200, 220, 240), col = colorlist[4], lwd = linegridwidth)
       par(new = T)

     #MTX Level Lines
     lines(x, 1137 * exp(-1 * 0.09467 * x) + 52.09 * exp(-1 * -0.000269 * x), col = colorlist[1], lwd = linewidth)
       par(new = T)
     lines(x, 254.3 * exp(-1 * 0.09239 * x) + 6.544 * exp(-1 * 0.001471 * x), col = colorlist[2], lwd = linewidth)
       par(new = T)
     lines(x, 24.99 * exp(-1 * 0.0644 * x) + 0.6168 * exp(-1 * 0.0009494 * x), col = colorlist[3], lwd = linewidth)
       par(new = T)
     abline(h = .1, col = "black", lwd = linewidth)
       par(new = T)

     #Dosage Text Labels:
     text(100,.03, "None")
     text(100,.23, "12 mg/m2 q 6 hr PO")
     text(100,2, "12 mg/m2 q 3 hr PO")
     text(100,15, "100 mg/m2 q 6 hr IV")
     text(100,300, "1000 mg/m2 q 6 hr IV")

     #Axis Labels
     mtext("MTX Micromol (uM)", side=2, line=3, cex = 1.25)
     mtext("Hours Since Start of MTX", side=1, line=3, cex = 1.25)

     #Show Cycle Info If Applicable
     allPoints <- values$cycleDataPointsDF
     colnames(allPoints) <- c("hrs", "mtx", "date", "time", "cycle")
     if(input$addPointShowCycle)
     {
         cyclePoints <- getCycleData(input$addPointCycleToAdd, allPoints)
         if(input$showInfusionTimeAddPoint && nrow(cyclePoints) > 1)
         {
           cyclePoints <- cyclePoints[-1,]
         }
         hoursList <- cyclePoints[1]
         levelList <- cyclePoints[2]
         levelList[levelList == 0] <- .001
         hoursVec <- unlist(hoursList)
         levelVec <- unlist(levelList)
         points(hoursVec,levelVec,col = "black", type = 'l', lwd = 2, pch = 21)
         points(hoursVec,levelVec,col = "black", lwd = 6, pch = 16)
     }

     #Point:
     cat(paste0("hoursSinglePoint: ", hoursSinglePoint, "   LevelSinglePoint: ", mtxLevelSinglePoint))
     points(hoursSinglePoint, mtxLevelSinglePoint,
       col = ifelse(mtxLevelSinglePoint < .1, colorlist[5],
                   #Value is above .1
                   ifelse(mtxLevelSinglePoint < (24.99 * exp(-1 * 0.0644 * hoursSinglePoint) + 0.6168 * exp(-1 * 0.0009494 * hoursSinglePoint)), colorlist[3],
                         ifelse(mtxLevelSinglePoint < (254.3 * exp(-1 * 0.09239 * hoursSinglePoint) + 6.544 * exp(-1 * 0.001471 * hoursSinglePoint)), colorlist[2],
                               ifelse(mtxLevelSinglePoint < (1137 * exp(-1 * 0.09467 * hoursSinglePoint) + 52.09 * exp(-1 * -0.000269 * hoursSinglePoint)), colorlist[1], colorlist[6])
                                     )
                               )
                         ),
                         lwd = 4)
      })
  output$addPointInfusionTable <- renderTable({
    DF <- getOrderedPoints(input$addPointCycleToAdd)
    DF <- DF[1,]
  })
  output$addPointTableConfirmation <- renderTable({
    DF <- getLineToAdd()
  })
  output$addPointInfusionData <- renderUI({
      HTML(paste(generateAddPointText(), "<br/>", bold("Cycle Selected: "), input$addPointCycleToAdd, sep=""))
  })
  output$addPointNewHeader <- renderUI({
      HTML(bold("Point To Add:"))
    })

  #Delete Point Tab
  output$deleteCycleDataTable <- renderDataTable({
    shiny::validate(
      need(!is.null(values$firstName), "Please upload / create a new patient before deleting points.")
    )
    #shiny::validate Chosen Cycle Exists:
    shiny::validate(
      need(input$deleteCycleChoice <= values$numCycles && input$deleteCycleChoice > 0 && input$deleteCycleChoice %%1 == 0, paste0("Please select a valid cycle choice from 1 - ", values$numCycles))
    )
    allPoints <- values$cycleDataPointsDF
    colnames(allPoints) <- c("hrs", "mtx", "date", "time", "cycle")
    hoursList <- NULL
    levelList <- NULL
    rowindex <- which(allPoints$cycle == toString(input$deleteCycleChoice))
    cycleRows <- allPoints[rowindex,]
    for(i in 1:nrow(cycleRows))
    {
      hoursList <- c(hoursList, as.numeric(cycleRows[i,1]))
      levelList <- c(levelList, as.numeric(cycleRows[i,2]))
    }

    numPoints <- nrow(cycleRows)
    levels <- levelList
    hrs <- hoursList

    DF <- data.frame(rep(NA, numPoints),
                     rep(NA, numPoints),
                     rep(NA, numPoints),
                     rep(NA, numPoints),
                     rep(NA, numPoints),
                     stringsAsFactors=TRUE)
    colnames(DF) <- c("Hours Since Infusion:", "MTX Level(uM):", "Date Drawn:", "Time Drawn:", "Leucovorin Dose:")
    doseCalculations <- getColors(levels, hrs, c("None", "12 mg/m2 q 6 hr PO", "12 mg/m2 q 3 hr PO", "100 mg/m2 q 6 hr IV", "1000 mg/m2 q 6 hr IV"))
    for(i in 1:numPoints)
    {
      DF[i, ] <- list(hrs[i], levels[i], cycleRows[i,3], cycleRows[i,4], doseCalculations[i])
    }
    DT::datatable(DF, options = list(initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"),
        searchHighlight = TRUE,
   lengthMenu = c(10, 25, 50, 100, nrow(DF)), pageLength = nrow(DF)))
  })
  output$deletePointSelected <- renderTable({
    DF <- getRowsToDelete()
    DF
  })

  #Add Cycle Tab
  output$addCycleText <- renderUI({
    shiny::validate(
      need(!is.null(values$firstName), "")
      )
      HTML(generateAddCycleText())
    })
  output$addCycleInfusionData <- renderUI({
    HTML(paste(
      bold(paste("Will Create Cycle" , values$numCycles + 1)), "<br/>",
      bold("Day: "), ifelse(input$addCycleDay == "", color("EMPTY", "Tomato"), input$addCycleDay), "<br/>",
      bold("MTX(mg/m2): "), ifelse(input$addCycleMTXMg == "", color("EMPTY", "Tomato"), input$addCycleMTXMg), "<br/>",
      bold("Total MTX: "), ifelse(input$addCycleMTXTotal == "", color("EMPTY", "Tomato"), input$addCycleMTXTotal), "<br/>",
      bold("Infusion Date: "), as.character(format(input$addCycleDate, "%m/%d/%Y")), "<br/>",
      bold("Infusion Time: "), input$addCycleTime, "<br/>",
      bold("MTX Level (uM): "), ifelse(is.na(input$addCycleMTXLevel) || input$addCycleMTXLevel < 0, color("INVALID", "Tomato"), as.character(input$addCycleMTXLevel)), sep=""))
  })

  #Delete Cycle Tab
  output$deleteCycleDT <- renderDataTable({
    shiny::validate(
      need(!is.null(values$firstName), "Please upload / create a new patient before deleting points.")
    )
    shiny::validate(
      need(values$numCycles > 1, "Cannot delete a cycle with only one cycle remaining. Please either add a new cycle first or create a new patient.")
    )
    allPoints <- values$cycleDataPointsDF
    colnames(allPoints) <- c("hrs", "mtx", "date", "time", "cycle")
    hoursList <- NULL
    levelList <- NULL
    rowindex <- which(allPoints$hrs == "0")
    cycleRows <- allPoints[rowindex,]
    for(i in 1:nrow(cycleRows))
    {
      hoursList <- c(hoursList, as.numeric(cycleRows[i,1]))
      levelList <- c(levelList, as.numeric(cycleRows[i,2]))
    }

    numCycles <- nrow(cycleRows)
    levels <- levelList
    hrs <- hoursList

    DF <- data.frame(rep(NA, numCycles),
                     rep(NA, numCycles),
                     rep(NA, numCycles),
                     stringsAsFactors=TRUE)
    rownames <- NULL
    for(i in 1:numCycles)
    {
      rownames <- c(rownames, paste("Cycle", i))
    }
    rownames(DF) <- rownames
    colnames(DF) <- c("Infusion MTX Level(uM):", "Infusion Date:", "Infusion Time:")
    for(i in 1: numCycles)
    {
      DF[i, ] <- list(as.character(levels[i]), cycleRows[i,3], cycleRows[i,4])
    }
    DT::datatable(DF, options = list(initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"),
        searchHighlight = TRUE,
   lengthMenu = c(10, 25, 50, 100, nrow(DF)), pageLength = nrow(DF)))
  })
  output$deleteCycleInformation <- renderUI({
    HTML(generateDeleteCycleText())
  })

  #Download Data Tab
  output$downloadData <- downloadHandler(
      filename = function() {
          paste(createFileName(), '.csv', sep='')
        },
      content = function(file) {
        write.table(getCurrentPatientDF(), file, sep=",", col.names=FALSE, row.names=FALSE)
      }
    )
  output$patientInfoText <- renderUI({
      HTML(generateTextPreview())
    })
  output$downloadError <- renderUI({
      HTML(paste("ERROR: No patient data found.</br></br>Please create a new patient or load an existing patient before attempting to download."))
    })
  output$cycleInfoText <- renderUI({
      HTML(generateCyclePreview())
    })

  #Settings Tab

  #FAQ Tab
  output$helpNote <- renderUI({
      HTML("This page is under construction.")
    })

    #Setting to TRUE to avoid reconnection issues
    session$allowReconnect(TRUE)
}


shinyApp(ui, server)
