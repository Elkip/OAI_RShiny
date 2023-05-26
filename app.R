library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(nnet)

# Load Data
DATAPATH <- Sys.getenv("OAI_DATA")
if (DATAPATH == "" ) stop( "Please add datapath to OAI_DATA" )

source("/home/elkip/Workspace/BU_Notes/Research/OAI_LoadData.R", chdir = T)

bsln <- getBaselineData(DATAPATH)
evnts <- getEvents(DATAPATH)

outcomeLabel <- function(i) {
  switch(as.character(i),
         '1' = {"No Event"},
         '2' = {"Drop Out"},
         '3' = {"Death"},
         (paste("Knee Replacement Cluster", as.numeric(i) - 3))
  )
}

setBslnClstr <- function(clstr = "") {
  data_full <- getCompleteData(DATAPATH, bsln_data = bsln, evnt_data = evnts, cluster = clstr)
  bsln$EVNT <- data_full$EVNT
  print(paste("Baseline Clusters Updated, Levels:", length(levels(bsln$EVNT))))
  return(bsln)
}

setEvtData <- function(dataa) {
  tmp <- dataa %>% select(c(col_bst, "EVNT"))
  return(data.frame(na.omit(tmp)))
}

col_all <- c("AGE", "SEX", "MEDINS", "PASE", "WOMADL", "WOMKP", "WOMSTF", 
             "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT", "COMORBSCORE", 
             "CESD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist", "EDCV", 
             "P01OAGRD", "P02JBMPCV_NEW", "DPRSD", "CEMPLOY_NW", "RACE_O")

col_num <-  c("ID", "AGE", "PASE", "WOMADL", "WOMKP", "WOMSTF", "V00WTMAXKG", 
              "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT", "COMORBSCORE", "CESD")

col_fac <-  c("SEX", "MEDINS", "DPRSD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist", 
              "CEMPLOY_NW", "EDCV", "P01OAGRD", "P02JBMPCV_NEW", "RACE_O")

col_bst <- c("AGE", "SEX", "RACE_O", "PASE", "WOMKP", "WOMSTF", "HEIGHT", 
             "WEIGHT", "V00WTMAXKG", "NSAID", "P01OAGRD_Severe", 
             "P01OAGRD_Moderate", "P01OAGRD_Mild", "P01OAGRD_Possible", 
             "EDCV_HSDeg", "EDCV_GradDeg", "EDCV_UGDeg", "CESD", "Surg_Inj_Hist")

eqtn_bst <- formula(paste("EVNT ~ ", paste(col_bst, collapse = " + "), 
                             "+ WEIGHT:HEIGHT"))

clusters <- c("None", "K.5.Clusters", "K.4.Clusters")

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "OAI Data Analysis"),
  
  dashboardSidebar(
    radioButtons(
      "c",
      "Cluster",
      c("None" = "n", "K.5.Clusters" = "k5", "K.4.Clusters" = "k4"),
      selected = "n"
    ),
    sidebarMenu(
      menuItem("Data Analysis", tabName = "data"),
      menuItem("Model Analysis", tabName = "model")
    )
  ),
  
  dashboardBody(
    tabItems(
    tabItem(tabName = "data",
            fluidRow(
              selectInput('p', 'Predictor', choices = col_all, selected = "AGE")
            ),
            fluidRow(box(
              plotOutput("distPlot")
            ),
            box(
              tabsetPanel(tabPanel("Summary", dataTableOutput("sumData")))
            ))),
    tabItem(tabName = "model",
            fluidRow(
              box(width = 8,
                helpText("Use this calculator to generate the predicted outcome", 
                         " given the predictor values below."),
                splitLayout(
                  numericInput("age", "AGE", 60),
                  numericInput('height', 'HEIGHT', 100),
                  numericInput('weight', 'WEIGHT', 100)
                ),
                splitLayout(
                  numericInput('max', 'Max Weight', 100),
                  numericInput("pase", "PASE", 0)
                ),
                splitLayout(
                  selectInput('sex', 'SEX', c("Male" = 1, "Female" = 2)),
                  selectInput('race', 'RACE', c("White" = 0, "Non-White" = 1)),
                  selectInput('edu', 'Education', c("None" = "0", "High School" = "1",
                                          "Undergrad" = "2", "Grad School" = "3"))
                ),
                splitLayout(
                  sliderInput('cesd', 'CESD', min = 0, max = 60, value = 0),
                  sliderInput('womkp', 'WOMKP', min = 0, max = 20, value = 0),
                  sliderInput('womstf', 'WOMSTF', min = 0, max = 8, value = 0)
                ),
                splitLayout(
                  selectInput('nsaid', 'NSAID', c("No" = 0, "Yes" = 1)),       
                  selectInput('grd', 'OAI GRADE', c("None" = "0", "Possible" = "1", "Mild" = "2",
                                                    "Moderate" = "3", "Severe" = "4")),   
                  selectInput('surj', 'Surg_Inj_Hist', c("No" = "0", "Yes" = "1"))
                ),
                actionButton("predict", "Submit")
              ),
              box(width = 2, title = "Predicted Outcome",
                       textOutput("predVal"),
                       dataTableOutput("predChart")
                     )
            ))
  ))
)

server <- function(input, output) {
 
  d <- reactiveVal()
  d_bst <- reactiveVal()
  mod <- reactiveVal()
  
  observeEvent(input$c, {
    if(input$c == "k5") {
      d(setBslnClstr(clstr = "K.5.Clusters"))
    }
    else if(input$c == "k4") {
      d(setBslnClstr(clstr = "K.4.Clusters"))
    }
    else {
      d(setBslnClstr())
    }
    d_bst(setEvtData(d()))
    mod(multinom(eqtn_bst, data=d_bst()))
  })
  
  ### Data Analysis
  output$distPlot <- renderPlot({ 
    if( input$p %in% col_num ) {
      hist(d()[,input$p], main = paste("Distribution of", input$p), xlab = input$p)
      
      output$sumData <- renderDataTable({
        datatable( d() %>%
                     filter(!is.na(get(input$p))) %>%
                     group_by(EVNT) %>%
                     summarise(avg = mean(get(input$p)), sd = sd(get(input$p)), 
                              min = min(get(input$p)), max = max(get(input$p))), 
                              options = list(dom = 't')
                   ) 
      })
    }
    if( input$p %in% col_fac ) {
        output$sumData <- renderDataTable({
        datatable( d() %>%
                     filter(!is.na(get(input$p))) %>%
                     group_by(EVNT) %>%
                     summarize(freq = n(), SUM = sum(get(input$p) == 1), 
                               perc = SUM/freq*100), options = list(dom = 't')
                   )
      })
      ggplot(d(), aes(get(input$p), fill = EVNT)) + 
        labs(title = input$p, x = input$p) + geom_bar()
    }
  })
  
  ### Model Analysis
  new_data =  data.frame(AGE=50, SEX="1", RACE_O="0", CESD=0, PASE=0, WOMKP=0,
                         WOMSTF=0, HEIGHT=0, WEIGHT=0, NSAID="0", 
                         P01OAGRD_Severe="0", P01OAGRD_Moderate="0", P01OAGRD_Mild="0", 
                         P01OAGRD_Possible="0", EDCV_HSDeg="0", EDCV_GradDeg="0", 
                         EDCV_UGDeg="0", V00WTMAXKG=0, Surg_Inj_Hist="0")
  
  observeEvent(input$predict, {
    new_data$AGE <- input$age    
    new_data$SEX <- input$sex
    new_data$RACE_O <- input$race
    new_data$CESD <- input$cesd
    new_data$PASE <- input$pase
    new_data$WOMKP <- input$womkp
    new_data$WOMSTF <- input$womstf
    new_data$HEIGHT <- input$height
    new_data$WEIGHT <- input$weight
    new_data$NSAID <- input$nsaid
    new_data$V00WTMAXKG <- input$max
    new_data$Surg_Inj_Hist <- input$surj
    
    new_data$P01OAGRD_Severe = "0"
    new_data$P01OAGRD_Moderate = "0"
    new_data$P01OAGRD_Mild = "0"
    new_data$P01OAGRD_Possible = "0"
    if( input$grd != "0") {
      switch(input$grd,
        "1" = {
          new_data$P01OAGRD_Possible = "1"
        },
        "2" = {
          new_data$P01OAGRD_Mild = "1" 
        },
        "3" = {
          new_data$P01OAGRD_Moderate = "1"
        },
        "4" = {
          new_data$P01OAGRD_Severe = "1"
        }
      )
    }
    
    new_data$EDCV_HSDeg = "0"
    new_data$EDCV_GradDeg = "0"
    new_data$EDCV_UGDeg = "0"
    if( input$edu != "0") {
      switch(input$edu,
             "1" = {
               new_data$EDCV_HSDeg = "1"
             },
             "2" = {
               new_data$EDCV_UGDeg = "1" 
             },
             "3" = {
               new_data$EDCV_GradDeg = "1"
             }
      )
    }
    
    output$predVal <- renderText({ outcomeLabel(predict(mod(), newdata = new_data)) })
    output$predChart <- renderDataTable({ datatable(data.frame(Probability = predict(mod(), type = "probs", newdata = new_data)), options = list(dom = 't')) })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
