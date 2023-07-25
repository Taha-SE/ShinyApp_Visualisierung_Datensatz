library(shiny)
library(shinydashboard)
library(data.tree)
library(DiagrammeR)
library(ggplot2)
library(rsconnect)
data<- as.data.frame(read.csv("lung_cancer.csv", header = T))
optionen <- c("SMOKING","YELLOW_FINGERS", "ANXIETY","PEER_PRESSURE","CHRONIC.DISEASE", "FATIGUE","ALLERGY","WHEEZING","ALCOHOL.CONSUMING","COUGHING","SHORTNESS.OF.BREATH","SWALLOWING.DIFFICULTY","CHEST.PAIN","LUNG_CANCER")

# Tab bedingte Wahrscheinlichkeiten
f_baumdiagramm <- function(var1, var2, gender = NULL, minAlter, maxAlter){
  if(gender == "Both"){
    gender <- NULL
  }
  if (is.null(gender)){
    data <- data[data$AGE >= minAlter & data$AGE <= maxAlter,]
    data <- data[,c(var1,var2)]
    ergebnis <- round(addmargins(prop.table(table(data[[var1]],data[[var2]]))),2)
  } else if (!is.null(gender)){
    data <- data[data$AGE >= minAlter & data$AGE <= maxAlter,]
    data <- data[data$GENDER==gender,]
    data <- data[,c(var1,var2)]
    ergebnis <- round(addmargins(prop.table(table(data[[var1]],data[[var2]]))),2)
  }
  rownames(ergebnis) <- c(sprintf("NO %s", var1), var1,"Sum")
  colnames(ergebnis) <- c(sprintf("NO %s", var2), var2, "Sum")
  
  tree <- Node$new("Wahrscheinlichkeit")
  tvar1 <- tree$AddChild(colnames(ergebnis)[2])
  tvar2 <- tree$AddChild(colnames(ergebnis)[1])
  tvar1a <- tvar1$AddChild(rownames(ergebnis)[2])
  tvar1b <- tvar1$AddChild(rownames(ergebnis)[1])
  tvar2a <- tvar2$AddChild(rownames(ergebnis)[2])
  tvar2b <- tvar2$AddChild(rownames(ergebnis)[1])
  
  SetEdgeStyle(tvar1, label = ergebnis[6])
  SetEdgeStyle(tvar2, label = ergebnis[3])
  SetEdgeStyle(tvar1a, label = ergebnis[5])
  SetEdgeStyle(tvar1b, label = ergebnis[4])
  SetEdgeStyle(tvar2a, label = ergebnis[2])
  SetEdgeStyle(tvar2b, label = ergebnis[1])
  
  return(tree)
}
f_kontingenztabelle <- function(var1, var2, gender = NULL, minAlter, maxAlter){
  if(gender == "Both"){
    gender <- NULL
  }
  
  if (is.null(gender)){
    data <- data[data$AGE >= minAlter & data$AGE <= maxAlter,]
    data <- data[,c(var1,var2)]
    ergebnis <- round(addmargins(prop.table(table(data[[var1]],data[[var2]]))),2)
  } else if (!is.null(gender)){
    data <- data[data$AGE >= minAlter & data$AGE <= maxAlter,]
    data <- data[data$GENDER==gender,]
    data <- data[,c(var1,var2)]
    ergebnis <- round(addmargins(prop.table(table(data[[var1]],data[[var2]]))),2)
  }
  rownames(ergebnis) <- c(sprintf("NO %s", var1), var1,"Sum")
  colnames(ergebnis) <- c(sprintf("NO %s", var2), var2, "Sum")
  
  result <- data.frame(
    var1 = c(ergebnis[1:3]),
    var2 = c(ergebnis[4:6]),
    Sum = c(ergebnis[7:9])
  )
  colnames(result) <- colnames(ergebnis)
  rownames(result) <- rownames(ergebnis)
  return(result)
}
# Tab Lift
f_lift <- function(var1, var2, gender = NULL, minAlter, maxAlter){
  if(gender == "Both"){
    gender <- NULL
  }
  if (is.null(gender)){
    data <- data[data$AGE >= minAlter & data$AGE <= maxAlter,]
    data <- data[,c(var1,var2)]
    ergebnis <- round(addmargins(prop.table(table(data[[var1]],data[[var2]]))),2)
  } else if (!is.null(gender)){
    data <- data[data$AGE >= minAlter & data$AGE <= maxAlter,]
    data <- data[data$GENDER==gender,]
    data <- data[,c(var1,var2)]
    ergebnis <- round(addmargins(prop.table(table(data[[var1]],data[[var2]]))),2)
  }
  rownames(ergebnis) <- c(sprintf("NO %s", var1), var1,"Sum")
  colnames(ergebnis) <- c(sprintf("NO %s", var2), var2, "Sum")
  result <- (ergebnis[5]/ergebnis[8])/ergebnis[6]
  return(result)
}
# Tab Erwartungswert
f_erwartungswert <- function(minAlter, maxAlter, gender = NULL){
  if(gender == "Both"){
    gender <- NULL
  }
  if (is.null(gender)){
    ergebnis <- data[data$AGE >= minAlter & data$AGE <= maxAlter,]
  } else {
    data <- data[data$AGE >= minAlter & data$AGE <= maxAlter,]
    ergebnis <- data[data$GENDER==gender,]
  }
  return(round(mean(ergebnis$AGE),2))
}
# Tab Konf_Inter
f_kon_interv <- function(anzahlStichproben,stichprobeGroesse,konfidenzniveau){
  einzel_f_kon_interv <- function(stichprobeGroesse){
    stichprobe <-sample(data$AGE, stichprobeGroesse, replace = FALSE)
    stichprobe_mean <- mean(stichprobe)
    standAbwei <- sd(data$AGE)
    fehl_Quote <- qt(konfidenzniveau,df=stichprobeGroesse-1)*standAbwei/sqrt(stichprobeGroesse)
    
    unter_G <- stichprobe_mean - fehl_Quote
    obere_G <- stichprobe_mean + fehl_Quote
    
    ergebnis <- c(stichprobe_mean, unter_G, obere_G)
    names(ergebnis) <- c("Mittelwert", "obere Grenze","untere Grenze")
    return(ergebnis)
    
  }
  
  ergebnis  <- data.frame(matrix(ncol = 3, nrow = 0))
  ergebnis_names <- c("ID","Mittelwert", "Obere_Grenze", "Untere_Grenze")
  
  
  for (i in 1:anzahlStichproben) {
    
    e <- c(i, einzel_f_kon_interv(stichprobeGroesse))
    ergebnis <- rbind(ergebnis, e)
    
  }
  colnames(ergebnis) <- ergebnis_names
  
  p <- ggplot(ergebnis , aes(ID, Mittelwert))+
    geom_point() +
    geom_errorbar(aes(ymin=Obere_Grenze, ymax=Untere_Grenze))+
    xlab("") + ylab("")
  
  return(p)
}

ui <- dashboardPage(
  dashboardHeader(title = "Gruppe M"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Willkommen", tabName = "Willkommen"),
      menuItem("Allgemein", tabName = "Allgemein"),
      menuItem("Wahrscheinlichkeiten", tabName = "Wahrscheinlichkeiten"),
      menuItem("Stochastische Unabhängigkeit", tabName = "Stochastische_Unabhängigkeit"),
      menuItem("Erwartungswert", tabName = "Erwartungswert"),
      menuItem("Konfidenzintervall", tabName = "Konfidenzintervall"),
      menuItem("Quellen", tabName = "Quellen")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Willkommen",
              h2("Willkommen!"),
              fluidRow(
                box(
                  HTML("<p>Diese App visualisiert die Daten eines Datensatzes &uuml;ber Krebspatienten.</p>
                      <p>Der Zweck der App ist es, an einem realen Datensatz Konzepte der Statistik interaktiv zu demonstrieren. Die Daten des Datensatz können anhand Parametereinstellungen visualisiert werden.</p>
                      <p>Die App bietet 5 &Uuml;bersichten:</p>
                      <ul>
                        <li><strong>Allgemein</strong>: Bietet allgemeine Information &uuml;ber den Datensatz.</li>
                        <li><strong>Wahrscheinlichkeiten</strong>: In dieser &Uuml;bersicht kann die Wahrscheinlichkeit zweier Variablen berechnet und als Kontingenztabelle oder Baumdiagramm dargestellt werden.</li>
                        <li><strong>Stochastische Unabh&auml;ngigkeit</strong>: Die stochastische Unabh&auml;ngigkeit zweier Variablen kann hier berechnet werden.</li>
                        <li><strong>Erwartungswert</strong>: In dieser &Uuml;bersicht wird das Erwartungsalter der Krebspatienten berechnet.</li>
                        <li><strong>Konfidenzintervall</strong>: Konfidenzintervall des Alters k&ouml;nnen hier erstellt werden.</li>
                      </ul>")
                )
              ),
              fluidRow(
                box(
                  HTML("<p>Der Datensatz bietet folgende Variablen:</p>
                          <ul>
                            <li><strong>Gender</strong>: M(male), F(female)</li>
                            <li><strong>Age</strong>: Age of the patient</li>
                            <li><strong>Smoking</strong>: YES=2 , NO=1.</li>
                            <li><strong>Yellow</strong> <strong>fingers</strong>: YES=2 , NO=1.</li>
                            <li><strong>Anxiety</strong>: YES=2 , NO=1.</li>
                            <li><strong>Peer pressure</strong>: YES=2 , NO=1.</li>
                            <li><strong>Chronic Disease</strong>: YES=2 , NO=1.</li>
                            <li><strong>Fatigue</strong>: YES=2 , NO=1.</li>
                            <li><strong>Allergy</strong>: YES=2 , NO=1.</li>
                            <li><strong>Wheezing</strong>: YES=2 , NO=1.</li>
                            <li><strong>Alcohol</strong>: YES=2 , NO=1.</li>
                            <li><strong>Coughing</strong>: YES=2 , NO=1.</li>
                            <li><strong>Shortness</strong> <strong>of</strong> <strong>Breath</strong>: YES=2 , NO=1.</li>
                            <li><strong>Swallowing</strong> <strong>Difficulty</strong>: YES=2 , NO=1.</li>
                            <li><strong>Chest pain</strong>: YES=2 , NO=1.</li>
                            <li><strong>Lung</strong> <strong>Cancer</strong>: YES , NO.</li>
                          </ul>
                          <p>&nbsp;</p>")
                )
              )
      ),
      tabItem(tabName = "Allgemein",
              h2("Allgemein"),
              h3(""), #Designentscheidung, kein Fehler
              fluidRow( 
                box(title= "Verteilung der Patienten in Altersgruppen", solidHeader = TRUE, plotOutput("hist_Altergruppen"))
              ),
              fluidRow(
                box(title = "Verteilung der Geschlechter", solidHeader = TRUE, plotOutput("pie_Verteilung"))
              ),
              fluidRow(
                box(title= "Datensatz", dataTableOutput("dataset"))
              )
      ),
      tabItem(tabName = "Wahrscheinlichkeiten",
              h2("Wahrscheinlichkeiten"),
              h3(""),
              fluidRow(
                box(title = "Wähle die Variablen aus", width = 4, solidHeader = TRUE,
                    selectInput(inputId = "var1_KT", label = "Var1", choices = optionen, selected = optionen[1]),
                    selectInput(inputId = "var2_KT", label = "Var2", choices = optionen,selected = optionen[14]),
                    radioButtons(inputId = "radio_KT", label = "Gender", choices = c("Both","M", "F")),
                    sliderInput(inputId = "slider_KT", label = "Age", min = 20, max = 100, value = c(30,60))
                ),
                box(title= "Kontingenztabelle", solidHeader = TRUE, tableOutput("kon_table")),
                box(title = "Baumdiagramm", solidHeader = TRUE, grVizOutput("baumdiagramm"))
              )
      ),
      tabItem(tabName = "Stochastische_Unabhängigkeit",
              h2("Stochastische Unabhängigkeit"),
              h3(""),
              fluidRow(
                box(title = "Wähle die Variablen aus", width = 4, solidHeader = TRUE,
                    selectInput(inputId = "var1_lift", label = "Var1", choices = optionen, selected = optionen[1]),
                    selectInput(inputId = "var2_lift", label = "Var2", choices = optionen,selected = optionen[14]),
                    radioButtons(inputId = "radio_lift", label = "Gender", choices = c("Both","M", "F")),
                    sliderInput(inputId = "slider_lift", label = "Age", min = 20, max = 100, value = c(30,60))
                ),
                box(
                  p("Um die stochastische Unabhängigkeit zu prüfen, wird der Lift berechnet:"),
                  p(textOutput("lift")),
                  p(textOutput("lift_erklaerung"))
                )
              )
              
      ),
      tabItem(tabName = "Erwartungswert",
              h2("Erwartungswert"),
              h3(""),
              fluidRow(
                box("Wähle die Grenzwerte aus", width = 4,
                    radioButtons(inputId = "radio_ew", label = "Gender", choices = c("Both","M", "F")),
                    sliderInput(inputId = "slider_ew", label = "Age", min = 20, max = 100, value = c(30,60))
                ),
                box(
                  p(textOutput("ew_ergebnis")),
                  p("Das ist das erwartete Alter, in welchem Krebs auftreten könnte.")
                )
              )
      ),
      tabItem(tabName = "Konfidenzintervall",
              h2("Konfidenzintervall für das Alter im Datensatz"),
              h3(""),
              fluidRow(
                box("Wähle die Variablen aus", width = 3,
                    numericInput(inputId = "anzahlStichproben", label = "Anzahl der Stichproben", value = 3),
                    numericInput(inputId = "stichprobeGroesse", label = "Größe der Stichprobe", value = 50),
                    numericInput(inputId = "konfidenzniveau", label = "Konfidenzniveau", value = 0.95,  min = 0, max = 1, step = 0.01),
                ),
                box(title = "Konfidenzintervalle", solidHeader = TRUE, plotOutput("konfidenzintervall"))
              )
      ),
      tabItem(tabName = "Quellen",
              h2("Quellen"),
              h3(""),
              fluidRow(
                box(
                  HTML("<p>Datensatz<p>
                        <ul>
                          <li>https://www.kaggle.com/datasets/nancyalaswad90/lung-cancer</li>
                      </ul>")
                )
              ),
              fluidRow(
                box(
                  HTML("<p>Andere Quellen<p>
                        <ul>
                          <li>https://rstudio.github.io/shinydashboard/get_started.html</li>
                          <li>https://stackoverflow.com/questions/59853592/unable-to-run-a-shiny-app-using-grvizoutput-and-rendergrviz</li>
                          <li>https://statologie.de/konfidenzintervall-r/</li>
                          <li>https://shiny.rstudio.com/articles/html-tags.html</li>
                      </ul>")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  # Tab Allgemein
  output$dataset <- renderDataTable({data},options = list(scrollX = TRUE))
  output$hist_Altergruppen <- renderPlot({
    hist(data[,c("AGE")], breaks = 10, ylab = "Anzahl der Patienten", xlab = "Altergruppen", main = "Verteilung der Patienten in Altersgruppen")
  })
  output$pie_Verteilung <- renderPlot({
    pie(table(data$GENDER), labels = c("Female", "Male"), main = "Verteilung der Geschlechter",
        col = c(gray(0.80), "white"))
    legend("topleft", c("Female", "Male"), fill = c(gray(0.80), "white"))
  })
  
  #Tab bedingte Wahr.
  re_kon_table <- reactive({
    f_kontingenztabelle(input$var1_KT,input$var2_KT,input$radio_KT,input$slider_KT[1],input$slider_KT[2])
  })
  output$kon_table <- renderTable({
    re_kon_table()
    
  }, rownames = TRUE ,scrollX = TRUE)
  
  # Baumdiagram anzeigen
  output$baumdiagramm <- renderGrViz({
    plot(f_baumdiagramm(input$var1_KT,input$var2_KT,input$radio_KT,input$slider_KT[1],input$slider_KT[2]))
  })
  # Tab lift
  output$lift <- renderText({
    round(f_lift(input$var1_lift,input$var2_lift,input$radio_lift,input$slider_lift[1],input$slider_lift[2]),3)
  })
  output$lift_erklaerung <- renderText({
    x <-f_lift(input$var1_lift,input$var2_lift,input$radio_lift,input$slider_lift[1],input$slider_lift[2])
    if (x < 1){
      sprintf("Das Ergebnis ist kleiner als 1. Das bedeutet, dass die Variable %s die Wahrscheinlichkeit von der Variable %s verringert. ", input$var2_lift, input$var1_lift)
    } else if (x >1){
      sprintf("Das Ergebnis ist größer als 1. Das bedeutet, dass die Variable %s die Wahrscheinlichkeit von der Variable %s erhöht.", input$var2_lift, input$var1_lift)
    } else {
      sprintf("Das Ergebnis ist genau 1. Das bedeutet, dass die Variable %s keinen Einfluss auf die Wahrscheinlichkeit von der Variable %s hat (und umgekehrt).", input$var2_lift, input$var1_lift)
    }
  })
  
  # Tab f_erwartungswert
  output$ew_ergebnis <- renderText({
    round(f_erwartungswert(input$slider_ew[1],input$slider_ew[2],input$radio_ew),1)
  })
  #Tab Konf_Inter
  output$konfidenzintervall <- renderPlot({
    f_kon_interv(input$anzahlStichproben,input$stichprobeGroesse,input$konfidenzniveau)
  })
}

shinyApp(ui, server)