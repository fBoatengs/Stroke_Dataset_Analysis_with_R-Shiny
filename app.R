library(tidyverse)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)

ui <- fluidPage(#theme = shinytheme("cerulean"),
  tabsetPanel(
    
      ##DESCRIPTIVE STATISTICS 
    
      tabPanel("Descriptive Statistics",
               fluidRow(
                 column(4,
                        sliderInput("bmirng", "BMI", value = c(10, 100), min = 10, max = 100),
                        sliderInput("agerng", "Age", value = c(0, 90), min = 0, max = 90),
                        sliderInput("glurng", "Average Glucouse Level", value = c(50, 300), min = 50, max = 300)
                 ),
                 
                 column(2,
                        
                        radioButtons("htd", "Heart Disease", c("All","Yes","No")),
                        radioButtons("marrg", "Ever Married", c("All","Yes","No")),
                        radioButtons("res", "Residence Type", c("All","Rural","Urban"))
                 ), 
                 
                 column(3,
                        checkboxGroupInput("worktyp", "Work Type",
                                           choices = c("All","Children", "Govt_job","Never_worked", "Private", "Self_employed"),
                                           selected = c("All")),
                        radioButtons("gend", "Gender", c("All","Male","Female"))
                 ),
                 column(3,
                        checkboxGroupInput("smoke", "Smoking Status",
                                           choices = c("All","Formerly_smoked", "Never_smoked", "Smokes", "Unknown"),
                                           selected = c("All")),
                        radioButtons("hpy", "Hypertension", c("All","Yes","No"))
                 ),
                 
               ),
               hr(),
               #submitButton("UPDATE"),
               actionButton("goButton", "Update",icon = icon("refresh")),
               hr(),
               tableOutput("summary")),

      
      ##DATA VISUALIZATION
      
      tabPanel("Visualizations",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("x_var", "Select X variable:",
                               c("age","ever_married","gender","hypertension","heart_disease","Residence_type",
                                 "smoking_status","stroke","work_type")),
                   selectInput("y_var", "Select Y variable:",c()),
                   conditionalPanel( condition = "input.x_var == 'age' && input.y_var == 'density'",
                                     selectInput("fillvar1", "Select fill variable:", c("none", "gender"))
                   ),
                   conditionalPanel( condition = "input.x_var == 'stroke' && input.y_var == 'bmi'",
                                     selectInput("fillvar2", "Select fill variable:", c("none", "gender"))
                   ),
                   conditionalPanel( condition = "input.x_var == 'work_type' && input.y_var == 'age'",
                                     selectInput("fillvar3", "Select fill variable:", c("none", "stroke"))
                   ),
                   conditionalPanel( condition = "input.x_var == 'ever_married' && input.y_var == 'age'",
                                     selectInput("fillvar4", "Select fill variable:", c("none", "stroke"))
                   )
                 ),
                 mainPanel(
                   verbatimTextOutput("ttest"),
                   plotOutput("scatter_plot")
                 )
               )
      ),
    
      
    ##PREDICTION
      
    tabPanel("Stroke Prediction",
             sidebarLayout(
               sidebarPanel(
                    numericInput("age", "Enter your Age", value =40, min = 0, max = 85),
                    numericInput("AGL", "What is your Avg Glucose Level?", value = 50, min = 40, max = 300),
                    selectInput("Hyp", "Do you have Hypertension?",choices = c("Yes", "No")),
                    selectInput("Htd", "Do you have Heart Disease?",choices = c("Yes", "No")),
                    actionButton("predictButton", "PREDICT",icon = icon("refresh"))
                    ),
               
                    mainPanel(
                    textOutput("stroke"))) 
    )         
  )
)
server <- function(input, output, session) {
  
  ##UPDATING THESE CHECK BOXES TO UNSELECT THE "All" OPTION WHEN THE USER CHECK 2 OR MORE ALTERNAE OPTIONS. 
  observeEvent(input$smoke, {
    if (length(input$smoke) > 1 && "All" %in% input$smoke) {
      updateCheckboxGroupInput(session, "smoke", selected = input$smoke[-which(input$smoke == "All")])
    }
  })
  
  observeEvent(input$worktyp, {
    if (length(input$worktyp) > 1 && "All" %in% input$worktyp) {
      updateCheckboxGroupInput(session, "worktyp", selected = input$worktyp[-which(input$worktyp == "All")])
    }
  })
  
  
  ##  FILTERING THE DATA BASED ON THE USER'S SELECTION.
  Data <- eventReactive(input$goButton, {
    
    data <- df
    
    if (input$htd != "All") {
      data <- data %>% filter(heart_disease == input$htd)
    }
    if (input$marrg != "All") {
      data <- data %>% filter(ever_married == input$marrg)
    }
    if (input$res != "All") {
      data <- data %>% filter(Residence_type == input$res)
    }
    if (input$gend != "All") {
      data <- data %>% filter(gender == input$gend)
    }
    if (input$hpy != "All") {
      data <- data %>% filter(hypertension == input$hpy)
    }
    if ("All" %in% input$smoke) {
      data
    } else {
      data <- data %>% filter(smoking_status %in% input$smoke)
    }
    if ("All" %in% input$worktyp) {
      data
    } else {
      data <- data %>% filter(work_type %in% input$worktyp)
    }
    data <- data %>% filter(bmi >= input$bmirng[1] & bmi <= input$bmirng[2])
    data <- data %>% filter(age >= input$agerng[1] & age <= input$agerng[2])
    data <- data %>% filter(avg_glucose_level >= input$glurng[1] & avg_glucose_level <= input$glurng[2])
    return((data%>% group_by(stroke) %>%summarize(count = n())) %>% 
             rename("Stroke" = "stroke", "Sample Size" = "count"))
  })
  
  
  ##RENDERING THE FILTERED DATA
  output$summary <- renderTable({
    Data()
  })
    
  
  
  
  ##VISUALIZATION BASE ON THE USER'S PREFERENCE. 
  
  ##UPDATING THE "y_var" OPTIONS BASED ON THE "x_var" OPTION THE USER SELECTS
  observeEvent(input$x_var, {
    if (input$x_var == "age") {
      updateSelectInput(session, inputId = "y_var",choices = c("density","bmi","avg_glucose_level","hypertension","stroke"))
      
    } else if (input$x_var == "ever_married") {
      updateSelectInput(session, inputId = "y_var",choices = c("age", "stroke"))
      
    } else if (input$x_var == "gender") {
      updateSelectInput(session, inputId = "y_var",choices = c("stroke"))
      
    } else if (input$x_var == "hypertension") {
      updateSelectInput(session, inputId = "y_var", choices = c("stroke"))
      
    } else if (input$x_var == "heart_disease") {
      updateSelectInput(session, input$y_var, choices = c("stroke"))
      
    } else if (input$x_var == "Residence_type") {
      updateSelectInput(session, inputId = "y_var", choices = c("stroke"))
      
    } else if (input$x_var == "smoking_status") {
      updateSelectInput(session, inputId = "y_var", choices = c("stroke"))
      
    } else if (input$x_var == "stroke") {
      updateSelectInput(session, inputId = "y_var", choices = c("bmi","avg_glucose_level"))
      
    } else {
      updateSelectInput(session, inputId = "y_var", choices = c("age", "stroke"))
    }})
  
  ##PERFORMING STATISTICAL TEST BASED ON THE INPUT THE USER SELECTS AND DISPLAYS THE P-VALUE
  output$ttest <- renderText({
    
    A <- c("age","bmi","avg_glucose_level")
    B <- c("ever_married","stroke","gender","hypertension","heart_disease",
           "Residence_type","smoking_status","work_type")
    
    if(input$x_var %in% A && input$y_var%in% A) {
      p_val <- t.test(df[[input$x_var]], df[[input$y_var]])$p.value
      paste("The p-value of the T-Test is", p_val)
      
    } else if (input$x_var %in% B && input$y_var %in% B) {
      p_val <- chisq.test(df[[input$x_var]], df[[input$y_var]])$p.value
      paste("The p-value of the Chi-Squared Test is", round(p_val, 4))
      
    } else if (input$x_var %in% A && input$y_var %in% B) {
      p_val <- summary(aov(df[[input$x_var]] ~ df[[input$y_var]]))[[1]]$"Pr(>F)"[1]
      paste("The p-value of the ANOVA test is", round(p_val, 4)) 
      
    } else if (input$x_var %in% B && input$y_var %in% A) {
      p_val <- summary(aov(df[[input$y_var]] ~ df[[input$x_var]]))[[1]]$"Pr(>F)"[1]
      paste("The p-value of the ANOVA test is", round(p_val, 4))  
      
    } else {
      p_val <- t.test(df$age, df$age)$p.value
      paste("The p-value of the T-Test is", p_val)
    }
  })
  
  
  ##GROUPING DATA FOR VISUALIZATION 
  df_4 <- df %>% group_by(age, hypertension) %>% filter(hypertension == "Yes") %>%count(hypertension)
  df_1 <- df %>% group_by(gender, stroke) %>% count(stroke)
  df_2 <- df %>% group_by(age, stroke) %>% filter(stroke == "Yes")%>% count(stroke)
  df_3 <- df %>% group_by(hypertension, stroke) %>% count(stroke)
  df_5 <- df %>% group_by(heart_disease, stroke) %>% count(stroke)
  df_6 <- df %>% group_by(ever_married, stroke) %>% count(stroke)
  df_7 <- df %>% group_by(work_type, stroke) %>% count(stroke)
  df_8 <- df %>% group_by(Residence_type, stroke) %>% count(stroke)
  df_10 <- df %>% group_by(bmi, stroke, gender) %>% count(stroke)
  df_11 <- df %>% group_by(smoking_status, stroke) %>% count(stroke)
  
  
  
  
  ##CODE FOR VISUALIZATION BASE ON THE USER'S SELECTIONS.
  
  Disp <- reactive({
    
    ##VISUALIZTION OF AGE vs HYPERTENSION
    if (input$x_var == "age" & input$y_var == "hypertension"){
      ggplot(df_4,mapping = aes(x = age, y = n )) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "loess", span = 0.3) +
        labs(title = "Hypertension Event Distribution with Age", 
             subtitle = "Age Distribution", 
             x = "Age", 
             y = "Number of Observation") +
        theme_minimal()
    
    ##VISUALIZATION OF AGE vs BMI    
    }else if (input$x_var == "age" & input$y_var == "bmi"){  
      ggplot(df, mapping = aes(x = age, y = bmi, color = stroke)) +
        geom_point(alpha = 0.5) +
        labs(title = "Age Distribution by BMI",
             x = "Age", 
             y = "BMI") +
        theme_minimal()
    
    ##VISUALIZATION OF AGE vs GLUCOSE LEVEL  
    }else if (input$x_var == "age" & input$y_var == "avg_glucose_level"){  
        ggplot(df, mapping = aes(x = age, y = avg_glucose_level, color = stroke)) +
          geom_point(alpha=0.5) +
          labs(title = "Age Distribution by Average Glucose Level",
               x = "Age", 
               y = "Average Glucose Level") +
          theme_minimal()  
      
    ##VISALIZATION OF THE AGE DENSITY BASED ON GENDER     
    }else if (input$x_var == "age" & input$y_var == "density" & input$fillvar1 == "gender"){
      ggplot(df, aes(x = age, fill = gender)) +
        geom_density(alpha = 0.4) +
        geom_vline(aes(xintercept=mean(age))) +
        labs(title = "Age Density", x = "Age", y = "Density") +
        theme_classic()  
      
    ##VISALIZATION OF THE AGE DENSITY  
    }else if (input$x_var == "age" & input$y_var == "density"){
      ggplot(df, aes(x = age)) +
        geom_density(alpha = 0.4) +
        geom_vline(aes(xintercept=mean(age))) +
        labs(title = "Age Density", x = "Age", y = "Density") +
        theme_classic()
    
    ##VISAULIZATION OF AGE VS STROKE     
    }else if (input$x_var == "age" & input$y_var == "stroke"){
      ggplot(df_2,mapping = aes(x = age, y = n)) +
        geom_point(alpha=0.5) +
        geom_smooth(method = "loess", span = 0.3) +
        labs(title = "Stoke Event Distribution by Age",
             x = "Age", 
             y = "Number of Observations") +
        theme_minimal()  
      
    ##VISUALIZATION OF GENDER VS  STROKE
    }else if (input$x_var == "gender" & input$y_var == "stroke"){
      ggplot(df_1, mapping = aes(x = gender, y = n, fill = stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Percentage of Female Group vs. Male Group", 
             x = "Gender", 
             y = "Percentage") +
        scale_fill_brewer(palette = "Paired")
    
    ##VISUALIZATION OF HYPERTENSION VS STROKE   
    }else if (input$x_var == "hypertension" & input$y_var == "stroke"){
      ggplot(df_3, mapping = aes(x = hypertension, y = n, fill = stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Percentage of subjects with Hypertension vs. without Hypertension", 
             x = "Hypertension", 
             y = "Percentage") +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF HEART DISEASE VS STROKE   
    }else if (input$x_var == "heart_disease" & input$y_var == "stroke"){
      ggplot(df_5,mapping = aes(x = heart_disease, y = n, fill = stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Percentage of subjects with Heart Disease vs. without Heart Disease", 
             x = "Heart Disease", 
             y = "Percentage") +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF EVER MARRIED VS STROKE   
    }else if (input$x_var == "ever_married" & input$y_var == "stroke"){
      ggplot(df_6, mapping = aes(x = ever_married, y = n, fill = stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Married subjects vs. Unmarried subjects", 
             x = "Ever Married", 
             y = "Percentage") +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF EVER MARRIED VS AGE FILLED WITH STOKE    
    }else if (input$x_var == "ever_married" & input$y_var == "age" & input$fillvar4 == "stroke"){
      ggplot(df,mapping = aes(x = ever_married, y = age,fill = stroke)) +
        geom_boxplot() +
        labs(title = "Age Distribution - Married vs. Unmarried", 
             x = "Ever Married", 
             y = "Age") +
        theme_minimal() +
        scale_fill_brewer(palette = "Paired")
    
    ##VISUALIZATION OF EVER MARRIED VS AGE    
    }else if (input$x_var == "ever_married" & input$y_var == "age" ){
      ggplot(df,mapping = aes(x = ever_married, y = age)) +
        geom_boxplot() +
        labs(title = "Age Distribution - Married vs. Unmarried", 
             x = "Ever Married", 
             y = "Age") +
        theme_minimal() +
        scale_fill_brewer(palette = "Paired")
      
    ##VISUALIZATION OF WORK TYPE VS STROKE 
    }else if (input$x_var == "work_type" & input$y_var == "stroke"){
      ggplot(df_7,mapping = aes(x = work_type, y = n, fill = stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Which Work Type Has Higher Risk of Getting a Stroke?", 
             x = "Work Type", 
             y = "Percentage") + 
        theme_minimal() + 
        scale_fill_brewer(palette = "Paired")
      
    ##VISUALIZATION OF WORK TYPE VS AGE FILLED WITH STROKE 
    }else if (input$x_var == "work_type" & input$y_var == "age" & input$fillvar3 == "stroke"){
      ggplot(df, mapping = aes(x = work_type, y = age, fill = stroke)) +
        geom_boxplot() +
        labs(title = "Age Distribution of Work Type", 
             x = "Work Type", 
             y = "Age") +
        theme_minimal()
     
    ##VISUALIZATION OF WORK TYPE VS AGE   
    }else if (input$x_var == "work_type" & input$y_var == "age"){
      ggplot(df, mapping = aes(x = work_type, y = age)) +
        geom_boxplot() +
        labs(title = "Age Distribution of Work Type", 
             x = "Work Type", 
             y = "Age") +
        theme_minimal()
     
    ##VISUALIZATION OF  RESIDENCE TYPE WITH STROKE 
    }else if (input$x_var == "Residence_type" & input$y_var == "stroke"){
      ggplot(df_8,mapping = aes(x = Residence_type, y = n, fill = stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Does residence type have a great impact on risk of getting stroke?", 
             x = "Residence Type", 
             y = "Percentage") +
        theme_minimal() +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF  STROKE VS GLUCOSE LEVEL
    }else if (input$x_var == "stroke" & input$y_var == "avg_glucose_level"){
      ggplot(df,mapping = aes(x = stroke, y = avg_glucose_level, fill = stroke)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Boxplot of Average Glucose Level", 
             x = "Stroke", 
             y = "Average Glucose Level") +
        scale_fill_brewer(palette = "Paired")
    
    ##VISUALIZATION OF STROKE VS BMI FILLED WITH GENDER
    }else if (input$x_var == "stroke" & input$y_var == "bmi" & input$fillvar2 == "gender"){
      ggplot(df_10,mapping = aes(x = stroke, y = bmi, fill = stroke, color = gender)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Boxplot of BMI", 
             x = "Stroke", 
             y = "BMI") +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF STROKE VS BMI   
    }else if (input$x_var == "stroke" & input$y_var == "bmi"){
      ggplot(df_10,mapping = aes(x = stroke, y = bmi)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Boxplot of BMI", 
             x = "Stroke", 
             y = "BMI") +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF SMOKING STATUS VS STROKE 
    }else {
      ggplot(df_11,mapping = aes(x = smoking_status, y = n, fill = stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Does the smoking status impact the risk of stroke? ", 
             x = "Smoking Status", 
             y = "Percentage") +
        theme_minimal() +
        scale_fill_brewer(palette = "Paired")
    }
  })
  
  
  output$scatter_plot <- renderPlot({
    Disp()
  })
  

  ## STROKE PREDICTION
  
  Sams <-eventReactive(input$predictButton, {
    
    validate(
      need(input$age >= 0 && input$age <= 85, "Age must be between 0 and 85"),
      need(input$AGL >= 40 && input$AGL <= 300, "AGL must be between 40 and 300")
    )
      
      message <- paste("If your average blood sugar is", input$AGL)
      
      if (input$Hyp == "Yes"){
        message <- paste(message, "and you a have heart disease")
      }
      if (input$Htd == "Yes"){
        message <- paste(message, "and a high blood pressure")
      }
      if (input$Hyp == "No" & input$Htd == "No"){
        message <- paste(message, "and you don't have a heart disease or high blood pressure")
      }
      message <- paste(message, ", you have a", as.character(nod()), "chance of having a stroke at age", input$age)
  })
 
  
  ##FORMULA TO CALCULATE THE PROBABILTY OF STROKE
   nod <- eventReactive(input$predictButton, {
     round((exp((-7.488996) + (0.068920 * input$age) + (0.004121 * input$AGL) + 
           ifelse(input$Hyp == "Yes", 0.381396, 0) + ifelse(input$Htd == "Yes", 0.329972, 0))/
       (1+exp((-7.488996) + (0.068920 * input$age) + (0.004121 * input$AGL) + 
                ifelse(input$Hyp == "Yes", 0.381396, 0) + ifelse(input$Htd == "Yes", 0.329972, 0)))),2)
   })
 
 
   output$stroke <- renderText({
     Sams()
  })

}

shinyApp(ui = ui, server = server)
