server <- function(input, output, session) {
  
  #Reactivity is a key concept in R Shiny that enables dynamic behavior by automatically updating 
  #the application based on changes in inputs and other reactive elements.
  
  
  ##UPDATING THESE CHECK BOXES TO UNSELECT THE "All" OPTION WHEN THE USER CHECK OTHER ALTERNATE OPTIONS. 
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
  
  #eventReactive() is used when you want to create a reactive object that depends on one or more input
  #values and is updated whenever any of those input values change. It allows you to create a reactive expression
  #that you can use as a variable, which automatically updates whenever the inputs it depends on change.
  
  Data <- eventReactive(input$goButton, {
    
    data <- df
    
    if (input$htd != "All") {
      data <- data %>% filter(Heart.Disease == input$htd)
    }
    if (input$marrg != "All") {
      data <- data %>% filter(Ever.Married == input$marrg)
    }
    if (input$res != "All") {
      data <- data %>% filter(Residence.Type == input$res)
    }
    if (input$gend != "All") {
      data <- data %>% filter(Gender == input$gend)
    }
    if (input$hpy != "All") {
      data <- data %>% filter(Hypertension == input$hpy)
    }
    if ("All" %in% input$smoke) {
      data
    } else {
      data <- data %>% filter(Smoking.Status %in% input$smoke)
    }
    if ("All" %in% input$worktyp) {
      data
    } else {
      data <- data %>% filter(Work.Type %in% input$worktyp)
    }
    data <- data %>% filter(BMI >= input$bmirng[1] & BMI <= input$bmirng[2])
    data <- data %>% filter(Age >= input$agerng[1] & Age <= input$agerng[2])
    data <- data %>% filter(Avg.Glucose.Level >= input$glurng[1] & Avg.Glucose.Level <= input$glurng[2])
    return((data%>% group_by(Stroke) %>%summarize(count = n())) %>% 
             rename("Stroke" = "Stroke", "Sample Size" = "count"))
  })
  
  
  ##RENDERING THE FILTERED DATA
  output$summary <- renderTable({
    Data()
  })
  
  
  ##VISUALIZATION BASE ON THE USER'S PREFERENCE. 
  
  ##UPDATING THE "y_var" OPTIONS BASED ON THE "x_var" OPTION THE USER SELECTS
  
  #observeEvent() is used when you want to perform reactive operations in response to a specific event, such as 
  #a button click, a selection change, or any other user interaction that triggers an event.
  
  observeEvent(input$x_var, {
    if (input$x_var == "Age") {
      updateSelectInput(session, inputId = "y_var",choices = c("Age.Density","BMI","Avg.Glucose.Level","Hypertension","Stroke"))
      
    } else if (input$x_var == "Ever.Married") {
      updateSelectInput(session, inputId = "y_var",choices = c("Age", "Stroke"))
      
    } else if (input$x_var == "Gender") {
      updateSelectInput(session, inputId = "y_var",choices = c("Stroke"))
      
    } else if (input$x_var == "Hypertension") {
      updateSelectInput(session, inputId = "y_var", choices = c("Stroke"))
      
    } else if (input$x_var == "Heart.Disease") {
      updateSelectInput(session, input$y_var, choices = c("Stroke"))
      
    } else if (input$x_var == "Residence.Type") {
      updateSelectInput(session, inputId = "y_var", choices = c("Stroke"))
      
    } else if (input$x_var == "Smoking.Status") {
      updateSelectInput(session, inputId = "y_var", choices = c("Stroke"))
      
    } else if (input$x_var == "Stroke") {
      updateSelectInput(session, inputId = "y_var", choices = c("BMI","Avg.Glucose.Level"))
      
    } else {
      updateSelectInput(session, inputId = "y_var", choices = c("Age", "Stroke"))
    }})
  
  ##PERFORMING STATISTICAL TEST BASED ON THE INPUT THE USER SELECTS AND DISPLAYS THE P-VALUE
  output$ttest <- renderText({
    
    A <- c("Age","BMI","Avg.Glucose.Level")
    B <- c("Ever.Married","Stroke","Gender","Hypertension","Heart.Disease",
           "Residence.Type","Smoking.Status","Work.Type")
    
    if(input$x_var %in% A && input$y_var%in% A) {
      p_val1 <- t.test(df[[input$x_var]], df[[input$y_var]])$p.value
      paste("The p-value of the T-Test is", p_val1)
      
    } else if (input$x_var %in% B && input$y_var %in% B) {
      p_val2 <- chisq.test(df[[input$x_var]], df[[input$y_var]])$p.value
      paste("The p-value of the Chi-Squared Test is", round(p_val2, 4))
      
    } else if (input$x_var %in% A && input$y_var %in% B) {
      p_val3 <- summary(aov(df[[input$x_var]] ~ df[[input$y_var]]))[[1]]$"Pr(>F)"[1]
      paste("The p-value of the ANOVA test is", round(p_val3, 4)) 
      
    } else if (input$x_var %in% B && input$y_var %in% A) {
      p_val4 <- summary(aov(df[[input$y_var]] ~ df[[input$x_var]]))[[1]]$"Pr(>F)"[1]
      paste("The p-value of the ANOVA test is", round(p_val4, 4))  
      
    } else {
      p_val5 <- t.test(df$Age, df$Age)$p.value
      paste("The p-value of the T-Test is", p_val5)
    }
  })
  
  
  ##GROUPING DATA FOR VISUALIZATION 
  df_4 <- df %>% group_by(Age, Hypertension) %>% filter(Hypertension == "Yes") %>%count(Hypertension)
  df_1 <- df %>% group_by(Gender, Stroke) %>% count(Stroke)
  df_2 <- df %>% group_by(Age, Stroke) %>% filter(Stroke == "Yes")%>% count(Stroke)
  df_3 <- df %>% group_by(Hypertension, Stroke) %>% count(Stroke)
  df_5 <- df %>% group_by(Heart.Disease, Stroke) %>% count(Stroke)
  df_6 <- df %>% group_by(Ever.Married, Stroke) %>% count(Stroke)
  df_7 <- df %>% group_by(Work.Type, Stroke) %>% count(Stroke)
  df_8 <- df %>% group_by(Residence.Type, Stroke) %>% count(Stroke)
  df_10 <- df %>% group_by(BMI, Stroke, Gender) %>% count(Stroke)
  df_11 <- df %>% group_by(Smoking.Status, Stroke) %>% count(Stroke)
  
  
  
  
  ##CODE FOR VISUALIZATION BASE ON THE USER'S SELECTIONS.
  
  #Reactivity is a key concept in R Shiny that enables dynamic behavior by automatically updating 
  #the application based on changes in inputs and other reactive elements.
  
  Disp <- reactive({
    
    ##VISUALIZTION OF AGE vs HYPERTENSION
    if (input$x_var == "Age" & input$y_var == "Hypertension"){
      ggplot(df_4,mapping = aes(x = Age, y = n )) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "loess", span = 0.3) +
        labs(title = "Hypertension Event Distribution with Age", 
             subtitle = "Age Distribution", 
             x = "Age", 
             y = "Number of Observation") +
        theme_minimal()
    
    ##VISUALIZATION OF AGE vs BMI    
    }else if (input$x_var == "Age" & input$y_var == "BMI"){  
      ggplot(df, mapping = aes(x = Age, y = BMI, color = Stroke)) +
        geom_point(alpha = 0.5) +
        labs(title = "Age Distribution by BMI",
             x = "Age", 
             y = "BMI") +
        theme_minimal()
    
    ##VISUALIZATION OF AGE vs GLUCOSE LEVEL  
    }else if (input$x_var == "Age" & input$y_var == "Avg.Glucose.Level"){  
        ggplot(df, mapping = aes(x = Age, y = Avg.Glucose.Level, color = Stroke)) +
          geom_point(alpha=0.5) +
          labs(title = "Age Distribution by Average Glucose Level",
               x = "Age", 
               y = "Average Glucose Level") +
          theme_minimal()  
      
    ##VISALIZATION OF THE AGE DENSITY BASED ON GENDER     
    }else if (input$x_var == "Age" & input$y_var == "Age.Density" & input$fillvar1 == "Gender"){
      ggplot(df, aes(x = Age, fill = Gender)) +
        geom_density(alpha = 0.4) +
        geom_vline(aes(xintercept=mean(Age))) +
        labs(title = "Age Density", x = "Age", y = "Density") +
        theme_classic()  
      
    ##VISALIZATION OF THE AGE DENSITY  
    }else if (input$x_var == "Age" & input$y_var == "Age.Density"){
      ggplot(df, aes(x = Age)) +
        geom_density(alpha = 0.4) +
        geom_vline(aes(xintercept=mean(Age))) +
        labs(title = "Age Density", x = "Age", y = "Density") +
        theme_classic()
    
    ##VISAULIZATION OF AGE VS STROKE     
    }else if (input$x_var == "Age" & input$y_var == "Stroke"){
      ggplot(df_2,mapping = aes(x = Age, y = n)) +
        geom_point(alpha=0.5) +
        geom_smooth(method = "loess", span = 0.3) +
        labs(title = "Stoke Event Distribution by Age",
             x = "Age", 
             y = "Number of Observations") +
        theme_minimal()  
      
    ##VISUALIZATION OF GENDER VS  STROKE
    }else if (input$x_var == "Gender" & input$y_var == "Stroke"){
      ggplot(df_1, mapping = aes(x = Gender, y = n, fill = Stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Percentage of Female Group vs. Male Group", 
             x = "Gender", 
             y = "Percentage") +
        scale_fill_brewer(palette = "Paired")
    
    ##VISUALIZATION OF HYPERTENSION VS STROKE   
    }else if (input$x_var == "Hypertension" & input$y_var == "Stroke"){
      ggplot(df_3, mapping = aes(x = Hypertension, y = n, fill = Stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Percentage of subjects with Hypertension vs. without Hypertension", 
             x = "Hypertension", 
             y = "Percentage") +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF HEART DISEASE VS STROKE   
    }else if (input$x_var == "Heart.Disease" & input$y_var == "Stroke"){
      ggplot(df_5,mapping = aes(x = Heart.Disease, y = n, fill = Stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Percentage of subjects with Heart Disease vs. without Heart Disease", 
             x = "Heart Disease", 
             y = "Percentage") +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF EVER MARRIED VS STROKE   
    }else if (input$x_var == "Ever.Married" & input$y_var == "Stroke"){
      ggplot(df_6, mapping = aes(x = Ever.Married, y = n, fill = Stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Married subjects vs. Unmarried subjects", 
             x = "Ever Married", 
             y = "Percentage") +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF EVER MARRIED VS AGE FILLED WITH STOKE    
    }else if (input$x_var == "Ever.Married" & input$y_var == "Age" & input$fillvar4 == "Stroke"){
      ggplot(df,mapping = aes(x = Ever.Married, y = Age,fill = Stroke)) +
        geom_boxplot() +
        labs(title = "Age Distribution - Married vs. Unmarried", 
             x = "Ever Married", 
             y = "Age") +
        theme_minimal() +
        scale_fill_brewer(palette = "Paired")
    
    ##VISUALIZATION OF EVER MARRIED VS AGE    
    }else if (input$x_var == "Ever.Married" & input$y_var == "Age" ){
      ggplot(df,mapping = aes(x = Ever.Married, y = Age)) +
        geom_boxplot() +
        labs(title = "Age Distribution - Married vs. Unmarried", 
             x = "Ever Married", 
             y = "Age") +
        theme_minimal() +
        scale_fill_brewer(palette = "Paired")
      
    ##VISUALIZATION OF WORK TYPE VS STROKE 
    }else if (input$x_var == "Work.Type" & input$y_var == "Stroke"){
      ggplot(df_7,mapping = aes(x = Work.Type, y = n, fill = Stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Which Work Type Has Higher Risk of Getting a Stroke?", 
             x = "Work Type", 
             y = "Percentage") + 
        theme_minimal() + 
        scale_fill_brewer(palette = "Paired")
      
    ##VISUALIZATION OF WORK TYPE VS AGE FILLED WITH STROKE 
    }else if (input$x_var == "Work.Type" & input$y_var == "Age" & input$fillvar3 == "Stroke"){
      ggplot(df, mapping = aes(x = Work.Type, y = Age, fill = Stroke)) +
        geom_boxplot() +
        labs(title = "Age Distribution of Work Type", 
             x = "Work Type", 
             y = "Age") +
        theme_minimal()
     
    ##VISUALIZATION OF WORK TYPE VS AGE   
    }else if (input$x_var == "Work.Type" & input$y_var == "Age"){
      ggplot(df, mapping = aes(x = Work.Type, y = Age)) +
        geom_boxplot() +
        labs(title = "Age Distribution of Work Type", 
             x = "Work Type", 
             y = "Age") +
        theme_minimal()
     
    ##VISUALIZATION OF  RESIDENCE TYPE WITH STROKE 
    }else if (input$x_var == "Residence.Type" & input$y_var == "Stroke"){
      ggplot(df_8,mapping = aes(x = Residence.Type, y = n, fill = Stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Does residence type have a great impact on risk of getting stroke?", 
             x = "Residence Type", 
             y = "Percentage") +
        theme_minimal() +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF  STROKE VS GLUCOSE LEVEL
    }else if (input$x_var == "Stroke" & input$y_var == "Avg.Glucose.Level"){
      ggplot(df,mapping = aes(x = Stroke, y = Avg.Glucose.Level, fill = Stroke)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Boxplot of Average Glucose Level", 
             x = "Stroke", 
             y = "Average Glucose Level") +
        scale_fill_brewer(palette = "Paired")
    
    ##VISUALIZATION OF STROKE VS BMI FILLED WITH GENDER
    }else if (input$x_var == "Stroke" & input$y_var == "BMI" & input$fillvar2 == "Gender"){
      ggplot(df_10,mapping = aes(x = Stroke, y = BMI, fill = Stroke, color = Gender)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Boxplot of BMI", 
             x = "Stroke", 
             y = "BMI") +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF STROKE VS BMI   
    }else if (input$x_var == "Stroke" & input$y_var == "BMI"){
      ggplot(df_10,mapping = aes(x = Stroke, y = BMI)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Boxplot of BMI", 
             x = "Stroke", 
             y = "BMI") +
        scale_fill_brewer(palette = "Paired")
     
    ##VISUALIZATION OF SMOKING STATUS VS STROKE 
    }else {
      ggplot(df_11,mapping = aes(x = Smoking.Status, y = n, fill = Stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Does the smoking status impact the risk of stroke? ", 
             x = "Smoking Status", 
             y = "Percentage") +
        theme_minimal() +
        scale_fill_brewer(palette = "Paired")
    }
  })
  
  
  output$plot <- renderPlot({
    Disp()
  })
  

  ## STROKE PREDICTION
  
  ##FORMULA TO CALCULATE THE PROBABILTY OF STROKE
   nod <- eventReactive(input$predictButton, {
     
     validate(
       need(input$age >= 0 && input$age <= 85, "Age must be between 0 and 85"),
       need(input$AGL >= 40 && input$AGL <= 300, "AGL must be between 40 and 300")
     )
     
     round((exp((-7.488996) + (0.068920 * input$age) + (0.004121 * input$AGL) + 
           ifelse(input$Hyp == "Yes", 0.381396, 0) + ifelse(input$Htd == "Yes", 0.329972, 0))/
       (1+exp((-7.488996) + (0.068920 * input$age) + (0.004121 * input$AGL) + 
                ifelse(input$Hyp == "Yes", 0.381396, 0) + ifelse(input$Htd == "Yes", 0.329972, 0)))),2)
   })
 
 
   output$stroke <- renderText({
     paste("The probability of experiencing a stroke is", nod())
  })

}
