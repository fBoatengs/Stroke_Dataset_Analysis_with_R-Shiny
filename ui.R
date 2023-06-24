ui <- fluidPage(
  theme = shinytheme("united"),
  
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background-color: #F2F3F4; /* Set your desired background color */
          font-family: 'Arial', sans-serif; /* Set your desired font family */
        }
      
        "
      )
    )
  ),
  
  titlePanel("Stroke Dataset Analysis"),
  
  tabsetPanel(
    
    ## DESCRIPTIVE STATISTICS 
    
    tabPanel(
      "Descriptive Statistics",
      fluidRow(
        column(width = 4,
          sliderInput("bmirng", "BMI",value = c(10, 100),min = 10, max = 100),
          sliderInput("agerng","Age",value = c(0, 90),min = 0,max = 90),
          sliderInput("glurng","Average Glucose Level",value = c(50, 300),min = 50,max = 300)
        ),
        
        column(width = 2,
          radioButtons("htd","Heart Disease",c("All", "Yes", "No")),
          radioButtons("marrg","Ever Married",c("All", "Yes", "No")),
          radioButtons("res","Residence Type",c("All", "Rural", "Urban"))
        ),
        
        column(width = 3,
          checkboxGroupInput("worktyp","Work Type",
                             choices = c("All","Children","Govt_job","Never_worked","Private","Self_employed"),selected = c("All")),
          radioButtons("gend","Gender",c("All", "Male", "Female"))),
        
        column(width = 3,
          checkboxGroupInput("smoke","Smoking Status",
                             choices = c("All","Formerly_smoked","Never_smoked","Smokes","Unknown"),selected = c("All")),
          radioButtons("hpy","Hypertension",c("All", "Yes", "No") )
        )
      ),
      #hr(),
      actionButton("goButton", "Update", icon = icon("refresh")),
      hr(),
      tableOutput("summary")
    ),
    
    
    ## DATA VISUALIZATION
    tabPanel("Visualizations",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_var", "Select X variable:",
                             c("Age","Ever.Married","Gender","Hypertension","Heart.Disease","Residence.Type",
                               "Smoking.Status","Stroke","Work.Type")),
                 selectInput("y_var", "Select Y variable:",c()),
                 conditionalPanel( condition = "input.x_var == 'Age' && input.y_var == 'Age.Density'",
                                   selectInput("fillvar1", "Select fill variable:", c("None", "Gender"))
                 ),
                 conditionalPanel( condition = "input.x_var == 'Stroke' && input.y_var == 'BMI'",
                                   selectInput("fillvar2", "Select fill variable:", c("None", "Gender"))
                 ),
                 conditionalPanel( condition = "input.x_var == 'Work.Type' && input.y_var == 'Age'",
                                   selectInput("fillvar3", "Select fill variable:", c("None", "Stroke"))
                 ),
                 conditionalPanel( condition = "input.x_var == 'Ever.Married' && input.y_var == 'Age'",
                                   selectInput("fillvar4", "Select fill variable:", c("None", "Stroke"))
                 )
               ),
               mainPanel(
                 verbatimTextOutput("ttest"),
                 plotOutput("plot")
               )
             )
    ),
     
    ##PREDICTION  
    tabPanel(
      "Stroke Prediction",
             sidebarLayout(
               sidebarPanel(
                 numericInput("age", "Enter your Age", value = 40, min = 0, max = 85),
                 numericInput("AGL", "What is your Avg Glucose Level?", value = 50, min = 40, max = 300),
                 selectInput("Hyp", "Do you have Hypertension?", choices = c("Yes", "No")),
                 selectInput("Htd", "Do you have Heart Disease?", choices = c("Yes", "No")),
                 actionButton("predictButton", "PREDICT", icon = icon("refresh"))
               ),
               mainPanel(
                 textOutput("stroke")
               )
             )
    )
    
  )
)
