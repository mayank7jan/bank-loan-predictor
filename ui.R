# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "blue",
  
  # Application title
  dashboardHeader(title = "Success of Tele-Marketing Call", titleWidth = 350),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      style = "font-size: 130%",
      menuItem(
        "Prediction Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem(
        "Decision Tree Summary",
        icon = icon("tree"),
        tabName = "dt_widget",
        badgeColor = "green"
      ),
      menuItem(
        "Logistic Regression Summary",
        icon = icon("chart-line"),
        tabName = "lr_widget",
        badgeColor = "green"
      ),
      menuItem(
        "KNN Summary",
        icon = icon("bezier-curve"),
        tabName = "knn_widget",
        badgeColor = "green"
      ),
      menuItem(
        "Attribute Examples",
        icon = icon("user-check"),
        tabName = "data_example",
        badgeColor = "green"
      ),
      menuItem(
        "Attribute Definitions",
        icon = icon("glasses"),
        tabName = "data_define",
        badgeColor = "green"
      ),
      menuItem(
        "Glossary",
        icon = icon("book-reader"),
        tabName = "glossary",
        badgeColor = "green"
      )
      
    )
    
  ),
  
  dashboardBody(fluidPage(
    tabItems(
      # Prediction Body UI
      tabItem(
        tabName = "dashboard",
        fluidRow(
          column(width = 10,
                 h2(tags$b("Make predictions for new customers:"))),
          column(
            width = 2,
            shiny::actionButton(
              inputId = 'git-hub',
              class = "pull-right",
              style = "background: #f0f3ff; border-radius: 50%; box-shadow: 6px 6px 10px -1px rgb(0 0 0 / 15%), -6px -6px 10px -1px rgb(255 255 255 / 70%);",
              label = NULL,
              icon = icon("github"),
              onclick ="window.open('https://github.com/mayank7jan/bank-loan-predictor')"
            )
          )
        ),
        
        
        fluidRow(
          valueBoxOutput("dt_cust_pred_ui"),
          valueBoxOutput("lr_cust_pred_ui"),
          valueBoxOutput("knn_cust_pred_ui")
        ),
        
        fluidRow(
          style = "font-size: 130%",
          box(
            width = 4,
            collapsible = TRUE,
            title = tags$b("DECISION TREE", style = "font-size: 150%"),
            tags$hr(),
            numericInput(
              inputId = "dt_nr_employed",
              label = "Number of employees:",
              value = 5009,
              step = 10
            ),
            radioButtons(
              inputId = "dt_poutcome",
              label = "Outcome of previous marketing campaign:",
              choices = list("failure", "nonexistent", "success"),
              selected = "success"
            ),
            hr(),
            actionButton(
              inputId = "update_dt_button",
              label = "Predict",
              icon = icon("stopwatch"),
              width = '100%',
              style = 'font-size:125%; font-variant: small-caps; font-weight: bold;'
            )
            # hr()
            # actionButton(inputId = "copy_button", label = "Make all values similar", icon = icon("copy"),
            #              width = '100%',
            #              style = 'font-size:125%; font-variant: small-caps; font-weight: bold;')
          ),
          
          box(
            width = 4,
            collapsible = TRUE,
            title = tags$b("LOGISTIC REGRESSION", style = "font-size: 150%"),
            tags$hr(),
            numericInput(
              inputId = "lr_nr_employed",
              label = "Number of employees:",
              value = 5009,
              step = 10
            ),
            radioButtons(
              inputId = "lr_poutcome",
              label = "Outcome of previous marketing campaign:",
              choices = list("failure", "nonexistent", "success"),
              selected = "success"
            ),
            numericInput(
              inputId = "lr_euribor",
              label = "Euribor 3 month rate:",
              value = 0.635,
              step = 0.1
            ),
            numericInput(
              inputId = "lr_cons_price_idx",
              label = "Consumer price index:",
              value = 93.37,
              step = 5
            ),
            numericInput(
              inputId = "lr_cons_conf_idx",
              label = "Consumer confidence index:",
              value = -34.8,
              step = 0.2
            ),
            numericInput(
              inputId = "lr_emp_var_rate",
              label = "Employment variation rate:",
              value = -1.8,
              step = 0.1
            ),
            selectInput(
              inputId = "lr_month",
              label = "Last contact month of year:",
              choices = list(
                "mar",
                "apr",
                "may",
                "jun",
                "jul",
                "aug",
                "sep",
                "oct",
                "nov",
                "dec"
              ),
              selected = "mar"
            ),
            sliderInput(
              inputId = "lr_previous",
              label = "Number of contacts performed before this campagin:",
              min = 0,
              max = 7,
              step = 1,
              value = 1
            ),
            numericInput(
              inputId = "lr_age",
              label = "Age of client:",
              value = 30
            ),
            radioButtons(
              inputId = "lr_contact",
              label = "Contact communication type:",
              choices = list("telephone", "cellular"),
              selected = "cellular"
            ),
            selectInput(
              inputId = "lr_job",
              label = "Type of Job:",
              choices = list(
                'management',
                'services',
                'admin.',
                'blue-collar',
                'technician',
                'retired',
                'student',
                'unemployed',
                'housemaid',
                'self-employed',
                'unknown',
                'entrepreneur'
              ),
              selected = "technician"
            ),
            radioButtons(
              inputId = "lr_dcredit",
              label = "Has credit in default?",
              choices = list("no", "yes", "unknown"),
              selected = "no"
            )
            
          ),
          
          box(
            width = 4,
            collapsible = TRUE,
            title = tags$b("KNN CLASSIFICATION", style = "font-size: 150%"),
            tags$hr(),
            numericInput(
              inputId = "knn_nr_employed",
              label = "Number of employees:",
              value = 5009,
              step = 10
            ),
            radioButtons(
              inputId = "knn_poutcome",
              label = "Outcome of previous marketing campaign:",
              choices = list("failure", "nonexistent", "success"),
              selected = "success"
            ),
            numericInput(
              inputId = "knn_euribor",
              label = "Euribor 3 month rate:",
              value = 0.635,
              step = 0.1
            ),
            numericInput(
              inputId = "knn_cons_price_idx",
              label = "Consumer price index:",
              value = 93.37,
              step = 5
            ),
            numericInput(
              inputId = "knn_cons_conf_idx",
              label = "Consumer confidence index:",
              value = -40,
              step = 0.2
            ),
            numericInput(
              inputId = "knn_emp_var_rate",
              label = "Employment variation rate:",
              value = -1.8,
              step = 0.1
            ),
            selectInput(
              inputId = "knn_month",
              label = "Last contact month of year:",
              choices = list(
                "mar",
                "apr",
                "may",
                "jun",
                "jul",
                "aug",
                "sep",
                "oct",
                "nov",
                "dec"
              ),
              selected = "apr"
            ),
            sliderInput(
              inputId = "knn_previous",
              label = "Number of contacts performed before this campagin:",
              min = 0,
              max = 7,
              step = 1,
              value = 1
            ),
            numericInput(
              inputId = "knn_age",
              label = "Age of client:",
              value = 30
            ),
            radioButtons(
              inputId = "knn_contact",
              label = "Contact communication type:",
              choices = list("telephone", "cellular"),
              selected = "cellular"
            ),
            selectInput(
              inputId = "knn_job",
              label = "Type of Job:",
              choices = list(
                'management',
                'services',
                'admin.',
                'blue-collar',
                'technician',
                'retired',
                'student',
                'unemployed',
                'housemaid',
                'self-employed',
                'unknown',
                'entrepreneur'
              ),
              selected = "technician"
            ),
            radioButtons(
              inputId = "knn_dcredit",
              label = "Has credit in default?",
              choices = list("no", "yes", "unknown"),
              selected = "no"
            )
          )
          
        )
      ),
      
      # Decision Tree Body UI
      tabItem(tabName = "dt_widget",
              h2(tags$b(
                "Decision Tree Summary"
              )),
              
              tabBox(
                width = 12,
                #id = "dt_fig",
                tabPanel(
                  title = tags$b("Decision Tree Structure") ,
                  solidHeader = TRUE,
                  withSpinner(plotOutput("Tree_Fig"), type = 6),
                  height = "auto",
                  width = "auto"
                  
                ),
                tabPanel("Accuracy Metrics",
                         fluidRow(
                           column(
                             width = 7,
                             box(
                               title = tags$b("ROC Curve"),
                               solidHeader = TRUE,
                               withSpinner(plotOutput("Tree_roc"), type = 6),
                               height = "auto",
                               width = "auto"
                             )
                           ),
                           column(
                             width = 5,
                             box(
                               title = tags$b("Accuracy of the Model - Training Data"),
                               solidHeader = TRUE,
                               withSpinner(verbatimTextOutput("dt_acc_train"), type = 1),
                               height = "auto",
                               width = "auto"
                             ),
                             box(
                               title = tags$b("Accuracy of the Model - Testing Data"),
                               solidHeader = TRUE,
                               withSpinner(verbatimTextOutput("dt_acc_test"), type = 1),
                               height = "auto",
                               width = "auto"
                             )
                           )
                         ))
              )),
      
      tabItem(tabName = "lr_widget",
              h2(
                tags$b("Logistic Regression Accuracy Metrics")
              ),
              
              tabBox(
                width = 12,
                tabPanel("Accuracy Metrics",
                         fluidRow(
                           column(
                             width = 7,
                             box(
                               title = tags$b("ROC Curve"),
                               solidHeader = TRUE,
                               withSpinner(plotOutput("lr_roc"), type = 6),
                               height = "auto",
                               width = "auto"
                             )
                           ),
                           column(
                             width = 5,
                             box(
                               title = tags$b("Accuracy of the Model - Training Data"),
                               solidHeader = TRUE,
                               withSpinner(verbatimTextOutput("lr_acc_train"), type = 1),
                               height = "auto",
                               width = "auto"
                             ),
                             box(
                               title = tags$b("Accuracy of the Model - Testing Data"),
                               solidHeader = TRUE,
                               withSpinner(verbatimTextOutput("lr_acc_test"), type = 1),
                               height = "auto",
                               width = "auto"
                             )
                           )
                         ))
              )),
      
      tabItem(tabName = "knn_widget",
              h2(
                tags$b("KNN Classification Accuracy Metrics")
              ),
              
              tabBox(
                width = 12,
                tabPanel("Accuracy Metrics",
                         fluidRow(
                           column(
                             width = 7,
                             box(
                               title = tags$b("ROC Curve"),
                               solidHeader = TRUE,
                               withSpinner(plotOutput("knn_roc"), type = 6),
                               height = "auto",
                               width = "auto"
                             )
                           ),
                           column(
                             width = 5,
                             box(
                               title = tags$b("Accuracy of the Model - Training Data"),
                               solidHeader = TRUE,
                               withSpinner(verbatimTextOutput("knn_acc_train"), type = 1),
                               height = "auto",
                               width = "auto"
                             ),
                             box(
                               title = tags$b("Accuracy of the Model - Testing Data"),
                               solidHeader = TRUE,
                               withSpinner(verbatimTextOutput("knn_acc_test"), type = 1),
                               height = "auto",
                               width = "auto"
                             )
                           )
                         ))
              )),
      
      tabItem(
        tabName = "data_define",
        h2(tags$b("Attribute Description")),
        box(
          width = 12,
          style = "font-size: 150%",
          tags$b("Age: "),
          br(),
          "Age of the cutomer",
          br(),
          br(),
          tags$b("Job: "),
          br(),
          (
            "Type of job - admin., bluecollar, entrepreneur, housemaid, management, retired, selfemployed,
                                 services, student, technician, unemployed, unknown"
          ),
          br(),
          br(),
          tags$b("Martial: "),
          br(),
          ("Martial status - divorced, married, single, unknown"),
          br(),
          br(),
          tags$b("Education: "),
          br(),
          (
            " Maximum level of education - basic.4y , basic.6y , basic.9y , high.school ,
                                                                illiterate , professional.course , university.degree , unknown"
          ),
          br(),
          br(),
          tags$b("Default Credit: "),
          br(),
          ("Has credit in default? - no, yes, unknown"),
          br(),
          br(),
          tags$b("Contact: "),
          br(),
          ("Contact communication type - cellular, telephone"),
          br(),
          br(),
          tags$b("Month: "),
          br(),
          (
            "Last contact month of the year - jan, feb, mar, apr, may, june, july, aug, sep, oct,
                                                          nov, dec"
          ),
          br(),
          br(),
          tags$b("Day of Week: "),
          br(),
          ("Last contact day of the week - mon, tue, wed, thu, fri"),
          br(),
          br(),
          tags$b("Campaign: "),
          br(),
          (
            "Number of contacts performed during this campaign and for this client"
          ),
          br(),
          br(),
          tags$b("Previous: "),
          br(),
          (
            "Number of contacts performed before this campaign and for this client"
          ),
          br(),
          br(),
          tags$b("Poutcome: "),
          br(),
          (
            "Outcome of the previous marketing campaign - failure, nonexistent, success"
          ),
          br(),
          br(),
          tags$b("Emp var rate: "),
          br(),
          ("Employmet variation rate - quarterly indicator"),
          br(),
          br(),
          tags$b("Cons price index: "),
          br(),
          ("Consumer Price Index - monthly indicator"),
          br(),
          br(),
          tags$b("euribor3m: "),
          br(),
          ("Euribor 3 month rate - daily indicator"),
          br(),
          br(),
          tags$b("nr_employed: "),
          br(),
          ("Number of employees - quarterly indicator"),
          br()
        )
        
        
      ),
      
      tabItem(
        tabName = "glossary",
        h2(tags$b("Accuracy metrices term descriptions")),
        box(
          width = 12,
          style = "font-size: 150%",
          tags$b("Precision: "),
          br(),
          "Precision tells us about when",
          tags$em("it predicts Yes, how often is it correct."),
          br(),
          br(),
          tags$b("Recall: "),
          br(),
          "Recall gives us an idea about when",
          tags$em("it is actually Yes, how often does it predict Yes."),
          br(),
          br(),
          tags$b("Sensitivity: "),
          br(),
          "Ability of the test to correctly identify the",
          tags$em("true positive rate."),
          br(),
          br(),
          tags$b("Specificity: "),
          br(),
          "Ability of the test to correctly identify the",
          tags$em("true negative rate."),
          br()
          
        )
      ),
      
      tabItem(tabName = "data_example",
              h2(tags$b(
                "Example datasets for reference"
              )),
              
              tabBox(
                width = 12,
                
                tabPanel(
                  title = tags$b("Ideal Customers") ,
                  solidHeader = TRUE,
                  withSpinner(DT::dataTableOutput("best_10_lr"), type = 1),
                  height = "auto",
                  width = "auto"
                  
                ),
                tabPanel(
                  title = tags$b("Non-Ideal Customers") ,
                  solidHeader = TRUE,
                  withSpinner(DT::dataTableOutput("worst_10_lr"), type = 1),
                  height = "auto",
                  width = "auto"
                  
                )
              ))
      
      
    )
  ))
  
)
